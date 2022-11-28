package qut.pm.spm.playout;

import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.deckfour.xes.model.XLog;

import qut.pm.spm.AcceptingStochasticNet;
import qut.pm.spm.TraceFreq;

public class CachingPlayoutGenerator implements PlayoutGenerator{

	private static Logger LOGGER = LogManager.getLogger();
	
	// TODO probably redundant
	private static class Key{
		public AcceptingStochasticNet net;
		public Long targetSize;
			
		public Key(AcceptingStochasticNet net, Long targetSize) {
			super();
			this.net = net;
			this.targetSize = targetSize;
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((net == null) ? 0 : net.hashCode());
			result = prime * result + ((targetSize == null) ? 0 : targetSize.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Key other = (Key) obj;
			if (net == null) {
				if (other.net != null)
					return false;
			} else if (!net.equals(other.net))
				return false;
			if (targetSize == null) {
				if (other.targetSize != null)
					return false;
			} else if (!targetSize.equals(other.targetSize))
				return false;
			return true;
		}
				
	}
	
	private PlayoutGenerator generator;
	private Map<Key,XLog> xlogCache = Collections.synchronizedMap( new WeakHashMap<>() );
	private Map<Key,TraceFreq> tfCache = Collections.synchronizedMap( new WeakHashMap<>() );
	
	public CachingPlayoutGenerator(long targetSize) {
		generator = new StochasticPlayoutGenerator(targetSize);
	}
	
	@Override
	public XLog buildPlayoutLog(AcceptingStochasticNet net, long targetSize) {
		LOGGER.debug("Checking cached playout log");
		Key key = new Key(net,targetSize);
		XLog result = xlogCache.get( key );
		if (result == null) {
			result = generator.buildPlayoutLog(net);
			xlogCache.put(key,result);
		}else {
			LOGGER.debug("Using cached playout log");
		}
		return result;
	}

	@Override
	public XLog buildPlayoutLog(AcceptingStochasticNet net) {
		return buildPlayoutLog(net,getTargetSize());
	}

	
	@Override
	public TraceFreq buildPlayoutTraceFreq(AcceptingStochasticNet net, long targetSize) {
		LOGGER.debug("Checking cached playout log");
		Key key = new Key(net,targetSize);
		TraceFreq result = tfCache.get( key );
		if (result == null) {
			result = generator.buildPlayoutTraceFreq(net,targetSize);
			tfCache.put(key,result);
		}else {
			LOGGER.debug("Using cached playout log");
		}
		return result;
	}
	
	@Override
	public TraceFreq buildPlayoutTraceFreq(AcceptingStochasticNet net) {
		return buildPlayoutTraceFreq(net,getTargetSize());
	}

	
	@Override
	public long getTargetSize() {
		return generator.getTargetSize();
	}

	@Override
	public TraceFreq scaleTo(TraceFreq tf, int newTargetSize) {
		return generator.scaleTo(tf,newTargetSize);
	}
	
}
