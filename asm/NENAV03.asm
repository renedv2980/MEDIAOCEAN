*          DATA SET NENAV03    AT LEVEL 003 AS OF 03/16/18                      
*PHASE T31803A                                                                  
NENAV03  TITLE '- Network Navigator - DDLINK interface'                         
NENAV03  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NN03**                                                       
                                                                                
         L     RE,4(RD)            RE=start of my chain                         
         L     RF,4(RE)            RF=start of caller's chain                   
         L     R2,24(RF)           R2=A(MONITOR's parameter list)               
                                                                                
         LA    R1,72(RF)           R1=New start of my chain                     
         MVC   0(72,R1),0(RE)      Copy chain to new location                   
         ST    R1,8(RF)            Chain caller to me                           
         LA    R9,72(R1)           Point to my working storage                  
         USING WORKD,R9            R9=A(Global working storage)                 
         LAY   RD,WORKD+WORKLON    Point to next in chain                       
         ST    RD,8(R1)            Chain me to next                             
         ST    R1,4(RD)            Chain next to me                             
                                                                                
         LAY   R8,LINKW                                                         
         USING LINKW,R8            R8=A(extended storage)                       
         LAY   RF,WORKD+WORKLON                                                 
         ST    RF,8(R1)            Set forward link                             
         LA    R0,WORKD                                                         
         SR    RF,R0               RF=length of work area                       
         LR    R1,RF                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear working storage                        
                                                                                
         MVC   ATIOB,0(R2)         Extract MONITOR's parameters                 
         MVC   ATWA,4(R2)                                                       
         MVC   ATIA,12(R2)                                                      
         MVC   ACOMFACS,16(R2)                                                  
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         USING FALSCRD,NAVINPH                                                  
         USING RUNPARMD,DDRUNP                                                  
         USING RUNFACSD,DDRUNF                                                  
         USING LP_D,DDLINKC                                                     
         USING WRKIOD,DDWRKIOC                                                  
         USING FALINKD,FABLK                                                    
                                                                                
         LAY   R0,SVSECD                                                        
         ST    R0,LP_ASECD         Set A(SECD) in LP_D                          
         LAY   R0,LP_D                                                          
         ST    R0,ALP              Set A(LP_D) in WORKD                         
         MVC   LP_AGYB,0(R2)       Set agency binary value                      
         MVC   RSVRSAVE,ATIA       Set A(server work area)                      
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         Set A(WRKIO control block)                   
         MVC   RCOMFACS,ACOMFACS                                                
         L     RF,ACOMFACS         Set A(DDLINK) from COMFACS                   
         MVC   RRUNIT,CRUNIT-COMFACSD(RF)                                       
         LA    R0,FACLIST                                                       
         ST    R0,RSYSFACS         Set A(facilities list)                       
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       Set A(RUNFACS) in RUNPARMS                   
         MVC   LP_ATWA,ATWA        Set A(TWA)                                   
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP                              
         MVC   LP_USRID,TWAUSRID   Set user-id                                  
         MVC   LP_AGY,TWAAGY       Set agency alpha                             
         MVC   LP_ACCS,TWAACCS     Set limit access bytes                       
         LAY   R0,SVDDLINK                                                      
         ST    R0,LP_ASAVE         Set A(DDLINK save area)                      
         LHI   R0,SVOVER-TWAD                                                   
         STCM  R0,3,LP_DSAVE       Set disp. to overlay save in TWA             
         MVC   LP_ATIOB,ATIOB      Set A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    Set A(COMFACS)                               
         LAY   R0,DDLINKW                                                       
         ST    R0,LP_AWORK         Set A(DDLINK work area)                      
         LAY   R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         Set A(dummy RUNPARMS)                        
         LAY   R0,FABLK                                                         
         ST    R0,LP_AFBLK         Set A(FALINK control block)                  
         LAY   R0,SVFALINK                                                      
         ST    R0,FALASVE          Set A(FALINK save area)                      
                                                                                
         L     RF,ACOMFACS         Load support routines                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   LP_AUIR1,0(R1)      Set A(support overlay 1)                     
         MVC   AROUTS1,0(R1)                                                    
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   LP_AUIR2,0(R1)      Set A(support overlay 2)                     
         MVC   AROUTS2,0(R1)                                                    
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
         MVC   LP_AFALK,VFALINK    Set A(FALINK)                                
         MVC   LP_ATSAR,VTSAR      Set A(TSAR) for DDLINK's use                 
         MVC   FALTBLD,VTWABLD     Set A(TWABLD) for FALINK's use               
                                                                                
         MVC   WRKIACOM,ACOMFACS   Initialize WRKIO control block               
         LAY   R0,WRKREC                                                        
         ST    R0,WRKIAREC         Set A(worker record)                         
         LAY   R0,WRKBUFF                                                       
         ST    R0,WRKIABUF         Set A(worker buffer)                         
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
         LA    R0,WORKD            Set LP_BLKS addresses                        
         ST    R0,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         MVC   LP_BLKS+((B#SAVED-1)*L'LP_BLKS),ATIA                             
                                                                                
         MVI   LP_FLAG,0           Patchable flag byte                          
         GOTOR VDDLINK,LP_D        Pass control to DDLINK                       
                                                                                
         CLI   FALCONC,FCZERO                                                   
         JE    *+12                                                             
         CLI   FALCONC,FCDONE                                                   
         JNE   *+8                                                              
         MVI   SVOLAY,0            Reset overlay if all done                    
         OI    NAVINPH+6,X'C0'     Position cursor to action field              
         L     RD,4(RD)            Go back one level in chain                   
         XIT1  ,                   Exit directly back to MONITOR                
                                                                                
         LTORG                                                                  
                                                                                
* NENAVWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NENAV03   03/16/18'                                      
         END                                                                    
