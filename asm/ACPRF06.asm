*          DATA SET ACPRF06    AT LEVEL 001 AS OF 09/29/04                      
*PHASE T63006A                                                                  
*INCLUDE WRKIO                                                                  
ACPRF06  TITLE 'PRESTO - BOOTSTRAP TO DDLINK'                                   
ACPRF06  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OWORKL,*T63006*,RR=RE,CLEAR=YES                                  
         USING OWORKD,RC           RC=A(LOCAL W/S)                              
         ST    RE,ORELO            SAVE RELOCATION FACTOR                       
         LR    R9,R1                                                            
         USING WORKD,R9            R9=(GLOBAL W/S)                              
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
***********************************************************************         
* INITIALIZE ONLINE RUNNER VALUES (RUNFACSD)                          *         
***********************************************************************         
                                                                                
         MVC   RSVRSAVE,ATIA       SET A(SERVER WORK AREA)                      
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         L     R0,VWRKIO                                                        
         A     R0,ORELO                                                         
         ST    R0,RWRKIO           SET A(WRKIO)                                 
         L     RF,ACOMFACS         SET A(DDLINK) FROM COMFACS                   
         MVC   RDDLINK,CDDLINK-COMFACSD(RF)                                     
         MVC   RRUNIT,CRUNIT-COMFACSD(RF)                                       
         LA    R0,FACLIST                                                       
         ST    R0,RSYSFACS         SET A(SERVER FACILITIES LIST)                
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
***********************************************************************         
* INITIALIZE DDLINK CONTROL BLOCK VALUES (LP_D)                       *         
***********************************************************************         
                                                                                
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP                              
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVOVDATA-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,MAP                                                           
         ST    R0,LP_ANDX          SET A(MASTER MAP INDEX)                      
         LHI   R0,SVOVDATA-TWAD                                                 
         STCM  R0,3,LP_DSAVE       SET DISP. TO OVERLAY SAVE IN TWA             
         LA    RE,TWAD                                                          
         AR    R0,RE                                                            
         ST    R0,LP_ABLKF         SET A(OVERLAY SAVE AREA) IN TWA              
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         MVC   LP_AWORK,AIO1       IO1/2 USED FOR DDLINK WORK                   
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FABLK                                                         
         ST    R0,LP_AFBLK         SET A(FALINK CONTROL BLOCK)                  
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
                                                                                
***********************************************************************         
* INITIALIZE WRKIO CONTROL BLOCK VALUES (WRKIOD)                      *         
***********************************************************************         
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO4       IO3 USED FOR WORKER RECORD AREA              
         LA    R0,WRKBUFF                                                       
         ST    R0,WRKIABUF         SET A(WORKER BUFFER)                         
                                                                                
         GOTOR RDDLINK,LP_D        PASS CONTROL TO DDLINK                       
         CLI   SVREASON,GLOBJBSC   IF 2ND CALL THEN DONE                        
         BE    EXIT                                                             
         MVI   SVXFROV,X'06'       SET CALLING OVERLAY TO CALL AGAIN            
         MVI   SVREASON,GLOBJBSC   SET REASON FOR CALL (JOB SCRIPT)             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
ONEK     EQU   1024                                                             
                                                                                
VWRKIO   DC    V(WRKIO)                                                         
                                                                                
         LTORG                                                                  
                                                                                
MAP      DS    0XL(LN_LNQ)         ** MASTER MAP INDEX **                       
**** INSERT MAPCODES HERE IF ANY                                                
MAPX     DC    AL2(0)                                                           
         EJECT                                                                  
OWORKD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
ORELO    DS    A                   PROGRAM RELOCATION FACTOR                    
FACLIST  DS    64F                 SERVER FACILITIES LIST                       
DDRUNP   DS    (RUNPARML)X         RUNPARMS EMULATION                           
DDRUNF   DS    (RUNFACSL)X         RUNFACS EMULATION                            
DDLINKC  DS    (LP_LNQ)X           DDLINK CONTROL BLOCK                         
DDWRKIOC DS    (WRKIOBL)X          WRKIO CONTROL BLOCK                          
WRKBUFF  DS    XL(18*ONEK)         WORKER BUFFER                                
OWORKL   EQU   *-OWORKD                                                         
                                                                                
* ACPRFWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPRFWRK                                                       
         PRINT ON                                                               
                                                                                
* DDLINKD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLINKD                                                        
         PRINT ON                                                               
                                                                                
* DDRUNNERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDRUNNERD                                                      
         PRINT ON                                                               
                                                                                
* DDWRKIOD                                                                      
         PRINT OFF                                                              
WRKIOD   DSECT                                                                  
       ++INCLUDE DDWRKIOD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACPRF06   09/29/04'                                      
         END                                                                    
