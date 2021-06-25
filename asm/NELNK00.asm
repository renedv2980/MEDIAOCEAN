*          DATA SET NELNK00    AT LEVEL 009 AS OF 01/07/11                      
*PHASE T30200A                                                                  
NELNK00  TITLE '- NETWORK SYSTEM FALINK/DDLINK INTERFACE'                       
NELNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**NL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS         LOAD SUPPORT ROUTINES                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)       SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)       SET A(ROUTINE OVERLAY 2)                     
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,=AL2(LINKW-WORKD)                                           
         LA    R2,WORKD(R2)                                                     
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LP_D             SET A(LP_D) IN GLOBAL W/S                    
         ST    R0,ALP                                                           
         PRINT GEN                                                              
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
         GOTOR (#GETAGY,AGETAGY),TWAAGY                                         
         PRINT NOGEN                                                            
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,SWITCH     A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         L     R0,ATWA                                                          
         AHI   R0,SVFALINK-TWAD    A(FALINK SAVED STORAGE)                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
                                                                                
         MVC   RSVRSAVE,ATIA       (FOR DDLINK USE)                             
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RRUNIT,RUNIT                                                     
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
         MVI   LP_AIND1,LP_AIUPL+LP_AIRLP+LP_AICOM                              
         MVC   LP_AGY,TWAAGY       SET AGENCY ALPHA CODE                        
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
         MVC   LP_ATSAR,TSAR       SET A(TSAR)                                  
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,TWAD                                                          
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD         SET A(SECRET CONTROL BLOCK)                  
         LA    R0,DDNDX                                                         
         STCM  R0,15,LP_ANDX       SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LA    R0,LINKW                                                         
         AHI   R0,DDLINKW-LINKW                                                 
         STCM  R0,15,LP_AWORK      SET A(DDLINK WORK AREA)                      
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         MVC   LP_AFALK,FALINK     SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LA    R0,TWAD                                                          
         AHI   R0,SVSERVER-TWAD                                                 
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R0,TWAD                                                          
         ST    R0,LP_BLKS+((B#TWAD-1)*L'LP_BLKS)                                
         LHI   R0,SVRETURN-TWAD                                                 
         STCM  R0,3,LP_DSAVE       SET DISP. TO SERVER RETURN AREA              
                                                                                
         LA    R0,LINKW                                                         
         AHI   R0,WMPAREA-LINKW    INCREASED WORK POOL AREA                     
         STCM  R0,15,LP_AUWMP                                                   
         LHI   R0,L'WMPAREA                                                     
         STCM  R0,3,LP_WMPL                                                     
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO5       IO5+IO6+IO7+IO8 =WORKER REC AREA             
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
                                                                                
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         GOTOR DDLINK,LP_D         PASS CONTROL TO DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKINPH+6,X'C0'                                                  
         XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DDNDX    DS    0X                  ** MASTER MAP INDEX **                       
                                                                                
         DC    AL2(M#INIT),AL1(X'10')  INITIAL DOWNLOAD                         
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#INIT)                                        
                                                                                
         DC    AL2(M#LTBK),AL1(X'10')  LATEST BOOK DOWNLOAD                     
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#LBDL)                                        
                                                                                
         DC    AL2(M#SMAST),AL1(X'10') MASTER RECDS DOWNLOAD                    
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#NTWKS)                                       
                                                                                
         DC    AL2(M#DDEF),AL1(X'10')  DEMDEF RECDS DOWNLOAD                    
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#DDEF)                                        
                                                                                
         DC    AL2(M#DDSTM),AL1(X'10') DEMDEF STAMP DOWNLOAD                    
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#DDST)                                        
                                                                                
         DC    AL2(M#CNRP),AL1(X'11')  AUD ESTIMATOR REPORT DOWNLOAD            
         DC    AL1(LP_RNOWQ+LP_RDSCQ,0)                                         
         DC    AL1(NETSYSQ),AL2(NE#ESREP)                                       
                                                                                
         DC    AL2(M#PDOWN),AL1(X'11')  PROGRAM RECORDS DOWNLOAD                
         DC    AL1(LP_RNOWQ+LP_RDSCQ,0)                                         
         DC    AL1(NETSYSQ),AL2(NE#PDOWN)                                       
                                                                                
         DC    AL2(M#DEINFO),AL1(X'10') DEMO ENGINE - INFO                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#DEINF)                                       
                                                                                
         DC    AL2(M#DEFORM),AL1(X'11') DEMO ENGINE - FORMULAS                  
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(NETSYSQ),AL2(NE#DEFOR)                                       
                                                                                
DDNDXX   DC    AL2(0)                                                           
                                                                                
         PRINT OFF                                                              
       ++INCLUDE NELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NELNK00   01/07/11'                                      
         END                                                                    
