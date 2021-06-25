*          DATA SET PPLNK00S   AT LEVEL 006 AS OF 12/01/03                      
*PHASE T41400A                                                                  
PPLNK00  TITLE '- PRINT SYSTEM FALINK/DDLINK INTERFACE'                         
PPLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**PL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   APRTFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         OI    GIND1,GIONLINE      SET RUNNING ONLINE                           
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)        SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)        SET A(ROUTINE OVERLAY 2)                     
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
***********************************************************************         
* INITIALIZE CONTROL BLOCKS AND CALL DDLINK                           *         
***********************************************************************         
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,=AL2(LINKW-WORKD)                                           
         LA    R2,WORKD(R2)                                                     
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         L     R0,ATWA                                                          
         AHI   R0,SVFALINK-TWAD    A(FALINK SAVED STORAGE)                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
                                                                                
***********************************************************************         
* INITIALIZE ONLINE RUNNER VALUES (RUNFACSD)                          *         
***********************************************************************         
                                                                                
         LA    R0,TWAD             PASS A(4K SERVER SAVE AREA)                  
         AHI   R0,SVSERVER-TWAD                                                 
         STCM  R0,15,RSVRSAVE                                                   
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RDDLINK,VDDLINK                                                  
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
***********************************************************************         
* INITIALIZE DDLINK CONTROL BLOCK VALUES (LP_D)                       *         
***********************************************************************         
                                                                                
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         MVI   LP_AIND1,LP_AICOM   SET USE COMMA NOT SEMI-COLON                 
         MVI   LP_FLAG,0           PATCHABLE FOR TESTING                        
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,TWAD                                                          
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD         SET A(SECRET BLOCK)                          
         LA    R0,DDNDX                                                         
         STCM  R0,15,LP_ANDX       SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LA    R0,DDLINKW                                                       
         STCM  R0,15,LP_AWORK      SET A(DDLINK WORK AREA)                      
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         PASS A(WORK AREA AS BLOCK 1)                 
         MVC   LP_ABLK2,RSVRSAVE   PASS A(SAVE AREA AS BLOCK 2)                 
                                                                                
***********************************************************************         
* INITIALIZE WRKIO CONTROL BLOCK VALUES (WRKIOD)                      *         
***********************************************************************         
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO7       IO7 USED FOR WORKER RECORD AREA              
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
                                                                                
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
                                                                                
         OI    LNKSERVH+(FHAT-FHD),FHATMO                                       
         OI    LNKSERVH+(FHOI-FHD),FHOITR                                       
         OI    LNKINPH+(FHOI-FHD),FHOICU+FHOITR                                 
         XIT1  ,                                                                
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DDLINK MASTER MAP INDEX                                             *         
***********************************************************************         
                                                                                
DDNDX    DS    0H                                                               
*                                  INITIAL DOWNLOAD                             
         DC    AL2(M#DLINI),AL1(O#DLINI)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLINI)                                       
*                                  CLIENT/PRODUCT/ESTIMATE DOWNLOAD             
         DC    AL2(M#DLCPE),AL1(O#DLCPE)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLCPE)                                       
*                                  INSERTION DOWNLOAD (OFFLINE)                 
         DC    AL2(M#DLINS),AL1(O#DLINS)                                        
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#DLBUY)                                       
*                                                                               
* NOT USED ANYMORE                 INSERTION DOWNLOAD (ONLINE)                  
*        DC    AL2(M#DLION),AL1(O#DLINS)                                        
*        DC    AL1(LP_RNOWQ,0)                                                  
*        DC    AL1(PRTSYSQ),AL2(PP#DLBUY)                                       
*                                                                               
*                                  INSERTION HISTORY DOWNLOAD                   
         DC    AL2(M#DLHST),AL1(O#DLHST)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLHST)                                       
*                                  DRAFT INSERTION WIZARD                       
         DC    AL2(M#DLWIZ),AL1(O#DLWIZ)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLWIZ)                                       
*                                  CLT/PRD/EST/PUB VERIFIER                     
         DC    AL2(M#DLVER),AL1(O#DLVER)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLVER)                                       
*                                  ADCODE VALIDATION/DOWNLOAD                   
         DC    AL2(M#DLAVD),AL1(O#DLAVD)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLAVD)                                       
*                                  REFRESH INSERTION DOWNLOAD                   
         DC    AL2(M#DLREF),AL1(O#DLREF)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#DLREF)                                       
*                                  DRAFT INSERTION UPLOAD                       
         DC    AL2(M#ULDFT),AL1(O#ULDFT)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULDFT)                                       
*                                  LIVE INSERTION UPLOAD                        
         DC    AL2(M#ULNEW),AL1(O#ULNEW)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULNEW)                                       
*                                  CHANGE INSERTION UPLOAD                      
         DC    AL2(M#ULCHA),AL1(O#ULCHA)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULCHA)                                       
*                                  DELETE INSERTION UPLOAD                      
         DC    AL2(M#ULDEL),AL1(O#ULDEL)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULDEL)                                       
*                                  INSERTION ORDER UPLOAD                       
         DC    AL2(M#ULIOR),AL1(O#ULIOR)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULIOR)                                       
*                                  DOWNLOAD BY INVOICE NUMBER (OFFLINE)         
         DC    AL2(M#DLINVN),AL1(O#DLINVN)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#DLIVN)                                       
*                                  DOWNLOAD BY INVOICE KEY (OFFLINE)            
         DC    AL2(M#DLINVK),AL1(O#DLINVK)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#DLIVK)                                       
*                                  CLEAR FOR PAY UPLOAD                         
         DC    AL2(M#ULCLRP),AL1(O#ULCLP)                                       
******** DC    AL1(LP_RUXWQ)                                                    
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULCLP)                                       
*                                  CLEAR FOR PAY REFRESH INFO UPLOAD            
         DC    AL2(M#DLRPA),AL1(O#DLRPA)                                        
******** DC    AL1(LP_RUXWQ)                                                    
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#DLRPA)                                       
*                                  ADD INVOICE HEADER UPLOAD                    
         DC    AL2(M#ULAIH),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#AINVH)                                       
*                                  ADD INVOICE DETAIL UPLOAD                    
         DC    AL2(M#ULAII),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#AINVD)                                       
*                                  CHG INVOICE HEADER UPLOAD                    
         DC    AL2(M#ULCIH),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#CINVH)                                       
*                                  CHG INVOICE DETAIL UPLOAD                    
         DC    AL2(M#ULCII),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#CINVD)                                       
*                                  DEL INVOICE HEADER UPLOAD                    
         DC    AL2(M#ULDIH),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#DINVH)                                       
*                                  DEL INVOICE DETAIL UPLOAD                    
         DC    AL2(M#ULDII),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#DINVD)                                       
*                                  UNLINK INVOICE AND INSERTION UPLOAD          
         DC    AL2(M#ULULK),AL1(O#ULINV)                                        
         DC    AL1(LP_RUPLQ+LP_RGLBQ,0)                                         
         DC    AL1(PRTSYSQ),AL2(PP#ULKIV)                                       
*                                  PROFILE DOWNLOAD                             
         DC    AL2(M#DLPROF),AL1(O#DLPROF)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#PRFDL)                                       
*                                  CFM MEDIA DOWNLOAD                           
         DC    AL2(M#CFMMED),AL1(O#CFMMED)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(PRTSYSQ),AL2(PP#CFMMD)                                       
*                                  CFM CLIENT DOWNLOAD (OFFLINE)                
         DC    AL2(M#CFMCLT),AL1(O#CFMCLT)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#CFMCL)                                       
*                                  CFM PRODUCT DOWNLOAD (OFFLINE)               
         DC    AL2(M#CFMPRD),AL1(O#CFMPRD)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#CFMPR)                                       
*                                  CFM PUBLICATION DOWNLOAD (OFFLINE)           
         DC    AL2(M#CFMPUB),AL1(O#CFMPUB)                                      
         DC    AL1(LP_RNOWQ+LP_RCONQ+LP_RDSCQ,0)                                
         DC    AL1(PRTSYSQ),AL2(PP#CFMPU)                                       
                                                                                
DDNDXX   DC    AL2(0)                                                           
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPLNK00S  12/01/03'                                      
         END                                                                    
