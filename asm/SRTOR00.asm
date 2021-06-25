*          DATA SET SRTOR00    AT LEVEL 020 AS OF 08/22/00                      
*PHASE T16D00A                                                                  
         TITLE '$TOR/AOR/NOR - SET APPLICATION DIRECTION'                       
DIRECT   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL,**$TOR**,CLEAR=YES                                          
         USING WRKD,RC             RC=A(W/S)                                    
         ST    R1,SAVER1                                                        
*                                                                               
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRPARM6                                                       
         USING SRTORFFD,RA         RA=A(TWA)                                    
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRPARM3                                                       
         USING UTLD,R8             R8=A(UTL)                                    
*                                                                               
SRV      USING FHD,SRVSRVH                                                      
*                                                                               
         BAS   RE,INIT             INITIALISE                                   
*                                                                               
         TM    TSTATU,TSTATRER     IN DUE TO ROUTING ERROR?                     
         BZ    TOR02               NO                                           
*                                                                               
         NI    TSTATU,255-(TSTATRER)                                            
         NI    TSTAT7,255-(TST7DIR)                                             
         MVC   AORBYTE,TTORAOR                                                  
         NI    AORBYTE,TTASPACE                                                 
         NI    TTORAOR,255-(TTASPACE)                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,AORBYTE                                                       
         BCTR  RE,0                                                             
         LA    RF,MSGEL                                                         
         MSR   RE,RF                                                            
         LA    RE,MSGERR(RE)       SET ERROR CONDITION MESSAGE                  
         MVC   SRVERR(MSGEL),0(RE)                                              
         OI    SRVERRH+(FHOI-FHD),FHOITR                                        
*                                                                               
         MVC   SRVSRV,SPACES       SET =RE FOR NEXT ENTRY                       
         MVC   SRV.FHDA(3),=CL3'=RE'                                            
         OI    SRV.FHOI,FHOITR+FHOIMO                                           
*                                                                               
         LA    R1,MSGRTE                                                        
         BAS   RE,SETMSG           SET ROUTING ERROR MESSAGE                    
         B     EXIT                                                             
*                                                                               
TOR02    CLC   TOR,SRV.FHDA+1      WANT TO ROUTE ALL TO TOR?                    
         BNE   TOR04                                                            
         XC    AORBYTE,AORBYTE     TOR IS AOR ZERO                              
*                                                                               
         OI    TSTAT7,TST7DIR      TELL TASKER TO REDIRECT                      
         NI    TTORAOR,255-(TTASPACE)                                           
         OC    TTORAOR,AORBYTE                                                  
*                                                                               
         MVC   SRVSRV,SPACES       SET =RE FOR NEXT ENTRY                       
         MVC   SRV.FHDA(3),=CL3'=RE'                                            
         OI    SRV.FHOI,FHOITR+FHOIMO                                           
*                                                                               
         LA    R1,MSGTOR                                                        
         BAS   RE,SETMSG           DISPLAY TASK INFORMATION                     
         B     EXIT                                                             
*                                                                               
TOR04    CLC   AOR,SRV.FHDA+1      WANT TO ROUTE ALL TO AOR?                    
         BNE   TOR06                                                            
         MVI   AORBYTE,1           CAN ADD CODE HERE FOR MULTIPLE AORS          
*                                                                               
         OI    TSTAT7,TST7DIR      TELL TASKER TO REDIRECT                      
         NI    TTORAOR,255-(TTASPACE)                                           
         OC    TTORAOR,AORBYTE                                                  
*                                                                               
         MVC   SRVSRV,SPACES       SET =RE FOR NEXT ENTRY                       
         MVC   SRV.FHDA(3),=CL3'=RE'                                            
         OI    SRV.FHOI,FHOITR+FHOIMO                                           
*                                                                               
         LA    R1,MSGAOR                                                        
         BAS   RE,SETMSG           DISPLAY TASK INFORMATION                     
         B     EXIT                                                             
*                                                                               
TOR06    CLC   NOR,SRV.FHDA+1      WANT TO ROUTE ALL TO ANYWHERE                
         BNE   TOR08                                                            
         NI    TSTAT7,255-(TST7DIR)                                             
         NI    TTORAOR,255-(TTASPACE)                                           
*                                                                               
         MVC   SRVSRV,SPACES       SET =RE FOR NEXT ENTRY                       
         MVC   SRV.FHDA(3),=CL3'=RE'                                            
         OI    SRV.FHOI,FHOITR+FHOIMO                                           
*                                                                               
         LA    R1,MSGANY                                                        
         BAS   RE,SETMSG           DISPLAY TASK INFORMATION                     
         B     EXIT                                                             
*                                                                               
TOR08    DC    0H'0'               HOW HERE?                                    
*                                                                               
EXIT     XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL VALUES                                               *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
         MVC   SYSNAME,SSBSYSN4    GET SYSTEM NAME                              
INITX    XIT1  ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET CORRECT HEADER MESSAGE                                          *         
* NTRY: R1 = A(MESSAGE TO OUTPUT)                                     *         
***********************************************************************         
         SPACE 1                                                                
HDR      USING FHD,SRVMSGH                                                      
SETMSG   NTR1  ,                                                                
         MVI   SRVMSG,C' '         CLEAR MESSGAE FIELD                          
         MVC   SRVMSG+1(L'SRVMSG-1),SRVMSG                                      
*                                                                               
         MVI   SRVMSG,C'('         SET SYSTEM NAME                              
         MVC   SRVMSG+1(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         LA    RF,3(RF)                                                         
*                                                                               
         MVC   0(MSGL,RF),0(R1)                                                 
         OI    HDR.FHOI,FHOITR     TRANSMIT IT                                  
*                                                                               
SETMSGX  XIT1  ,                                                                
         DROP  HDR                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
SPACES   DC    20C' '                                                           
*                                                                               
TOR      DC    CL3'TOR'                                                         
AOR      DC    CL3'AOR'                                                         
NOR      DC    CL3'NOR'                                                         
*                                                                               
MSGL     EQU   45                                                               
MSGTOR   DC    CL(MSGL)'Transactions will now be routed through TOR'            
MSGAOR   DC    CL(MSGL)'Transactions will now be routed through AOR'            
MSGANY   DC    CL(MSGL)'Transactions will be routed normally'                   
MSGRTE   DC    CL(MSGL)'This transaction will not run in this mode'             
*                                                                               
MSGEL    EQU   60                                                               
MSGERR   DS    0X                                                               
MSGE1    DC    CL(MSGEL)'Program cannot run in requested region'                
MSGE2    DC    CL(MSGEL)'No Facpak found in region requested'                   
MSGE3    DC    CL(MSGEL)'Requested Facpak has not initialised'                  
MSGE4    DC    CL(MSGEL)'Requested Facpak is not available'                     
MSGE5    DC    CL(MSGEL)'Requested Facpak is at maximum tasks'                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WRKD     DSECT                                                                  
SAVER1   DS    A                                                                
AORBYTE  DS    X                   HOLDS AOR NUMBER FOR DIRECT                  
SYSNAME  DS    CL(L'SSBSYSNA)                                                   
WRKL     EQU   *-WRKD              SHOULD BE LESS THAN 256                      
         SPACE 2                                                                
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRTORFFD DSECT                                                                  
         DS    CL64                                                             
* SRTORFFD                                                                      
       ++INCLUDE SRTORFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SRTOR00   08/22/00'                                      
         END                                                                    
