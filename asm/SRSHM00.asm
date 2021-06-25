*          DATA SET SRSHM00    AT LEVEL 006 AS OF 05/02/12                      
*PHASE T17800A                                                                  
         TITLE 'T17800 - SHARE MEMORY FACILITY'                                 
SHMFAC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**$SHM**,R8,RR=R2,CLEAR=YES                            
         USING WRKD,RC                                                          
         SAM31                                                                  
*                                                                               
         ST    R2,RELO                                                          
         MVC   SRPARMS(24),0(R1)                                                
         L     R1,SRPARMS          A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         MVC   ASSB,VSSB                                                        
         DROP  R1                                                               
         L     R1,SRPARMS+8        A(UTL)                                       
         ST    R1,AUTL                                                          
*                                                                               
         L     RA,SRPARMS+20                                                    
         USING T178FFD,RA          RA=A(T178FFD)  FOR WHOLE PROGRAM             
         L     R9,SRPARMS+12                                                    
         USING COMFACSD,R9         R9=A(COMFACSD) FOR WHOLE PROGRAM             
         EJECT                                                                  
                                                                                
***********************************************************************         
MAIN     DS    0H                                                               
*                                  LOOK FOR COMMANDS                            
         LA    R2,SHMCM1H                                                       
         OC    SHMCM1,SHMCM1                                                    
         BZ    LIST                                                             
         CLI   SHMCM1,C'?'                                                      
         BE    BADCMD                                                           
         CLC   =C'ATTACH',SHMCM1                                                
         BE    ATTACH                                                           
         CLC   =C'DETACH',SHMCM1                                                
         BE    DETACH                                                           
         CLC   =C'RESET',SHMCM1                                                 
         BE    RESET                                                            
         CLC   =C'LOCK',SHMCM1                                                  
         BE    LOCK                                                             
         CLC   =C'UNLOCK',SHMCM1                                                
         BE    UNLOCK                                                           
         B     BADCMD                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
         USING SSBD,R5                                                          
LIST     DS    0H                                                               
         L     R5,ASSB                                                          
         ICM   R3,15,SSBSHMTB                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         USING SHMTABD,R3                                                       
         LA    R6,SHMTB1H                                                       
         LA    R7,SHMTB1                                                        
         LHI   R8,SHMTB2H-SHMTB1H                                               
*                                                                               
LIST010  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    LISTX                                                            
*                                                                               
         OI    6(R6),X'80'         TRANSMIT THIS FIELD                          
         USING TABLIND,R7                                                       
         MVC   TABNAME,SHMTNAME                                                 
         L     R4,SHMTKEY                                                       
         USING SHMKEYD,R4                                                       
         GOTO1 CHEXOUT,PARA,SHMKEY,TABKEY,4,=C'TOG'                             
         GOTO1 CHEXOUT,PARA,SHMKADDR,TABADDR,4,=C'TOG'                          
         GOTO1 CHEXOUT,PARA,SHMKSTAR,TABSTAR,4,=C'TOG'                          
         GOTO1 CHEXOUT,PARA,SHMKSIZE,TABSIZE,4,=C'TOG'                          
         GOTO1 CHEXOUT,PARA,SHMKSMID,TABSMID,4,=C'TOG'                          
         CLC   SHMKADDR,=F'0'                                                   
         BE    LIST030                                                          
         L     R1,SHMKSTAR                                                      
         LA    R1,8(0,R1)          INFO ABOUT LOCK THIS TAB                     
         GOTOR CPROTOFF                                                         
         MVC   FULL,0(R1)                                                       
         GOTOR CPROTON                                                          
         GOTO1 CHEXOUT,PARA,FULL,TABLOCK,4,=C'TOG'                              
*                                                                               
LIST030  LA    R3,SHMTABLQ(0,R3)                                                
         AR    R6,R8                                                            
         AR    R7,R8                                                            
         B     LIST010                                                          
*                                                                               
LISTX    B     OK                                                               
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
ATTACH   DS    0H                                                               
         OC    SHMCM2,SPACES                                                    
         GOTO1 CDATAMGR,P0,=C'SHMUSS',(0,=CL8'ATTACH'),                +        
               (0,SHMCM2),0,0                                                   
         BL    BADNME                                                           
         B     LIST                REDISPLAY SHM LISTING                        
         EJECT                                                                  
***********************************************************************         
DETACH   DS    0H                                                               
         OC    SHMCM2,SPACES                                                    
         GOTO1 CDATAMGR,P0,=C'SHMUSS',(0,=CL8'DETACH'),                +        
               (0,SHMCM2),0,0                                                   
         BL    BADNME                                                           
         B     LIST                REDISPLAY SHM LISTING                        
         EJECT ,                                                                
***********************************************************************         
* LOCK THE TABLE                                                                
***********************************************************************         
LOCK     DS    0H                                                               
         CLC   =C'STAPACK',SHMCM2                                               
         BE    LKSTPK                                                           
         LA    R2,SHMCM2H                                                       
         B     BADCMD                                                           
*                                                                               
         USING SSBD,R1                                                          
LKSTPK   L     R1,ASSB                                                          
         L     R3,SSBSHMTB                                                      
         USING SHMTABD,R3                                                       
         DROP  R1                                                               
*                                                                               
         LHI   R0,SHMTABLQ                                                      
LKSPK10  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    LKSPK50                STAPACK SHM DEF NOT FOUND                 
*                                                                               
         USING SHMKEYD,R4                                                       
         L     R4,SHMTKEY                                                       
         CLC   =CL8'STAPACK',SHMTNAME                                           
         BNE   LKSPK20                                                          
         L     RE,SHMKSTAR                                                      
         USING MSAGSHMD,RE                                                      
         TS    MSAGTLCK            JUST LOCK IT!                                
         B     LKSPKX                                                           
         DROP  R4,RE                                                            
*                                                                               
LKSPK20  AR    R3,R0                                                            
         B     LKSPK10                                                          
         DROP  R3                                                               
*                                                                               
LKSPK50  DS    0H                                                               
*                                                                               
* WARNING FOR MISSING SHM TABLE DEF                                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
LKSPKX   B     LIST                REDISPLAY SHM LISTING                        
         EJECT                                                                  
                                                                                
***********************************************************************         
UNLOCK   DS    0H                                                               
         CLC   =C'STAPACK',SHMCM2                                               
         BE    UKSTPK                                                           
         LA    R2,SHMCM2H                                                       
         B     BADCMD                                                           
*                                                                               
UKSTPK   DS    0H                                                               
         L     R1,ASSB                                                          
         USING SSBD,R1                                                          
         L     R3,SSBSHMTB                                                      
         USING SHMTABD,R3                                                       
         DROP  R1                                                               
*                                                                               
         LHI   R0,SHMTABLQ                                                      
UKSPK10  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    UKSPK50                STAPACK SHM DEF NOT FOUND                 
*                                                                               
         L     R4,SHMTKEY                                                       
         USING SHMKEYD,R4                                                       
         CLC   =CL8'STAPACK',SHMTNAME                                           
         BNE   UKSPK20                                                          
         L     RE,SHMKSTAR                                                      
         USING MSAGSHMD,RE                                                      
         MVI   MSAGTLCK,0          JUST UNLOCK IT!                              
         DROP  R4,RE                                                            
         B     UKSPKX                                                           
*                                                                               
UKSPK20  AR    R3,R0                                                            
         B     UKSPK10                                                          
         DROP  R3                                                               
*                                                                               
UKSPK50  DS    0H                                                               
*                                                                               
* WARNING FOR MISSING SHM TABLE DEF                                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
UKSPKX   B     LIST                REDISPLAY SHM LISTING                        
         EJECT                                                                  
                                                                                
***********************************************************************         
RESET    DS    0H                                                               
         CLC   =C'STAPACK',SHMCM2                                               
         BE    RSSTPK                                                           
         LA    R2,SHMCM2H                                                       
         B     BADCMD                                                           
*                                                                               
RSSTPK   MVC   PARA+4(3),=X'D9000A'   GET V(STAPACK)                            
         MVI   PARA+7,QSTAPACK                                                  
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'Z'                                                     
         STCM  R9,15,STAPACOM                                                   
         MVC   STAPAGY,SHMCM3      2 CHAR AGENCY, JUST CLEAR THIS AGY           
         CLC   =C'ALL',SHMCM3      CLEAR ALL                                    
         BNE   *+10                                                             
         MVC   STAPAGY,=X'FFFF'                                                 
         DROP  R1                                                               
         GOTO1 (RF),(R1)                                                        
         B     LIST                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
BADNME   MVC   MSG(27),=CL27'NOT A VALID KEY NAME'                              
         J     *+10                                                             
BADCMD   MVC   MSG(27),=C'NO VALID COMMAND RECOGNISED'                          
         FOUT  SHMTB2H,=C'COMMANDS - ATTACH   SHM_KEY'                          
         FOUT  SHMTB3H,=C'           DETACH   SHM_KEY'                          
         FOUT  SHMTB4H,=C'           LOCK     SHM_KEY'                          
         FOUT  SHMTB5H,=C'           UNLOCK   SHM_KEY'                          
         FOUT  SHMTB6H,=C'           MSRESET  STAPACK  ALL/AG'                  
         FOUT  SHMTB7H,=C'                           '                          
         B     ALLX                                                             
*                                                                               
OK       OI    SHMCM3H+6,X'80'                                                  
         OI    SHMCM4H+6,X'80'                                                  
         MVC   MSG(29),=C'RESULT DISPLAYED - ENTER NEXT'                        
         LA    R2,SHMSRVH                                                       
*                                                                               
ALLX     OI    6(R2),X'40'                                                      
         FOUT  SHMMSGH,MSG,60                                                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                                  
***********************************************************************         
SPACES   DC    CL64' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WRKD     DSECT                                                                  
DUB      DS    D                                                                
P0       DS    F                                                                
PARA     DS    6F                                                               
WORK     DS    CL32                                                             
RELO     DS    A                                                                
MSG      DS    CL60                                                             
BYTE     DS    C                                                                
NUM      DS    F                                                                
MYRE     DS    F                                                                
CASH     DS    F                                                                
FULL     DS    F                                                                
AUTL     DS    A                                                                
ASSB     DS    A                                                                
DATE     DS    CL6                                                              
SRPARMS  DS    6F                                                               
STAWORK  DS    XL32                STAPACK INTERFACE AREA                       
WRKX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
TABLIND  DSECT                                                                  
TABNAME  DS    CL8                                                              
         DS    2C                                                               
TABKEY   DS    CL8                                                              
         DS    2C                                                               
TABADDR  DS    CL8                                                              
         DS    2C                                                               
TABSTAR  DS    CL8                                                              
         DS    2C                                                               
TABSIZE  DS    CL8                                                              
         DS    2C                                                               
TABSMID  DS    CL8                                                              
         DS    2C                                                               
TABLOCK  DS    CL8                                                              
***********************************************************************         
       ++INCLUDE FASYSFAC                                                       
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
SRSHMFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRSHMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DMSHMUSSD                                                      
         EJECT                                                                  
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRSHM00   05/02/12'                                      
         END                                                                    
