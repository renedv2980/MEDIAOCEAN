*          DATA SET NEWRI22    AT LEVEL 045 AS OF 03/14/18                      
*PHASE T32022A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T32022 - DEMOSEED EDIT PHASE'                                   
T32022   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE22**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD        * ANETWS1+2 WORK AREA                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         LR    RE,R7             * ANETWS3 = CLIST                              
         LA    RF,MYDLENE                                                       
         XCEF                                                                   
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         ST    R2,RELO           * ANETWS4 = NDDEMBLK,DEDBLOCK                  
         SPACE 1                                                                
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,EDITMOD                                                       
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ANETWS3                                                       
         ST    RF,ACLIST                                                        
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    R2,WORK                   DUMMY POL PRODUCT                      
         MVC   8(3,R2),=C'POL'                                                  
         MVI   5(R2),3                                                          
         MVI   0(R2),11                                                         
         NETGO NVPRD,DMCB,0                                                     
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVEST,DMCB,SPLESTN,NDDEMBLK                                      
         OI    SPLESTNH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
**       XC    SVDEMOS,SVDEMOS                                                  
**       MVC   SVDEMOS(60),EDEMLST                                              
**       MVC   SVDEMOS+60(3),EDEM21                                             
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNET,DMCB                                                       
*                                                                               
         LA    R2,SPLNTISH               NTI PERIOD START                       
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),(X'80',NTISTART)                           
         OC    DMCB(4),DMCB                                                     
         BZ    EDINV                                                            
*                                                                               
         LA    R2,SPLNTIEH               NTI PERIOD END                         
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),(X'80',NTIEND)                             
         OC    DMCB(4),DMCB                                                     
         BZ    EDINV                                                            
         GOTO1 PERVERT,DMCB,NTISTART,NTIEND                                     
         CLC   12(2,R1),=AL2(NWEEKS)       MAX N WEEKS                          
         BH    WEEKERR                                                          
*                                                                               
                                                                                
                                                                                
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         XC    PAKTBL,PAKTBL                                                    
         LA    R2,SPLPAKH                PACKAGE                                
*****    CLI   5(R2),0                   NEED                                   
*****    BE    PAKEND                                                           
         NETGO NVPAK,DMCB                                                       
         OI    SPLPAKNH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         CLI   0(R3),X'02'         PACKAGE?                                     
         BNE   EDINV                                                            
         USING NPKEY,R3                                                         
         TM    NPAKCNTL,X'40'      IMP BASE?                                    
         BO    PAKOK                                                            
         CLI   5(R2),0             PKG INPUT?                                   
         BE    PAKEND              NO-SO OK (MAY BE MORE PKGS)                  
*                                    (NEWRI23 WILL PICK THEM UP)                
         B     EDINV               PKG INPUT - SO INVALID                       
PAKOK    MVC   PAKTBL(1),NPKPACK                                                
PAKEND   EQU   *                                                                
*                                                                               
         LA    R2,SPLSTRTH               UNIT START DATE                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               UNIT END DATE                          
         NETGO NVENDDAT,DMCB                                                    
         SPACE                                                                  
                                                                                
         LA    R2,SPLTSTH           TEST RUN?                                   
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   8(R2),C'Y'                                                       
         BE    EDT18                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EDINV                                                            
EDT18    MVC   TESTRUN,8(R2)                                                    
*                                  DIG OUT ANY OPTIONS                          
         LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         B     EDT20                                                            
*                                                                               
EDT20    DS    0H                                                               
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         PRINT GEN                                                              
EDERR    GOTO1 ERREX                                                            
*                                                                               
WEEKERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'MAXIMUM DATE SPREAD IS 13 WEEKS'                  
         OI    CONHEAD+6,X'80'                                                  
         GOTO1 ERREX2                                                           
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
* NOTE - MUST BE UPDATED ALSO IN NEWRI23                                        
RELO     DS    A                                                                
ACLIST   DS    A                                                                
         DS    A                   SPARE                                        
SVDEMOS  DS    CL78                25 DEMO CODES + 3 EOF                        
NTISTART DS    CL6                 YYMMDD NTI REQ START                         
NTIEND   DS    CL6                 YYMMDD NTI REQ END                           
NWEEKS   EQU   13                  MAX NUMBER OF NTI WEEKS                      
TESTRUN  DS    CL1                                                              
PAKTBL   DS    CL100               FOR 100 PKGS                                 
         DC    X'FF'                                                            
*                                                                               
MYDLENE  EQU   *-MYD                                                            
*******                    ANY NEW FIELDS GO HERE                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE2D                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NEGENPACK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045NEWRI22   03/14/18'                                      
         END                                                                    
