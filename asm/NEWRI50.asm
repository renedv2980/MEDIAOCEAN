*          DATA SET NEWRI50    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T32050A                                                                  
         TITLE 'T32050 - MM TRANSFER   '                                        
T32050   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MMTR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            ANETWS1=CLIST                                
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ANETWS2=WORKING STORAGE                      
         USING MYD,R7                                                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB,                                                      
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTALL,DMCB                                                    
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1               OPTIONAL                                
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPT,DMCB                                                       
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
*                                                                               
         LA    R2,SPLRSTRH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLRENDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         MVI   TESTRUN,C'Y'                                                     
         LA    R2,SPLTESTH               TEST RUN                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         MVC   TESTRUN,FLD                                                      
*                                                                               
EDT20    DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    EDTX                                                             
         CLI   T320FFD+1,C'*'      IF NOT DDS                                   
         BNE   EDINV                                                            
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
         XC    COUNTER,COUNTER                                                  
         XC    COUNTU,COUNTU                                                    
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         MVI   NBDATA,C'B'         UNITS AND PACKAGES                           
         MVI   NBRESUME,NBPROCPK   RE-READ 1ST PACKAGE                          
         GOTO1 NBCLPACK,DMCB,=C'MM2',NEWCC    NEW CLIENT CODE                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
MM10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    MM15                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   MM10                                                             
         BAS   RE,DOUNIT                                                        
         B     MM10                                                             
*                                                                               
MM15     MVC   P+2(13),=C'UNITS UPDATED'                                        
         L     R3,COUNTER                                                       
         EDIT  (R3),(7,P+16),ALIGN=LEFT                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   TESTRUN,C'N'                                                     
         BE    XIT                                                              
         MVC   P+2(32),=C'(** TEST RUN-FILE NOT MARKED **)'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DOUNIT   NTR1                                                                   
         MVI   NBUPUNIT,0                                                       
         MVI   NBNOWRIT,0                                                       
*                                                                               
         CLI   NBMODE,NBPROCPK    PACKAGE                                       
         BNE   MAIN20                                                           
         L     R3,NBAIO                                                         
         USING NPKEY,R3                                                         
         MVC   P+1(4),=C'PKG1'                                                  
         LA    R2,P+7                                                           
         GOTO1 HEXOUT,DMCB,(R3),(R2),30                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(4),=C'PKG2'                                                  
         MVC   NPKCLT,NEWCC                                                     
         LA    R2,P+7                                                           
         GOTO1 HEXOUT,DMCB,(R3),(R2),30                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MAIN40                                                           
*                                                                               
MAIN20   CLI   NBMODE,NBPROCUN    UNIT                                          
         BNE   MAINX                                                            
*                                                                               
         L     R1,COUNTU                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNTU                                                        
         EDIT  (R1),(5,P+1)                                                     
         L     R3,NBAIO                                                         
         MVC   P+7(4),=C'UNT1'                                                  
         LA    R2,P+14                                                          
         GOTO1 HEXOUT,DMCB,(R3),(R2),30                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+14(60),0(R3)                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   ELCODE,X'21'        COMMERCIAL                                   
         L     R6,NBAIO         CLEAR ELEMENT BUT LEAVE IT AS ZEROS             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MAIN25   BAS   RE,NEXTEL                                                        
         BNE   MAIN30                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     MAIN25                                                           
         XC    2(0,R6),2(R6)                                                    
*                                                                               
MAIN30   MVC   AIO,NBAIO                                                        
         MVI   ELCODE,X'10'        BILLING                                      
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'12'        PAYING                                       
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'22'        FEED                                         
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'23'        COMMERCIAL                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'24'        COMMERCIAL                                   
         GOTO1 REMELEM                                                          
*                                                                               
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         MVC   NUKCLT,NEWCC        SET NEW CLIENT CODE                          
         L     R3,NBAIO                                                         
         MVC   P+7(4),=C'UNT2'                                                  
         LA    R2,P+14                                                          
         GOTO1 HEXOUT,DMCB,(R3),(R2),30                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                                                               
MAIN40   L     R1,COUNTER          ADD TO COUNTER                               
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
*                                                                               
         CLI   TESTRUN,C'Y'         IS IT TEST                                  
         BE    MAIN50                                                           
         L     R1,NBAIO                                                         
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY(20),0(R1)                                                  
         LA    R3,MYKEY+21                                                      
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'UNTFILE ',(R3),NBAIO,MYDM             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NBFUNCT,NBFRDHI                                                  
MAIN50   DS    0H                                                               
*                                                                               
MAINX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
COUNTER  DS    F                                                                
COUNTU   DS    F                                                                
         EJECT                                                                  
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(8),SPLEST                                                  
         CLI   SPLDPT,X'40'                                                     
         BNH   *+16                                                             
         MVC   H5+63(1),SPLDPT                                                  
         MVC   H5+55(7),=C'DAYPART'                                             
         CLI   SPLPAK,X'40'                                                     
         BNH   *+16                                                             
         MVC   H6+63(3),SPLPAK                                                  
         MVC   H6+55(7),=C'PACKAGE'                                             
HOOKX    B     XIT                                                              
*                                                                               
         SPACE 3                                                                
*              SPECS FOR PHASE                                                  
MYSPECS  DS    0F                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
TESTRUN  DS    CL1                                                              
NEWCC    DS    CL2                                                              
MYKEY    DS    CL30                                                             
         DS    0D                                                               
MYDM     DS    CL96                MY DMWORK FOR ADDREC                         
*                                                                               
         SPACE                                                                  
*                                                                               
* NEGENINCLS                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID6D                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEWRI50   05/01/02'                                      
         END                                                                    
