*          DATA SET NEWRI40    AT LEVEL 024 AS OF 05/01/02                      
*PHASE T32040A                                                                  
         TITLE 'T32040 - FLIPPER REPORT'                                        
T32040   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FLIP**,RR=R2                                                 
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
         LA    R1,100(R7)                                                       
         ST    R1,AFLPTBL          FLIP TABLE = ANETWS2+100                     
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
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB,,ANETWS1                                              
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
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
         CLI   TESTRUN,C'Y'                                                     
         BE    EDT20                                                            
         CLI   TESTRUN,C'N'        IF MARKING FILE                              
         BNE   EDINV                                                            
         TM    WHEN,X'10'          MUST BE OVERNIGHT                            
         BNO   EDINV                                                            
*                                                                               
EDT20    DS    0H                                                               
         EJECT                                                                  
         SPACE 1                                                                
EDT22    LA    R2,SPLFLIPH         GET PROD CODES INTO TABLE IN REVERSE         
         L     R3,AFLPTBL                                                       
         CLI   5(R2),0                                                          
         BE    EDMISS                                                           
EDT24    MVC   5(3,R3),8(R2)       FLIP PRODS                                   
         BAS   R5,EDT40            CHK PRODS/GET PRDCODE                        
         MVC   1(1,R3),3(RE)                                                    
         ZIC   R1,0(R2)            BUMP TO 2DN PROD                             
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BE    EDMISS                                                           
         MVC   2(3,R3),8(R2)       SET 2ND PROD FIRST                           
         BAS   R5,EDT40                                                         
         MVC   0(1,R3),3(RE)       SET 2ND PRD CODE FIRST                       
EDT26    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'         PROTECTED FIELD                              
         BNO   EDT28                                                            
         CLI   0(R2),9                /END OF SCREEN                            
         BNH   EDTX                   /YES                                      
         B     EDT26                  /NO SKIP IT                               
EDT28    CLI   5(R2),0                                                          
         BE    EDTDISP                                                          
         LA    R3,8(R3)                                                         
         B     EDT24                                                            
*                                                                               
EDTDISP  DS    0H                  CLEAR SCREEN AFTER LAST PROD INPUT           
         CLI   0(R2),9                                                          
         BNH   EDTX                                                             
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         XC    8(3,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     EDTDISP                                                          
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
EDT40    DS    0H                  CHECK PROD AGAINST CLIST                     
*                                                                               
         L     RE,ANETWS1          CLIST                                        
         CLI   5(R2),3                                                          
         BE    EDT42                                                            
         MVI   10(R2),X'40'                                                     
EDT42    CLC   0(3,RE),8(R2)                                                    
         BER   R5                  OK                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   EDT42                                                            
         B     EDINV                                                            
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDMISS   DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX                                                            
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBSEQ,C'N'          NETWORK ORDER                                
         LA    R2,MAINLINE         SET NBHOOK WITH MAINLINE                     
         ST    R2,NBHOOK                                                        
FLP10    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   FLP10                                                            
         MVC   P+2(13),=C'UNITS UPDATED'                                        
         L     R3,COUNTER                                                       
         EDIT  (R3),(7,P+16),ALIGN=LEFT                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   TESTRUN,C'N'                                                     
         BE    XIT                                                              
         MVC   P+2(32),=C'(** TEST RUN-FILE NOT MARKED **)'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE                                                                  
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,0                                                       
         MVI   NBNOWRIT,0                                                       
*                                                                               
         CLI   NBMODE,NBPROCUN    ONLY WANT UNIT RECORDS                        
         BNE   FLPXIT                                                           
*                                                                               
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         L     R3,AFLPTBL                                                       
         USING FLPTBL,R3                                                        
FLP20    CLC   NUPRD(2),FLPPRD                                                  
         BE    FLP30                                                            
         LA    R3,8(R3)                                                         
         CLC   0(2,R3),=X'0000'                                                 
         BNE   FLP20                                                            
         B     FLPXIT              NO MATCH                                     
*                                                                               
FLP30    DS    0H                                                               
         MVC   NUPRD,FLPPRD2       FLIP PROD CODES                              
         MVC   NUPRD2,FLPPRD                                                    
         CLI   TESTRUN,C'Y'         IS IT TEST                                  
         BE    FLP40                                                            
         MVI   NBUPUNIT,C'Y'       NO/SET ON WRITE SWITCHES                     
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
FLP40    L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         LA    R4,P                                                             
         USING PLINE,R4                                                         
         CLC   NETSV,NBACTNET                                                   
         BE    *+16                                                             
         MVC   NETSV,NBACTNET                                                   
         MVC   PLNET,NBACTNET                                                   
         MVC   PLPROG,NBACTPRG                                                  
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,PLDATE)                              
         MVI   PLDATE+5,C'-'                                                    
         EDIT  (B1,NBACTSUB),(3,PLDATE+6),ALIGN=LEFT                            
         MVC   PLOPRDS(3),FLPPROD                                               
         MVI   PLOPRDS+3,C'-'                                                   
         MVC   PLOPRDS+4(3),FLPPROD2                                            
         MVC   PLNPRDS(3),FLPPROD2                                              
         MVI   PLNPRDS+3,C'-'                                                   
         MVC   PLNPRDS+4(3),FLPPROD                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
FLPXIT   B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
COUNTER  DS    F                                                                
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
         LA    R2,H8                                                            
         USING PLINE,R2                                                         
         MVC   PLNET,=C'NTWK'                                                   
         MVC   PLNET+132(4),=10C'-'                                             
         MVC   PLPROG,=C'PROGRM'                                                
         MVC   PLPROG+132(6),=10C'-'                                            
         MVC   PLDATE(4),=C'DATE'                                               
         MVC   PLDATE+132(9),=10C'-'                                            
         MVC   PLOPRDS(8),=C'OLD PRDS'                                          
         MVC   PLOPRDS+132(8),=10C'-'                                           
         MVC   PLNPRDS(8),=C'NEW PRDS'                                          
         MVC   PLNPRDS+132(8),=10C'-'                                           
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
AFLPTBL  DS    A                                                                
NETSV    DS    CL4                                                              
TESTRUN  DS    CL1                                                              
         ORG   MYD+100                                                          
FLPTBL   DS    0CL8                ORGANIZATION OF PROD TABLE                   
FLPPRD   DS    CL1                                                              
FLPPRD2  DS    CL1                                                              
FLPPROD  DS    CL3                                                              
FLPPROD2 DS    CL3                                                              
*                                                                               
PLINE    DSECT                                                                  
         DS    CL40                                                             
PLNET    DS    CL4                                                              
         DS    CL1                                                              
PLPROG   DS    CL6                                                              
         DS    CL1                                                              
PLDATE   DS    CL10                DATE  + SUB-LINE                             
         DS    CL1                                                              
PLOPRDS  DS    CL7                 OLD PROD ORDER                               
         DS    CL3                                                              
PLNPRDS  DS    CL7                 NEW PROD ORDER                               
         SPACE                                                                  
         SPACE                                                                  
*                                                                               
* NEGENINCLS                                                                    
* NEGENUNIT                                                                     
* SPGENCLT                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEAD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024NEWRI40   05/01/02'                                      
         END                                                                    
