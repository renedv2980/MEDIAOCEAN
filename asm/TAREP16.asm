*          DATA SET TAREP16    AT LEVEL 002 AS OF 11/18/14                      
*PHASE T70316D,*                                                                
         TITLE 'T70316 - QUARTERLY WORKERS COMP'                                
T70316   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70316,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         ST    RB,MYRB                                                          
         ST    R7,MYR7                                                          
         ST    R8,MYR8                                                          
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         OC    ABUFFER,ABUFFER     FREEMAIN IF NEEDED                           
         BZ    XIT                                                              
         L     R0,LBUFFER                                                       
         L     R1,ABUFFER                                                       
         FREEMAIN RC,A=(1),LV=(0)                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,SPLPERH          PERIOD                                       
         GOTO1 ANY                 (REQUIRED)                                   
         ST    R2,APERH                                                         
         GOTO1 VALPERD                                                          
         MVI   TIQDTYPE,TIQDCHK    (FILTER ON CHECK DATE)                       
         SPACE 1                                                                
VREC2    LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK                                                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         SPACE 1                                                                
VREC4    LA    R2,SPLUNITH         TAX UNIT                                     
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC8                                                            
         GOTO1 ANY                                                              
         CLI   WORK,C'@'           (MAY BE A FLIST RECORD)                      
         BE    VREC6                                                            
         MVC   TIFUNIT,WORK                                                     
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   BADUNIT                                                          
         B     VREC6X                                                           
         SPACE 1                                                                
VREC6    MVI   TGLTYP,TLGLTYPF     SET FOR FLIST RECORD VALIDATION              
         MVC   TGLST,WORK+1                                                     
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'08',0),0                                  
         BNE   BADLIST                                                          
         MVC   TIFUNIT,TGLST                                                    
         NI    TIFUNIT,X'7F'       SET UNIT FILTER IS FLIST RECORD              
VREC6X   OI    TIFTUNIT,TIFTWRK    FILTER BY WORK STATE                         
         SPACE 1                                                                
VREC8    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'99999999'                                            
         MVI   DETOPT,C'Y'                                                      
         MVI   GREYOPT,C'N'        NO GREY RECORDS                              
         MVI   TABLOPT,C'N'        DEFAULT TO NEW TABLE                         
         MVI   CORPOPT,C'N'        DON'T REPORT ON CORPS                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(3,R4),=C'SSN'    TESTING OPTION 1 SS#                         
         BNE   OPT8                                                             
         MVC   TIFSSN,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(6,R4),=C'AGENCY' AGENCY OPTION                                
         BNE   OPT9                                                             
         MVC   TIFAGY,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(6,R4),=C'OFFICE' OFFICE OPTION                                
         BNE   OPT10                                                            
         MVC   TIFOFF,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'GREY'   OPTION TO READ GREY RECORDS                  
         BNE   OPT12                                                            
         MVC   GREYOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(6,R4),=C'DETAIL' OPTION TO GET DETAIL REPORT                  
         BNE   OPT14                                                            
         MVC   DETOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(3,R4),=C'OLD'    OPTION TO USE OLD TABLE                      
         BNE   OPT16                                                            
         MVI   TABLOPT,C'O'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(3,R4),=C'NYQ'    OPTION TO USE NY QUARTER MAX                 
         BNE   OPT18                                                            
         MVI   TABLOPT,C'Q'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(5,R4),=C'CORPS'   OPTION TO GET CORPS ONLY                    
         BNE   OPT20                                                            
         MVI   CORPOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    DS    0H                                                               
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     BADXIT                                                           
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LSTERR),LSTERR                                         
         B     BADXIT                                                           
         SPACE 1                                                                
BADUNIT  MVC   CONHEAD(L'UNTERR),UNTERR                                         
BADXIT   GOTO1 ERREX2                                                           
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
LSTERR   DC    C'** ERROR ** INVALID LIST CODE'                                 
UNTERR   DC    C'** ERROR ** INVALID UNIT'                                      
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              HANDLE REPORT                                                    
         SPACE 3                                                                
PREP     NTR1                                                                   
         BAS   RE,MYINIT           INITIALIZE                                   
         BAS   RE,READCHEK         USE SYSIO FOR CHECKS/BILLS                   
         CP    SRTCOUNT,=P'0'      NOTHING TO REPORT                            
         BE    XIT                 EXIT OR SORTER WILL ABEND                    
         ZAP   SRTCOUNT,=P'0'                                                   
         BAS   RE,DOREPS           THEN DO THE REPORTS                          
         B     XIT                                                              
         EJECT                                                                  
*              INITIAL FOR REPORTS                                              
         SPACE 3                                                                
MYINIT   NTR1                                                                   
         ZAP   TRACOUNT,=P'0'      SET COUNTERS                                 
         ZAP   RECCOUNT,=P'0'                                                   
         ZAP   REPCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         ZAP   INVCOUNT,=P'0'                                                   
         ZAP   SRTCOUNT,=P'0'                                                   
         ZAP   INPCOUNT,=P'0'                                                   
         SPACE 1                                                                
         BAS   RE,MYCLEAR                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
INIT20   MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         MVC   MYTITLE,=CL32'QUARTERLY WORKERS COMPENSATION'                    
         SPACE 1                                                                
         XC    WEEKLIST,WEEKLIST   BUILD WEEK LIST                              
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(0,WORK)                                 
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,EBCEND)                               
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLI   DMCB,7              IS START DATE SUNDAY?                        
         BE    WKL2                                                             
         ZIC   R0,DMCB             NO - SO SKIP TO IT                           
         LA    R1,7                                                             
         SR    R1,R0                                                            
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
WKL2     LA    R2,WEEKLIST         NOW BUILD 54 SUNDAYS - PACKED                
         LA    R3,7                                                             
         LA    R0,54                                                            
         SPACE 1                                                                
WKL4     CLC   WORK(6),EBCEND      GET TO END YET?                              
         BNL   WKL6                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         MVC   WORK(6),WORK+6                                                   
         LA    R2,3(R2)                                                         
         BCT   R0,WKL4                                                          
         B     XIT                                                              
         SPACE 1                                                                
*                                  USE END DATE FOR LAST WEEK                   
WKL6     GOTO1 DATCON,DMCB,(0,EBCEND),(1,0(R2))                                 
         B     XIT                                                              
         EJECT                                                                  
*              READ CHECK RECORDS                                               
         SPACE 3                                                                
READCHEK NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,CHEKHOOK                                                      
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
         MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         OI    TIFTUNIT,TIFTWRK    PROGRAM IS INTERESTED IN WORK UNIT           
         OI    TIFPDSN,TAPDSCAN    NO CANADIAN                                  
         OI    TIFPDSN,TAPDSCNL    NO CANCELS                                   
         OI    TIFPDPN,TAPDPBNP    NO BNP                                       
         OI    TIFPDPN,TAPDPCRD    NO CREDITS                                   
         OI    TIFPO3N,TAPDODUM    NO DUMMYS                                    
         CLI   GREYOPT,C'Y'        IF GREY OPTION SET,                          
         BNE   *+8                                                              
         OI    TIQFLAG2,TIQFPGRY   TELL SYSIO TO PASS GREY RECORDS              
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         B     CKEOF2                                                           
         SPACE 1                                                                
CHEKHOOK NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         AP    INPCOUNT,=P'1'                                                   
         L     R6,TIAREC                                                        
         ST    R6,ACHECK                                                        
         BAS   RE,PROCCHEK                                                      
         B     XIT                                                              
         EJECT                                                                  
*              COUNTS AT CHECK EOF                                              
         SPACE 3                                                                
CKEOF2   CLI   TRACOPT,C'Y'                                                     
         BNE   CKEOF4                                                           
         MVC   MYP(24),=C'CHECK READING COMPLETE  '                             
         BAS   RE,SPLAT                                                         
         MVC   MYP(24),=C'----------------------  '                             
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  (P6,INPCOUNT),(8,MYP)                                            
         MVC   MYP+9(13),=C'INPUT RECORDS'                                      
         BAS   RE,SPLAT                                                         
         EDIT  (P6,CHKCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'CHECKS PROCESSED'                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,SRTCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'SORTS PROCESSED '                                   
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
CKEOF4   ZAP   INPCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A CHECK RECORD                                           
         SPACE 3                                                                
*              INPUT               ACHECK=A(CHECK RECORD)                       
         SPACE 1                                                                
PROCCHEK NTR1                                                                   
         L     R4,ACHECK                                                        
         MVI   CHKSTAT,0                                                        
         BAS   RE,INMLTSTA         INITIALIZE FOR MULTI-STATE                   
         SPACE 1                                                                
         LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         SPACE 1                                                                
PCC0     XC    SORTIO,SORTIO                                                    
         USING TLCKD,R4                                                         
         BAS   RE,PRMLTSTA         PROCESS FIRST/NEXT STATE                     
         BNE   XIT                                                              
         SPACE 1                                                                
         LR    R6,R4                                                            
         MVI   ELCODE,TACDELQ      MUST BE CHECK DETAILS                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACDD,R6                                                         
         CLI   CORPOPT,C'Y'        IF CORP                                      
         BNE   PCC1                                                             
         OC    TACDNTAX,TACDNTAX   MUST BE NON-TAXABLE EARNINGS                 
         BZ    XIT                                                              
         CLC   TLCKSORT,HEXFFS     AND NOT UNION CHECK                          
         BNE   *+14                                                             
         CLC   TLCKCAT,=C'ZZZ'                                                  
         BE    XIT                                                              
         B     *+14                                                             
PCC1     OC    TACDEARN,TACDEARN   ELSE, MUST BE EARNINGS                       
         BZ    XIT                                                              
         SPACE 1                                                                
         LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ      MUST BE PAY DETAILS                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         CLI   CORPOPT,C'Y'        IF PROCESSING CORPS ONLY                     
         BNE   PCC1A                                                            
         CLI   TAPDW4TY,TAW4TYCO   JUST LOOK FOR CORPS                          
         BE    PCC1B                                                            
         CLI   TAPDW4TY,TAW4TYCA   AND CANADIANS                                
         BE    PCC1B                                                            
         B     XIT                                                              
         SPACE 1                                                                
PCC1A    CLI   TAPDW4TY,TAW4TYCO   ELSE, NO CORPS                               
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYFO         OR FOREIGN                             
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYCA         OR CANADIANS                           
         BE    XIT                                                              
         SPACE 1                                                                
PCC1B    AP    CHKCOUNT,=P'1'                                                   
         MVC   SRTSSN,TISSN                                                     
         MVC   SRTEMP,TIEMP                                                     
         MVC   SRTSTATE,TIUNIT                                                  
         OC    TIUNIT,TIUNIT       IF NO UNIT                                   
         BNZ   *+14                                                             
         MVC   SRTSTATE,=C'NY '    DEFAULT TO NY                                
         B     PCC1D                                                            
         SPACE 1                                                                
         GOTO1 TAXVAL,DMCB,(3,TIUNIT)  LOOK UP ITS STATE                        
         BNE   XIT                     DONT PROCESS INVALID STATES              
         CLI   TIUNIT+2,C' '           IF WORK UNIT IS CITY                     
         BNH   PCC1D                                                            
*****    GOTO1 TAXVAL,DMCB,(3,TIUNIT)  LOOK UP ITS STATE                        
*****    BNE   XIT                     DONT PROCESS INVALID STATES              
         MVC   SRTSTATE,TGTASTCY       AND USE IT                               
PCC1D    LR    R6,R4                                                            
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PCC2     BAS   RE,NEXTEL                                                        
         BNE   PCCEND                                                           
         CLI   0(R6),TACAELQ                                                    
         BE    PCCCA                                                            
         CLI   0(R6),TACDELQ                                                    
         BE    PCCCD                                                            
         CLI   0(R6),TAPDELQ                                                    
         BE    PCCPD                                                            
         CLI   0(R6),TACWELQ                                                    
         BE    PCCCW                                                            
         CLI   0(R6),TATIELQ                                                    
         BE    PCCTI                                                            
         B     PCC2                                                             
         SPACE 1                                                                
         USING TACAD,R6                                                         
PCCCA    CLC   TACAONOF(2),=C'ON'  NOTE IF ON CAMERA                            
         BNE   PCC2                                                             
         OI    SRTINDS,SRTION                                                   
         B     PCC2                                                             
         SPACE 1                                                                
         USING TACDD,R6                                                         
PCCCD    L     R1,TACDEARN         PICK UP EARNINGS                             
         CLI   CORPOPT,C'Y'                                                     
         BNE   *+8                                                              
         L     R1,TACDNTAX                                                      
         CVD   R1,DUB                                                           
         ZAP   SRTGROSS,DUB                                                     
         LA    R1,1                                                             
         LA    R2,WEEKLIST         LOOK UP WEEK NUMBER                          
         LA    R0,54                                                            
         SPACE 1                                                                
PCCCD2   STC   R1,SRTWEEK                                                       
         CLC   TACDDTE,0(R2)                                                    
         BNH   PCC2                                                             
         LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,PCCCD2                                                        
         B     PCC2                                                             
         SPACE 1                                                                
         USING TAPDD,R6                                                         
PCCPD    OC    TAPDEOR,TAPDEOR                                                  
         BZ    *+10                                                             
         MVC   SRTEMP,TAPDEOR                                                   
         OC    TAPDUSE,TAPDUSE     IF NO USE                                    
         BZ    PCCPDX              CONSIDER IT REUSE                            
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         TM    TGUSSTA2,LIVEWORK   CHECK FOR LIVE WORK                          
         BNO   *+8                                                              
         OI    SRTINDS,SRTILIVE                                                 
PCCPDX   B     PCC2                                                             
         SPACE 1                                                                
         USING TACWD,R6                                                         
PCCCW    B     PCC2                IGNORED FOR NOW                              
         SPACE 1                                                                
         CLI   TACWUNIT+2,C' '     LOOK FOR STATE                               
         BH    PCC2                                                             
         CLC   TACWUNIT,=C'FD '                                                 
         BE    PCC2                                                             
         CLC   TACWUNIT,=C'CN '    ASSUME CANADA IS TAXABLE                     
         BNE   PCCCW2                                                           
         CLI   SRTSTATE,C'9'       UNLESS PREVIOUS TAXABLE FOUND                
         BNE   PCC2                                                             
         MVC   SRTSTATE,TACWUNIT                                                
         B     PCC2                                                             
         SPACE 1                                                                
PCCCW2   TM    TACWSTAT,TACWSWRK   WORKING                                      
         BNO   PCC2                                                             
         MVC   SRTSTATE,TACWUNIT   PICK UP WORK STATE                           
         B     PCC2                                                             
         SPACE 1                                                                
         USING TATID,R6                                                         
PCCTI    CLI   TATITYPE,TATITYCO   MAY USE CORP SS#                             
         BNE   PCC2                                                             
         MVC   SRTSSN,TATIID                                                    
         B     PCC2                                                             
         SPACE 1                                                                
PCCEND   CLI   SRTSTATE,C'9'                                                    
         BE    PCCEND2                                                          
         B     PCCEND1             IGNORED FOR NOW                              
         SPACE 1                                                                
         CLI   TIFUNIT,0           FILTER ON STATE                              
         BE    PCCEND1                                                          
         CLC   SRTSTATE,TIFUNIT                                                 
         BNE   PCCEND2                                                          
         SPACE 1                                                                
PCCEND1  BAS   RE,SORTPUT                                                       
PCCEND2  TM    CHKSTAT,CSMLTSTA                                                 
         BO    PCC0                                                             
         B     XIT                                                              
         EJECT                                                                  
*              IF CHECK IS FOR MULTIPLE STATES, PREPARE TO HANDLE               
*              ON ENTRY ... R4 = A(CHECK RECORD)                                
                                                                                
INMLTSTA NTR1                                                                   
         LR    R6,R4                                                            
         MVI   ELCODE,TATUELQ      EXIT IF CHECK HAS NO TAX UNIT                
         BAS   RE,GETEL            ELEMENTS                                     
         BNE   XIT                                                              
                                                                                
         USING TATUD,R6                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TATUELQ      SET TO NOT PROCESS THE CITY                  
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
IMS10    BAS   RE,NEXTEL                                                        
         BNE   IMS20                                                            
         CLC   TATUUNIT,=C'FD '                                                 
         BE    IMS15                                                            
         CLC   TATUUNIT,=C'CN '                                                 
         BE    IMS15                                                            
         TM    TATUSTAT,TATUSRES    DONT PROCESS RESIDENT STATE                 
         BO    IMS15                                                            
****     GOTO1 TAXVAL,DMCB,(3,TATUUNIT) LOOK UP ITS STATE                       
***      BNE   IMS15                                                            
         CLI   TATUUNIT+2,C' '                                                  
         BE    IMS10                                                            
IMS15    MVI   TATUEL,X'FF'                                                     
         B     IMS10                                                            
         DROP  R6                                                               
                                                                                
IMS20    LR    R6,R4               AT THIS POINT, EXIST IF CHECK HAS            
         MVI   ELCODE,TATUELQ      ONLY ONE TAX UNIT ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   XIT                 NO STATES - ONLY CITIES (LEAVE)              
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
                                                                                
         OI    CHKSTAT,CSMLTSTA    SET MULTI-STATE STATUS                       
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*              IF CHECK IS FOR MULTIPLE STATES, SET CHECK VARIABLES             
*              BASED ON STATE BEING PROCESSED                                   
*              ON ENTRY ... R4=A(CHECK RECORD)                                  
                                                                                
PRMLTSTA NTR1                                                                   
         TM    CHKSTAT,CSMLTSTA    IF THIS IS A MULTI-STATE CHECK               
         BZ    YES                                                              
                                                                                
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,TATUELQ      GET CHECK'S FIRST/NEXT TAX UNIT              
         BAS   RE,GETEL            ELEMENT                                      
         BNE   NO                                                               
                                                                                
         USING TATUD,R3                                                         
         LR    R3,R6                                                            
         MVC   TIUNIT,TATUUNIT     SET TIUNIT                                   
         MVI   TATUEL,X'FF'        SET TO NOT PROCESS ELEMENT AGAIN             
                                                                                
         USING TACDD,R6                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TACDEARN,TACDEARN                                                
         JZ    PMS10                                                            
***      MVC   TACDEARN,TATUWAGE   SAVE STATE WAGES OVER CHECK WAGES            
         MVC   TACDEARN,TATUWAAD   SAVE STATE WAGES OVER CHECK WAGES            
         B     YES                                                              
                                                                                
**PMS10    ICM   RE,15,TATUWAGE                                                 
PMS10    ICM   RE,15,TATUWAAD                                                   
         ICM   RF,15,TATUTNWA                                                   
         AR    RE,RF                                                            
         ICM   RF,15,TATUNNWA                                                   
         AR    RE,RF                                                            
         STCM  RE,15,TACDNTAX                                                   
         B     YES                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              CONTROL REPORTS ETC                                              
         SPACE 3                                                                
DOREPS   NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         BAS   RE,SETPAGE                                                       
         BAS   RE,SORTRECS                                                      
         B     XIT                                                              
         EJECT                                                                  
*              SORT THE WC CHECKS RECORDS AND                                   
*              COMBINE THOSE FOR SAME EMP/STATE/SS#                             
         SPACE 3                                                                
SORTRECS NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         ZAP   SRTCOUNT,=P'0'                                                   
         XC    SORTIO,SORTIO                                                    
         BAS   RE,CLDETS           CLEAR DETAILS                                
         BAS   RE,CLSTA                  AND STATE TOTALS                       
         BAS   RE,SUMCLEAR               AND SUMMARY BUFFERS                    
         LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         SPACE 1                                                                
SORT2    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   SORT4                                                            
         BAS   RE,SSNEND                                                        
         BAS   RE,STATEND                                                       
         BAS   RE,SUMMARY                                                       
         B     XIT                                                              
         SPACE 1                                                                
SORT4    LR    R6,R2                                                            
         MVC   RECTYPE,=CL16'SORT INPUT'                                        
         AP    SRTCOUNT,=P'1'                                                   
         BAS   RE,SORTRACE                                                      
         OC    SORTIO,SORTIO                                                    
         BZ    SORT20                                                           
*                                  R5=A(SAVED RECORD IN SORTIO)                 
*                                  R2=A(NEW RECORD FROM SORT)                   
         CLC   SRTEMP,0(R2)        CHECK CONTROL BREAKS                         
         BNE   SORT6                                                            
         CLC   SRTSTATE,3(R2)                                                   
         BNE   SORT8                                                            
         CLC   SRTSSN,SRTSSN-SRTD(R2)                                           
         BNE   SORT10                                                           
         MVC   SORTIO,0(R2)                                                     
         BAS   RE,ADDETS           ADD INTO SS# DETAILS                         
         B     SORT2                                                            
         SPACE 1                                                                
SORT6    BAS   RE,SSNEND           DIFFERENT EMPLOYER                           
         BAS   RE,STATEND                                                       
         BAS   RE,SUMMARY                                                       
         B     SORT20                                                           
         SPACE 1                                                                
SORT8    BAS   RE,SSNEND           DIFFERENT STATE                              
         BAS   RE,STATEND                                                       
         B     SORT20                                                           
         SPACE 1                                                                
SORT10   BAS   RE,SSNEND           DIFFERENT SS#                                
         SPACE 1                                                                
SORT20   MVC   SORTIO,0(R2)        NEW ENTRY                                    
         MVC   THISEMP,SRTEMP                                                   
         GOTO1 TAXVAL,DMCB,(3,SRTSTATE)                                         
         MVC   THISSTA,TGTANAME                                                 
         BAS   RE,W4DETS           (NEED SOME W4 STUFF)                         
         MVI   SSNON,C'N'                                                       
         TM    SRTINDS,SRTION      FIRST ENTRY FOR SS# ESTABLISHES              
         BNO   *+8                 IF PERFORMER IS ON CAMERA                    
         MVI   SSNON,C'Y'                                                       
         BAS   RE,ADDETS           ADD INTO SS# DETAILS                         
         B     SORT2                                                            
         EJECT                                                                  
*              NEED SOME W4 INFO FOR THIS SS#                                   
         SPACE 3                                                                
*              INPUT               RECORD IS IN SORTIO                          
         SPACE 1                                                                
W4DETS   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         LA    R2,SRTSSN                                                        
         MVC   THISNAME(16),=CL16'UNKNOWN'                                      
         MVC   THISNAME+16(16),=CL16'UNKNOWN'                                   
         BAS   RE,NEEDW4                                                        
         CLI   NEEDHIT,C'N'                                                     
         BE    *+10                                                             
         MVC   THISNAME,NEEDNAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              ADD DETAILS FOR THIS SORT RECORD                                 
         SPACE 3                                                                
*              INPUT               SORTIO                                       
         SPACE 1                                                                
ADDETS   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         AP    SSNGROSS,SRTGROSS                                                
         TM    SRTINDS,SRTILIVE    IF IT'S A REUSE PAYMENT                      
         BO    ADDETS2                                                          
         AP    SSNREUSE,SRTGROSS      ADD TO REUSE                              
         B     XIT                                                              
         SPACE 1                                                                
ADDETS2  AP    SSNSESS,SRTGROSS    ELSE ADD TO SESSION (TOTAL)                  
         ZIC   R1,SRTWEEK               AND ADD TO WEEK TOTAL                   
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   R1,3                                                             
         LA    R1,SSNSESS(R1)                                                   
         AP    0(8,R1),SRTGROSS                                                 
         SPACE 1                                                                
         ZIC   R1,SRTWEEK          PICK UP WEEK NUMBER                          
         BCTR  R1,0                                                             
         LA    R1,WEEKNOTE(R1)                                                  
         CLI   0(R1),1             ANY EARNINGS IN THAT WEEK YET?               
         BE    XIT                                                              
         MVI   0(R1),1                NO - SO NOTE EARNINGS                     
         L     R1,SSNWEEKS                 AND ADD TO WEEKS                     
         LA    R1,1(R1)                                                         
         ST    R1,SSNWEEKS                                                      
         B     XIT                                                              
         SPACE 1                                                                
CLSTA    NTR1                                                                   
         XC    STAWEEKS,STAWEEKS   CLEAR STATE ACCUMS                           
         LA    R1,STAGROSS                                                      
         LA    R0,112                                                           
         B     CLDETS2                                                          
         SPACE 1                                                                
CLDETS   NTR1                                                                   
         XC    SSNWEEKS,SSNWEEKS   CLEAR SS# ACCUMS                             
         XC    WEEKNOTE,WEEKNOTE                                                
         LA    R1,SSNGROSS                                                      
         LA    R0,112                                                           
         SPACE 1                                                                
CLDETS2  ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,CLDETS2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES AT END OF THIS SSN                                      
         SPACE 3                                                                
SSNEND   NTR1                                                                   
         MVI   REPTYPE,C'D'                                                     
         LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         BAS   RE,CALCWKS          CALCULATE NUMBER OF WEEKS WORKED             
         BAS   RE,ESTWC            ESTABLISH WORKERS COMP                       
         L     R1,SSNWEEKS                                                      
         BAS   RE,MAYSKIP                                                       
         LA    R2,MYP                                                           
         USING DETD,R2                                                          
         MVC   DETSSN(3),SRTSSN    FORMAT SSN TOTALS                            
         MVI   DETSSN+3,C'-'                                                    
         MVC   DETSSN+4(2),SRTSSN+3                                             
         MVI   DETSSN+6,C'-'                                                    
         MVC   DETSSN+7(4),SRTSSN+5                                             
         MVC   DETNAME(L'THISNAME),THISNAME                                     
         GOTO1 SQUASHER,DMCB,DETNAME,33                                         
         EDIT  (P8,SSNGROSS),(15,DETGROSS),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SSNREUSE),(15,DETREUSE),2,MINUS=YES,ZERO=BLANK,     X        
               COMMAS=YES                                                       
         LA    R3,SSNSESS+8        PRINT BREAKDOWN BY WEEK                      
         LA    R4,SSNWCTAX+8                                                    
         LA    R5,WEEKLIST                                                      
         LA    R6,54                                                            
         SPACE 1                                                                
SSNEND2  CP    0(8,R3),=P'0'                                                    
         BE    SSNEND4                                                          
         GOTO1 DATCON,DMCB,(1,0(R5)),(8,DETWEEK)                                
         EDIT  (P8,0(R3)),(15,DETSESS),2,MINUS=YES,COMMAS=YES                   
         EDIT  (P8,0(R4)),(15,DETWCTAX),2,MINUS=YES,COMMAS=YES                  
         CLI   DETOPT,C'N'         OPTION TO SUPPRESS DETAIL                    
         BE    *+8                                                              
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
SSNEND4  CLI   0(R5),0                                                          
         BE    SSNEND5                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,SSNEND2                                                       
         SPACE 1                                                                
SSNEND5  CP    NWEEKS,=P'1'                                                     
         BL    SSNEND6                                                          
         BE    SSNEND8                                                          
         EDIT  (P4,NWEEKS),(2,DETWEEK)                                          
         MVC   DETWEEK+3(5),=C'WEEKS'                                           
         EDIT  (P8,SSNSESS),(15,DETSESS),2,MINUS=YES,COMMAS=YES                 
         EDIT  (P8,SSNWCTAX),(15,DETWCTAX),2,MINUS=YES,COMMAS=YES               
         SPACE 1                                                                
SSNEND6  CLI   DETOPT,C'N'         OPTION TO SUPPRESS DETAIL                    
         BE    *+8                                                              
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
SSNEND8  BAS   RE,MYCLEAR                                                       
         BAS   RE,ADDSTATE         ADD PERSON TO STATE                          
         BAS   RE,SUMPOST          POST TO SUMMARY                              
         BAS   RE,CLDETS           AND CLEAR PERSON                             
         AP    WRKCOUNT,=P'1'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES NUMBER OF WEEKS WORKED                        
         SPACE 1                                                                
CALCWKS  NTR1                                                                   
         ZAP   NWEEKS,=P'0'                                                     
         LA    R3,SSNSESS+8                                                     
         LA    R5,WEEKLIST                                                      
         LA    R6,54                                                            
         SPACE 1                                                                
CALCWK5  CP    0(8,R3),=P'0'                                                    
         BE    *+10                                                             
         AP    NWEEKS,=P'1'                                                     
         SPACE 1                                                                
         CLI   0(R5),0                                                          
         BE    CALCWKX                                                          
         LA    R3,8(R3)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,CALCWK5                                                       
         SPACE 1                                                                
CALCWKX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ESTABLISH WORKERS COMP.                               
         SPACE 3                                                                
ESTWC    NTR1                                                                   
         CLI   TABLOPT,C'N'        IF PROCESSING NEW TABLE                      
         BNE   ESTWC1                                                           
         CLC   =C'CA ',SRTSTATE    AND THIS IS CALF.                            
         BNE   ESTWC1                                                           
         CP    NWEEKS,=P'1'        AND SSN WORKED MORE THAN A WEEK              
         BNH   ESTWC1                                                           
         BAS   RE,ESTWCTOT         SET WC BASED ON N'WKS WORKED                 
         B     XIT                                                              
         SPACE 1                                                                
ESTWC1   LA    R2,SSNSESS+8        R2=WEEK SESSION                              
         LA    R3,SSNWCTAX+8       R3=WC TAXABLE                                
         LA    R0,54               UP TO 54 WEEKS                               
         SPACE 1                                                                
ESTWC2   ZAP   DUB,0(8,R2)         WEEK'S TOTAL SESSION                         
         LA    R1,WCTABLE                                                       
         CLI   TABLOPT,C'O'        OLD OPTION                                   
         BNE   *+8                                                              
         LA    R1,OLDTABLE                                                      
         CLI   TABLOPT,C'Q'        NEW YORK QUARTERLY OPTION                    
         BNE   *+8                                                              
         LA    R1,NYQTABLE                                                      
         SPACE 1                                                                
ESTWC4   CLC   SRTSTATE,0(R1)      CHECK STATE MAX/MIN TABLE                    
         BE    ESTWC6                                                           
         CLI   0(R1),X'FF'                                                      
         BE    ESTWC10                                                          
         LA    R1,12(R1)                                                        
         B     ESTWC4                                                           
         SPACE 1                                                                
ESTWC6   CP    DUB,4(4,R1)         CHECK MAX                                    
         BL    ESTWC8                                                           
         ZAP   DUB,4(4,R1)                                                      
         B     ESTWC10                                                          
         SPACE 1                                                                
ESTWC8   CP    DUB,8(4,R1)         CHECK MIN                                    
         BH    ESTWC10                                                          
         ZAP   DUB,8(4,R1)                                                      
         SPACE 1                                                                
ESTWC10  AP    0(8,R3),DUB         ESTABLISH WEEK                               
         AP    SSNWCTAX,DUB        ADD TO TOTAL                                 
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,ESTWC2                                                        
         B     XIT                                                              
         EJECT                                                                  
ESTWCTOT NTR1                                                                   
         LA    R1,WCTABLE                                                       
ESTWCT2  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,R1),=C'CA '                                                  
         BE    ESTWCT5                                                          
         LA    R1,12(R1)                                                        
         B     ESTWCT2                                                          
*                                                                               
ESTWCT5  ZAP   CAMAX,NWEEKS        # OF WEEKS WORKED * MAX                      
         MP    CAMAX,4(4,R1)                                                    
         ZAP   CAMIN,NWEEKS        # OF WEEKS WORKED * MIN                      
         MP    CAMIN,8(4,R1)                                                    
*                                                                               
         CP    SSNSESS,CAMAX       CHECK TOT EARN AGAINST TOT MAX/MIN           
         BL    *+14                                                             
         ZAP   SSNWCTAX,CAMAX                                                   
         B     XIT                                                              
*                                                                               
         CP    SSNSESS,CAMIN                                                    
         BH    *+14                                                             
         ZAP   SSNWCTAX,CAMIN                                                   
         B     XIT                                                              
*                                                                               
         ZAP   SSNWCTAX,SSNSESS                                                 
         B     XIT                                                              
         EJECT                                                                  
WCTABLE  DS    0D                                                               
         DC    C'CA ',AL1(0),PL4'157500',PL4'-157500'  01/28/04                 
         DC    C'NJ ',AL1(0),PL4'182000',PL4'-182000'  01/28/04                 
         DC    C'NY ',AL1(0),PL4'382500',PL4'-382500'  01/28/04                 
         DC    X'FF'                                                            
*                                                                               
OLDTABLE DS    0D                                                               
         DC    C'CA ',AL1(0),PL4'152500',PL4'-152500'  03/31/04                 
         DC    C'NJ ',AL1(0),PL4'179000',PL4'-179000'  03/31/04                 
         DC    C'NY ',AL1(0),PL4'382500',PL4'-382500'  03/31/04                 
         DC    X'FF'                                                            
*                                                                               
         DC    C'CA ',AL1(0),PL4'142500',PL4'-142500'  10/25/02                 
         DC    C'NJ ',AL1(0),PL4'161000',PL4'-161000'  10/25/02                 
         DC    C'NY ',AL1(0),PL4'342500',PL4'-345500'  10/25/02                 
         DC    X'FF'                                                            
*                                                                               
         DC    C'CA ',AL1(0),PL4'137500',PL4'-137500'  03/13/01                 
         DC    C'NJ ',AL1(0),PL4'153000',PL4'-153000'  03/13/01                 
         DC    C'NY ',AL1(0),PL4'332500',PL4'-332500'  03/13/01                 
         DC    X'FF'                                                            
*                                                                               
         DC    C'CA ',AL1(0),PL4'132500',PL4'-132500'  03/24/00                 
         DC    C'NJ ',AL1(0),PL4'146000',PL4'-146000'  03/24/00                 
         DC    C'NY ',AL1(0),PL4'297500',PL4'-297500'  03/24/00                 
         DC    X'FF'                                                            
*                                                                               
         DC    C'CA ',AL1(0),PL4'130000',PL4'-130000'  05/04/99                 
         DC    C'NJ ',AL1(0),PL4'140000',PL4'-140000'  05/04/99                 
         DC    C'NY ',AL1(0),PL4'287500',PL4'-287500'  05/04/99                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         DC    C'CA ',AL1(0),PL4'112500',PL4'-112500'  03/20/98                 
         DC    C'NJ ',AL1(0),PL4'130000',PL4'-130000'  03/20/98                 
         DC    C'NY ',AL1(0),PL4'282500',PL4'-282500'  03/20/98                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         DC    C'CA ',AL1(0),PL4'120000',PL4'-120000'  03/06/96                 
         DC    C'MN ',AL1(0),PL4'97000',PL4'-97000'    01/19/95                 
         DC    C'NJ ',AL1(0),PL4'125000',PL4'-125000'  03/06/96                 
         DC    C'NY ',AL1(0),PL4'265000',PL4'-265000'  03/06/96                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         DC    C'CA ',AL1(0),PL4'117500',PL4'-117500'  01/19/95                 
         DC    C'MN ',AL1(0),PL4'97000',PL4'-97000'    01/19/95                 
         DC    C'NJ ',AL1(0),PL4'110000',PL4'-110000'  05/20/93                 
         DC    C'NY ',AL1(0),PL4'250000',PL4'-250000'  03/09/94                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         DC    C'CA ',AL1(0),PL4'115000',PL4'-115000'  03/09/94                 
         DC    C'MN ',AL1(0),PL4'30000',PL4'-30000'                             
         DC    C'NJ ',AL1(0),PL4'110000',PL4'-110000'  05/20/93                 
         DC    C'NY ',AL1(0),PL4'250000',PL4'-250000'  03/09/94                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'CA ',AL1(0),PL4'110000',PL4'-110000'                           
         DC    C'MN ',AL1(0),PL4'30000',PL4'-30000'    10/24/91                 
         DC    C'NJ ',AL1(0),PL4'105000',PL4'-105000'  5/20/93                  
         DC    C'NY ',AL1(0),PL4'230000',PL4'-230000'  10/24/91                 
         DC    X'FF'                                                            
         SPACE 1                                                                
NYQTABLE DS    0D                                                               
         DC    C'NY ',AL1(0),PL4'34000',PL4'-34000'                             
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINES AT END OF THIS EMPLOYER/STATE                           
         SPACE 1                                                                
STATEND  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         CLI   DETOPT,C'N'         OPTION TO SUPPRESS DETAIL                    
         BE    STAEND1                                                          
         CLI   LINE,40                                                          
         BL    *+8                                                              
         BAS   RE,SETSKIP                                                       
         BAS   RE,SPLAT                                                         
STAEND1  LA    R5,SORTIO                                                        
         USING SRTD,R5                                                          
         LA    R2,MYP                                                           
         USING DETD,R2                                                          
         CLI   DETOPT,C'N'         OPTION TO SUPPRESS DETAIL                    
         BE    STAEND6                                                          
         MVC   DETNAME(23),=C'*** TOTALS FOR ********'                          
         MVC   DETNAME+15(8),THISSTA                                            
         EDIT  (P8,STAGROSS),(15,DETGROSS),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,STAREUSE),(15,DETREUSE),2,MINUS=YES,COMMAS=YES               
         LA    R3,STASESS+8                                                     
         LA    R4,STAWCTAX+8                                                    
         LA    R5,WEEKLIST                                                      
         LA    R6,54                                                            
         SPACE 1                                                                
STAEND2  CP    0(8,R3),=P'0'                                                    
         BE    STAEND4                                                          
         GOTO1 DATCON,DMCB,(1,0(R5)),(8,DETWEEK)                                
         EDIT  (P8,0(R3)),(15,DETSESS),2,MINUS=YES,COMMAS=YES                   
         EDIT  (P8,0(R4)),(15,DETWCTAX),2,MINUS=YES,COMMAS=YES                  
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
STAEND4  CLI   0(R5),0                                                          
         BE    STAEND5                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,STAEND2                                                       
*****    CLC   STAWEEKS,=F'2'                                                   
*****    BL    STAEND6                                                          
*****    EDIT  (4,STAWEEKS),(3,DETWEEK)                                         
*****    MVC   DETWEEK+3(5),=C'WEEKS'                                           
STAEND5  EDIT  (P8,STASESS),(15,DETSESS),2,MINUS=YES,COMMAS=YES                 
         EDIT  (P8,STAWCTAX),(15,DETWCTAX),2,MINUS=YES,COMMAS=YES               
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
STAEND6  EDIT  (P8,WRKCOUNT),(7,DETNAME)                                        
         MVC   DETNAME+8(7),=C'WORKERS'                                         
         BAS   RE,SPLAT            PRINT PEOPLE COUNT                           
         ZAP   WRKCOUNT,=P'0'                                                   
         BAS   RE,CLSTA            CLEAR STATE                                  
         BAS   RE,SETSKIP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE  TO PRINT A SUMMARY OF STATES                            
         SPACE 3                                                                
SUMMARY  NTR1                                                                   
         OC    NSTATES,NSTATES     NOTHING TO DO IF NO STATES                   
         BZ    XIT                                                              
         MVI   REPTYPE,C'S'                                                     
         BAS   RE,SETSKIP                                                       
         L     R2,=A(SUMBSTA)      SORT STATES INTO ALPHA SEQ.                  
         L     R3,NSTATES                                                       
         LA    R4,SUMLENT                                                       
         GOTO1 XSORT,DMCB,(R2),(R3),(R4),8,0                                    
         SPACE 1                                                                
         L     R2,=A(SUMBSTA)      NOW PRINT STATES                             
         USING SUMD,R2                                                          
         LA    R3,MYP                                                           
         USING SPLD,R3                                                          
         L     R4,NSTATES                                                       
         SPACE 1                                                                
SUMM2    MVC   SPLDESC+2(8),SUMSTA                                              
         BAS   RE,SUMLINE                                                       
         LA    R2,SUMLENT(R2)                                                   
         BCT   R4,SUMM2                                                         
         SPACE 1                                                                
         L     R2,=A(SUMBLCA)      LESS CA TOTALS                               
         CLI   SUMSTA,0                                                         
         BE    SUMM4                                                            
         MVC   SPLDESC(10),=C'ALL STATES'                                       
         MVC   SPLDESC+132(10),=C'LESS CALIF'                                   
         BAS   RE,SUMLINE                                                       
         SPACE 1                                                                
SUMM4    L     R2,=A(SUMBOUSA)     OUTSIDE US TOTALS                            
         CLI   SUMSTA,0                                                         
         BE    SUMM6                                                            
         MVC   SPLDESC(10),=C'OUTSIDE US'                                       
         BAS   RE,SUMLINE                                                       
         SPACE 1                                                                
SUMM6    L     R2,=A(SUMBALL)      ALL STATES                                   
         MVC   SPLDESC(10),=C'ALL STATES'                                       
         BAS   RE,SUMLINE                                                       
         BAS   RE,SUMCLEAR                                                      
         BAS   RE,SETSKIP                                                       
         B     XIT                                                              
         SPACE 1                                                                
SUMLINE  NTR1                                                                   
         LA    R1,4                                                             
         BAS   RE,MAYSKIP                                                       
         LA    R3,MYP                                                           
         MVC   SPLDESC+12(10),=C'     TOTAL'                                    
         EDIT  (P8,SUMTOGRS),(15,SPLGROSS),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMTOREU),(15,SPLREUSE),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMTOSES),(15,SPLSESS),2,MINUS=YES,COMMAS=YES                
         EDIT  (P8,SUMTOSWC),(15,SPLWCTAX),2,MINUS=YES,COMMAS=YES               
         SPACE 1                                                                
         LA    R3,MYP2                                                          
         MVC   SPLDESC+12(10),=C' ON CAMERA'                                    
         EDIT  (P8,SUMONGRS),(15,SPLGROSS),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMONREU),(15,SPLREUSE),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMONSES),(15,SPLSESS),2,MINUS=YES,COMMAS=YES                
         EDIT  (P8,SUMONSWC),(15,SPLWCTAX),2,MINUS=YES,COMMAS=YES               
         SPACE 1                                                                
         LA    R3,MYP3                                                          
         MVC   SPLDESC+12(10),=C'OFF CAMERA'                                    
         EDIT  (P8,SUMOFGRS),(15,SPLGROSS),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMOFREU),(15,SPLREUSE),2,MINUS=YES,COMMAS=YES               
         EDIT  (P8,SUMOFSES),(15,SPLSESS),2,MINUS=YES,COMMAS=YES                
         EDIT  (P8,SUMOFSWC),(15,SPLWCTAX),2,MINUS=YES,COMMAS=YES               
         SPACE 1                                                                
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SUMCLEAR NTR1                                                                   
         XC    NSTATES,NSTATES                                                  
         L     R2,=A(SUMBSTA)      CLEAR SUMMARY BUFFER                         
         USING SUMD,R2                                                          
         LA    R0,STABMAX          STATION TABLE MAXIMUM                        
         SPACE 1                                                                
SUMCL2   XC    SUMSTA,SUMSTA                                                    
         LA    R3,SUMTOGRS                                                      
         LA    R4,12                                                            
         SPACE 1                                                                
SUMCL4   ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,SUMCL4                                                        
         LA    R2,SUMLENT(R2)                                                   
         BCT   R0,SUMCL2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              POST TO SUMMARY BUFFERS                                          
         SPACE 3                                                                
SUMPOST  NTR1                                                                   
         L     R2,=A(SUMBSTA)      POST TO STATE SUMMARY                        
         USING SUMD,R2                                                          
         LA    R0,MAXSTA                                                        
         SPACE 1                                                                
SUMPOST2 CLC   SUMSTA,THISSTA      LOOK FOR MATCH ON STATE                      
         BE    SUMPOST6                                                         
         OC    SUMSTA,SUMSTA                                                    
         BZ    SUMPOST4                                                         
         LA    R2,SUMLENT(R2)                                                   
         BCT   R0,SUMPOST2                                                      
         DC    H'0'                RAN OUT OF ROOM IN TABLE                     
         SPACE 1                                                                
SUMPOST4 MVC   SUMSTA,THISSTA      NEW ENTRY                                    
         L     R1,NSTATES                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NSTATES                                                       
         SPACE 1                                                                
SUMPOST6 BAS   RE,POSTLINE         POST STATE                                   
         L     R2,=A(SUMBLCA)                                                   
         CLC   THISSTA(5),=C'CALIF'                                             
         BE    SUMPOST8                                                         
         MVC   SUMSTA,=C'-CALIF  '                                              
         BAS   RE,POSTLINE         POST LESS CALIFORNIA                         
         SPACE 1                                                                
SUMPOST8 L     R2,=A(SUMBALL)                                                   
         MVC   SUMSTA,=C'*TOTALS*'                                              
         BAS   RE,POSTLINE         POST 'ALL'                                   
         B     XIT                                                              
         SPACE 1                                                                
*                                  ADD TO LINE IN BUFFER                        
*              INPUT               R2=A(LINE)                                   
         SPACE 1                                                                
POSTLINE NTR1                                                                   
         AP    SUMTOGRS,SSNGROSS   ADD TO TOTAL                                 
         AP    SUMTOREU,SSNREUSE                                                
         AP    SUMTOSES,SSNSESS                                                 
         AP    SUMTOSWC,SSNWCTAX                                                
         CLI   SSNON,C'Y'          ON CAMERA?                                   
         BE    POSTON                                                           
         AP    SUMOFGRS,SSNGROSS   ADD TO OFF CAMERA                            
         AP    SUMOFREU,SSNREUSE                                                
         AP    SUMOFSES,SSNSESS                                                 
         AP    SUMOFSWC,SSNWCTAX                                                
         B     XIT                                                              
         SPACE 1                                                                
POSTON   AP    SUMONGRS,SSNGROSS   ADD TO ON CAMERA                             
         AP    SUMONREU,SSNREUSE                                                
         AP    SUMONSES,SSNSESS                                                 
         AP    SUMONSWC,SSNWCTAX                                                
         B     XIT                                                              
         EJECT                                                                  
*              ADD FROM PERSON TO STATE AND CLEAR PERSON                        
         SPACE 3                                                                
ADDSTATE NTR1                                                                   
         L     R1,SSNWEEKS                                                      
         A     R1,STAWEEKS                                                      
         ST    R1,STAWEEKS                                                      
         SPACE 1                                                                
         LA    R2,SSNGROSS                                                      
         LA    R3,STAGROSS                                                      
         LA    R0,112                                                           
         SPACE 1                                                                
ADDSTA2  AP    0(8,R3),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,ADDSTA2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
*              INPUT               R2=A(EMP)                                    
         SPACE 1                                                                
NEEDEM   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
*              INPUT               R2=A(SS#)                                    
         SPACE 1                                                                
NEEDW4   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'100'     SET UP FOR 100 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,100                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEF                                                                   
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'400016'           (100*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDSHRT DS    CL16                                                             
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(32),TAW4CRPN                                            
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   NEEDSHRT,MYSPACES                                                
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'           R6=A(INPUT RECORD)                           
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         AP    SRTCOUNT,=P'1'                                                   
         LA    R6,SORTIO                                                        
         MVC   RECTYPE,=CL16'SORT OUTPUT'                                       
         BAS   RE,SORTRACE                                                      
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         SPACE 1                                                                
SORTRACE NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         CP    SRTCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,LSORT            L'SORT                                       
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(32)'                                  
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SETSKIP  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
MAYSKIP  NTR1                                                                   
         L     R5,ASPOOLD          R1=N'LINES TO FIT                            
         USING SPOOLD,R5                                                        
         ZIC   R0,LINE                                                          
         AR    R1,0                                                             
         CH    R1,=H'57'                                                        
         BL    XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SETPAGE  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   PAGE,=H'1'                                                       
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RB,MYRB                                                          
         L     R7,MYR7                                                          
         L     R8,MYR8                                                          
         DROP  RF                                                               
         USING T70316,RB,R7,R8                                                  
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   H1+48(32),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+48,32                                             
         GOTO1 UNDERLIN,DMCB,(32,H1+48),(X'BF',H2+48)                           
         CLI   THISEMP,0                                                        
         BE    HOOK2                                                            
         LA    R3,H3                                                            
         MVC   0(20,R3),MYSPACES                                                
         MVC   0(8,R3),=C'EMPLOYER'                                             
         MVC   9(3,R3),THISEMP                                                  
         LA    R2,THISEMP                                                       
         BAS   RE,NEEDEM                                                        
         MVC   13(30,R3),NEEDNAME                                               
         MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERH                                                         
         MVC   H3+59(17),8(R1)                                                  
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
HOOK2    CLI   REPTYPE,C'S'                                                     
         BE    HOOKSUM                                                          
         MVC   H4(5),=C'STATE'                                                  
         MVC   H4+9(8),THISSTA                                                  
         L     R1,=A(MYH6)                                                      
         MVC   H6,0(R1)                                                         
         L     R1,=A(MYH7)                                                      
         MVC   H7,0(R1)                                                         
         L     R1,=A(MYCOLS)                                                    
         MVC   BOXCOLS,0(R1)                                                    
         B     HKX                                                              
         SPACE 1                                                                
HOOKSUM  MVC   H4(7),=C'SUMMARY'                                                
         L     R1,=A(SUMH6)                                                     
         MVC   H6,0(R1)                                                         
         L     R1,=A(SUMH7)                                                     
         MVC   H7,0(R1)                                                         
         L     R1,=A(SUMCOLS)                                                   
         MVC   BOXCOLS,0(R1)                                                    
         DROP  R2                                                               
         DROP  R5                                                               
HKX      XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
HEXFFS   DC    6X'FF'                                                           
WRKCOUNT DC    PL8'0'                                                           
         LTORG                                                                  
         SPACE 1                                                                
LSORT    EQU   32                                                               
         EJECT                                                                  
*              HEADINGS, BOXES FOR DETAILS REPORT                               
         SPACE 3                                                                
MYH6     DS    0C                                                               
         DC    CL06' '                                                          
         DC    CL01' '                                                          
         DC    CL11'  SOCIAL'                                                   
         DC    CL01' '                                                          
         DC    CL33'PERFORMER NAME'                                             
         DC    CL01' '                                                          
         DC    CL15'    GROSS'                                                  
         DC    CL01' '                                                          
         DC    CL15'    REUSE'                                                  
         DC    CL01' '                                                          
         DC    CL08'  WEEK'                                                     
         DC    CL01' '                                                          
         DC    CL15'   SESSION'                                                 
         DC    CL01' '                                                          
         DC    CL15'  SUBJECT TO'                                               
         DC    CL01' '                                                          
         DC    CL06' '                                                          
         SPACE 1                                                                
MYH7     DS    0C                                                               
         DC    CL06' '                                                          
         DC    CL01' '                                                          
         DC    CL11' SECURITY'                                                  
         DC    CL01' '                                                          
         DC    CL33' '                                                          
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL08' ENDING'                                                    
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL15'  WORKERS COMP.'                                            
         DC    CL01' '                                                          
         DC    CL06' '                                                          
         SPACE 1                                                                
MYCOLS   DS    0C                                                               
         DC    CL06' '                                                          
         DC    CL01'L'                                                          
         DC    CL11' '                                                          
         DC    CL01'C'                                                          
         DC    CL33' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL08' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'R'                                                          
         DC    CL06' '                                                          
         EJECT                                                                  
*              HEADERS, BOXES FOR SUMMARY                                       
         SPACE 3                                                                
SUMH6    DS    0D                                                               
         DC    CL21' '                                                          
         DC    CL01' '                                                          
         DC    CL24'  STATE'                                                    
         DC    CL01' '                                                          
         DC    CL15'    GROSS'                                                  
         DC    CL01' '                                                          
         DC    CL15'    REUSE'                                                  
         DC    CL01' '                                                          
         DC    CL15'   SESSION'                                                 
         DC    CL01' '                                                          
         DC    CL15'  SUBJECT TO'                                               
         DC    CL01' '                                                          
         DC    CL21' '                                                          
         SPACE 1                                                                
SUMH7    DS    0D                                                               
         DC    CL21' '                                                          
         DC    CL01' '                                                          
         DC    CL24' '                                                          
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL15'    WAGES'                                                  
         DC    CL01' '                                                          
         DC    CL15'  WORKERS COMP.'                                            
         DC    CL01' '                                                          
         DC    CL21' '                                                          
         SPACE 1                                                                
SUMCOLS  DS    0D                                                               
         DC    CL21' '                                                          
         DC    CL01'L'                                                          
         DC    CL24' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'R'                                                          
         DC    CL21' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
         DS    0D                                                               
SUMBSTA  DS    CL(MAXSTA*104)                                                   
SUMBLCA  DS    CL104               LESS CALIFORNIA                              
SUMBOUSA DS    CL104               OUTSIDE USA                                  
         DS    CL104               SPARE                                        
SUMBALL  DS    CL104               ALL                                          
         SPACE 2                                                                
MAXSTA   EQU   80                  UP TO EIGHTY 'STATES'                        
STABMAX  EQU   MAXSTA+4            STATION TABLE MAXIMUM                        
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
MYSORTER DS    A                                                                
ACHECK   DS    A                                                                
APERH    DS    A                                                                
MYRB     DS    A                                                                
MYR7     DS    A                                                                
MYR8     DS    A                                                                
         SPACE 1                                                                
THISEMP  DS    CL3                                                              
THISSTA  DS    CL8                                                              
THISNAME DS    CL32                LAST, FIRST OR CORP NAME                     
REPTYPE  DS    CL1                                                              
NSTATES  DS    F                                                                
         SPACE 1                                                                
NWEEKS   DS    PL4                                                              
CAMAX    DS    PL8                                                              
CAMIN    DS    PL8                                                              
         SPACE 1                                                                
*                                  COUNTS                                       
TRACOUNT DS    PL6                                                              
TRALIMIT DS    PL6                                                              
RECCOUNT DS    PL6                                                              
RECLIMIT DS    PL6                                                              
REPCOUNT DS    PL6                                                              
REPLIMIT DS    PL6                                                              
CHKCOUNT DS    PL6                                                              
INVCOUNT DS    PL6                                                              
SRTCOUNT DS    PL6                                                              
INPCOUNT DS    PL6                                                              
         DS    PL6                                                              
         DS    PL6                                                              
         SPACE 1                                                                
SSNWEEKS DS    F                                                                
SSNGROSS DS    PL8                                                              
SSNREUSE DS    PL8                                                              
SSNSESS  DS    PL8                                                              
         DS    54PL8                                                            
SSNWCTAX DS    PL8                                                              
         DS    54PL8                                                            
         SPACE 1                                                                
STAWEEKS DS    F                                                                
STAGROSS DS    PL8                                                              
STAREUSE DS    PL8                                                              
STASESS  DS    PL8                                                              
         DS    54PL8                                                            
STAWCTAX DS    PL8                                                              
         DS    54PL8                                                            
*                                                                               
CHKSTAT  DS    XL1                                                              
CSMLTSTA EQU   X'80'               MULTI-STATE CHECK                            
         DS    CL4                 SPARE                                        
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 Y=TRACE                                      
GREYOPT  DS    CL1                                                              
DETOPT   DS    CL1                                                              
TABLOPT  DS    CL1                                                              
CORPOPT  DS    CL1                                                              
         SPACE 1                                                                
         DS    0D                                                               
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
PTODAY   DS    PL3                                                              
SAVEEL   DS    CL1                                                              
THISTYPE DS    CL1                                                              
SSNON    DS    CL1                 ON CAMERA? Y/N                               
WEEKLIST DS    CL163               54 WEEKS + 1                                 
WEEKNOTE DS    XL60                WEEK MARKERS FOR ADDING WEEKS                
EBCEND   DS    XL6                                                              
         SPACE 1                                                                
         DS    0D                                                               
SORTIO   DS    CL32                                                             
         DS    0D                                                               
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER SORT RECORDS FOR WORKERS COMP                     
         SPACE 3                                                                
SRTD     DSECT                                                                  
SRTREC   DS    0CL32               SORT RECORDS                                 
SRTKEY   DS    0CL16               KEY SECTION                                  
SRTEMP   DS    CL3                 EMPLOYER                                     
SRTSTATE DS    CL3                 TAXABLE STATE                                
SRTSSN   DS    CL9                 SS#                                          
SRTWEEK  DS    XL1                 WEEK NUMBER (PHASE 1)                        
SRTKLNQ  EQU   *-SRTKEY                                                         
         SPACE 1                                                                
SRTINDS  DS    XL1                 INDICATORS                                   
SRTILIVE EQU   X'80'               'LIVE' PAYMENT                               
SRTION   EQU   X'40'               ON CAMERA                                    
SRTGROSS DS    PL8                 GROSS EARNINGS                               
         DS    CL7                 SPARE                                        
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER SUMMARY BUFFER AREAS                              
         SPACE 3                                                                
SUMD     DSECT                                                                  
SUMSTA   DS    CL8                                                              
SUMTOGRS DS    PL8                 TOTAL GROSS                                  
SUMTOREU DS    PL8                       REUSE                                  
SUMTOSES DS    PL8                       SESSION                                
SUMTOSWC DS    PL8                       SUBJECT TO WORK COMP                   
SUMONGRS DS    PL8                 ON    GROSS                                  
SUMONREU DS    PL8                       REUSE                                  
SUMONSES DS    PL8                       SESSION                                
SUMONSWC DS    PL8                       SUBJECT TO WORK COMP                   
SUMOFGRS DS    PL8                 OFF   GROSS                                  
SUMOFREU DS    PL8                       REUSE                                  
SUMOFSES DS    PL8                       SESSION                                
SUMOFSWC DS    PL8                       SUBJECT TO WORK COMP                   
SUMLENT  EQU   *-SUMD                                                           
         EJECT                                                                  
*              DSECTS FOR PRINT LINES                                           
         SPACE 3                                                                
DETD     DSECT                                                                  
         DC    CL06' '                                                          
         DC    CL01' '                                                          
DETSSN   DC    CL11' '                                                          
         DC    CL01' '                                                          
DETNAME  DC    CL33' '                                                          
         DC    CL01' '                                                          
DETGROSS DC    CL15' '                                                          
         DC    CL01' '                                                          
DETREUSE DC    CL15' '                                                          
         DC    CL01' '                                                          
DETWEEK  DC    CL08' '                                                          
         DC    CL01' '                                                          
DETSESS  DC    CL15' '                                                          
         DC    CL01' '                                                          
DETWCTAX DC    CL15' '                                                          
         DC    CL01' '                                                          
         DC    CL06' '                                                          
         SPACE 1                                                                
SPLD     DSECT                                                                  
         DC    CL21' '                                                          
         DC    CL01' '                                                          
SPLDESC  DC    CL24' '                                                          
         DC    CL01' '                                                          
SPLGROSS DC    CL15' '                                                          
         DC    CL01' '                                                          
SPLREUSE DC    CL15' '                                                          
         DC    CL01' '                                                          
SPLSESS  DC    CL15' '                                                          
         DC    CL01' '                                                          
SPLWCTAX DC    CL15' '                                                          
         DC    CL01' '                                                          
         DC    CL24' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPEBD                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP16   11/18/14'                                      
         END                                                                    
