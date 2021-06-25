*          DATA SET ACREPLD02  AT LEVEL 224 AS OF 05/01/01                      
*PHASE ACLD02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*   READS COST PERSON RECS AND LOOK FOR TIMESHEET LOCK DATE (IN X'83')*         
*   (IN X'83' ELEMENT IT IS LOCLOCK) IF EXIST PRINT PERSON LOCSTART DT*         
*   LOCEND DATE, LOCK DATE AND OFFICE AND DEPARTMENT                  *         
*   READ CAL RECORD, GET PERIOD END DATE AND THEN READ TIME RECORD    *         
*   COMPARE TIME RECORD END DATE WITH PERIOD END DATE                 *         
*   QOPT1=C'Y' PRINT CAL START,END DATE. PERIOD END DATE.             *         
*   QOPT2=C'Y' PRINT PERSON DETAIL LINE ONLY IF IT HAS ERRORS IN IT   *         
*              TIME END DATE.                                         *         
***********************************************************************         
         TITLE 'TIME SHEET LOCK DATE CHECK'                                     
ACLD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACLD**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACLDD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST              REQUEST LAST                           
         BE    REQL                                                             
         CLI   MODE,RUNLAST              REQUEST LAST                           
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         MVC   VTYPES(VTYPLNQ),ADCONS    RELOCATE VTYPES                        
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
         ZAP   PKRECT,=P'0'       RECORD COUNTER WHOLE ACC FILE                 
         MVC   SVDISP,=F'0'                                                     
*                                                                               
         DROP  R2,R4,RE                                                         
RUNFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R4                                                        
REQF     DS    0H                                                               
         ZAP   PKCOUNT,=P'0'       RECORD COUNTER                               
         LA    R4,XP                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*****************************************************************               
* READ COMPANY RECORD USING REGISTER R5                                         
*****************************************************************               
         USING CPYRECD,R5                                                       
         LA    R5,SVKEY                                                         
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL                                                 
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQF20                                                           
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
*                                                                               
         USING CPYELD,R3                                                        
REQF20   DS    0H                                                               
         CLC   IOKEY(L'CPYKEY),SVKEY                                            
         BNE   REQFX                                                            
*                                                                               
         LA    R5,IO         SET R3 TO POINT TO THE RECORD                      
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         LA    R3,CPYRFST-CPYRECD(R5)                                           
REQF30   CLI   0(R3),0                                                          
         BE    REQF10                                                           
         CLI   0(R3),CPYELQ                                                     
         BE    REQF50                                                           
REQF40   ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     REQF30                                                           
REQF50   DS    0H                                                               
         TM    CPYSTAT7,CPYSTMSY   IS TIME MNGMNT SYS IN USE                    
         BNO   REQF10                                                           
         MVC   SVKEY1,0(R5)        SAVE COMPANY RECD KEY FOR NXT SEQ            
         DROP  R3,R5                                                            
***************************************************************                 
* READ PERSON RECORD X'0F' USING REGISTER R3                                    
***************************************************************                 
         USING PERRECD,R3                                                       
         LA    R3,SVKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F' - PERSON RECORD                        
         MVC   PERKCPY,RCCOMPFL                                                 
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQF70                                                           
*                                                                               
REQF60   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
REQF70   CLC   IOKEY(PERKCODE-PERKEY),SVKEY                                     
         BNE   REQF200                                                          
*                                                                               
         MVC   SVKEY2,0(R3)        SAVE PERSON RECD KEY FOR NXT SEQ             
         LA    R3,IO         SET R3 TO POINT TO THE RECORD                      
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         LA    RE,XP               CLEAR XP                                     
         LA    RF,L'XP                                                          
         LR    R1,RF                                                            
         LA    R0,EMPTIES                                                       
         MVCL  RE,R0                                                            
*                                                                               
         USING LOCELD,R2                                                        
         LA    R2,PERRFST-PERRECD(R3)                                           
*                                                                               
REQF75   CLC   SVDISP,=F'0'                                                     
         BE    REQF80                                                           
         LR    R2,R3                                                            
         A     R2,SVDISP                                                        
         B     REQF90                                                           
*                                                                               
REQF80   CLI   0(R2),0                                                          
         BE    REQF110                                                          
         CLI   0(R2),LOCELQ         X'83' STAFF LOCATION ELEM                   
         BE    REQF100                                                          
REQF90   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF80                                                           
REQF100  DS    0H                                                               
         OC    LOCLOCK,LOCLOCK     IS THEIR ANYTHING IN DATE                    
         BZ    REQF90                                                           
         CLC   LOCLOCK,=X'A00101'  IS DATE LOWER THAN JAN 1ST 2000              
         BL    REQF90                                                           
         CLC   LOCLOCK,LOCEND                                                   
         BNH   REQF90                                                           
*                                                                               
         LA    R1,IO                                                            
         SR    R2,R1                                                            
         ST    R2,SVDISP           SVDISPLACEMENT OF THE ELEMENT                
         AR    R2,R1                                                            
*                                                                               
         MVC   SVOFFICE,LOCOFF     SAVE OFFICE                                  
         MVC   SVWRKOFF,LOCOFF     SAVE OFFICE TO BUILD WORK AREA               
         MVC   SVDEPT,LOCDEPT      SAVE DEPARTMENT                              
         MVC   SVSUBDPT,LOCSUB     SAVE SUBDEPARTMENT                           
         MVC   SVKEY2,0(R3)        SAVE PERSON RECD KEY FOR NXT SEQ             
*                                                                               
         MVC   PPERSON,PERKCODE    FROM PERSON RECD KEY                         
         MVC   POFFICE,LOCOFF                                                   
         MVC   PDEPT,LOCDEPT                                                    
         MVC   PSUBDEPT,LOCSUB     MOVE IN SUB DEPT TO PLINE                    
         GOTO1 DATCON,DMCB,(1,LOCSTART),(8,PSTARTDT)                            
         GOTO1 DATCON,DMCB,(1,LOCEND),(8,PENDDT)                                
         GOTO1 DATCON,DMCB,(1,LOCLOCK),(8,PLOCDATE)                             
*                                                                               
         MVC   PDESC,SPACES                                                     
         MVC   SVLOCEND,LOCEND      SAVE LOCATION END DATE                      
         MVC   SVPERSON,PERKCODE    SAVE PERSON CODE                            
*                                                                               
         OC    LOCEND,LOCEND        IS ANYTHING IN LOCATION END DATE            
         BNZ   REQCST05                                                         
*        GOTO1 ACREPORT                                                         
*        AP    PKCOUNT,=P'1'        INC COMPANY TOTAL                           
*        AP    PKRECT,=P'1'         INC RECD TOTAL                              
         B     REQF90                                                           
*                                                                               
REQF110  DS    0H                                                               
         XC    SVDISP,SVDISP                                                    
         MVC   SVKEY,SVKEY2           REESTABLISH READ SEQ                      
         GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
         B     REQF60              GO READ SEQ FOR PERSON RECD                  
******************************************************************              
* READ COST CALENDAR PASSIVE POINTER WITH OFFICE IN KEY          *              
* CASPAS X'3E0C' RECORD   USING REGISTER R6                      *              
******************************************************************              
REQCST05 LA    R6,SVKEY                                                         
         USING CASRECD,R6                                                       
         XC    SVKEY,SVKEY                                                      
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,RCCOMPFL    COMPANY                                      
         MVC   CASPEDTE,SVLOCEND     LOCEND FROM PERSON RECORD                  
*        XC    CASPSDTE,CASPSDTE                                                
         MVC   CASPOFC,SVOFFICE    LOCOFF FROM PERSON RECORD                    
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQCST20                                                         
*                                                                               
REQCST10 GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
REQCST20 CLC   IOKEY(CASPEDTE-CASPAS),SVKEY                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TEMP     USING CASRECD,RF                                                       
         LA    RF,IOKEY                                                         
*                                                                               
         CLC   CASPEDTE,TEMP.CASPSDTE  IS DATE BETWEEN THIS PERIOD              
         BL    REQCST10                                                         
         CLC   CASPEDTE,TEMP.CASPEDTE                                           
         BH    REQCST10                                                         
         CLC   TEMP.CASPOFC,CASPOFC                                             
         BE    REQCST25                                                         
*                                                                               
REQCST21 CLC   CASPOFC,SPACES      ARE WE AT OFFICE LEVEL                       
         BNE   REQCST22            NO, READ NEXT PERSON RECORD                  
         B     REQCST10                                                         
*        MVC   PDESC,=CL30'CASRECD X"3E0C" DON"T EXIST'                         
*        B     REQCST90            NO, READ NEXT PERSON RECORD                  
REQCST22 MVC   SVOFFICE,SPACES     YES, CHECK AGENCY LEVEL                      
         B     REQCST05                                                         
*                                                                               
REQCST25 DS    0H                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   REQCST27                                                         
         GOTO1 DATCON,DMCB,(1,TEMP.CASPSDTE),(8,PCALSTRT)   CAL START           
         LA    RF,IOKEY            REESTABLISH RF                               
         GOTO1 DATCON,DMCB,(1,TEMP.CASPEDTE),(8,PCALEND)   CAL END              
         DROP  TEMP                                                             
REQCST27 LA    R6,IO         SET R3 TO POINT TO THE RECORD                      
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         USING TMPELD,R7                                                        
         LA    R7,CASRFST-CASRECD(R6)                                           
REQCST30 CLI   0(R7),0                                                          
         BE    REQCST10                                                         
         CLI   0(R7),TMPELQ         X'88' TIMESHEET PERIOD ELEM                 
         BE    REQCST50                                                         
REQCST40 ZIC   R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     REQCST30                                                         
REQCST50 DS    0H                                                               
         CLC   SVLOCEND,TMPSTART                                                
         BL    REQCST40                                                         
         CLC   SVLOCEND,TMPEND                                                  
         BH    REQCST40                                                         
*                                                                               
         MVC   SVTMPEND,TMPEND     SAVE OF PERIOD ENDING DATE                   
         CLI   QOPT1,C'Y'                                                       
         BNE   REQTMS10                                                         
         GOTO1 DATCON,DMCB,(1,TMPEND),(8,PPEREND)   PERIOD END DATE             
         DROP  R7                                                               
         B     REQTMS10                                                         
*                                                                               
REQCST90 DS    0H         REESTABLISH PERSON RECD KEY FOR NXT SEQ               
*        GOTO1 ACREPORT                                                         
*        AP    PKCOUNT,=P'1'        INC COMPANY TOTAL                           
*        AP    PKRECT,=P'1'         INC RECD TOTAL                              
REQCST95 DS    0H                                                               
         MVC   SVKEY,SVKEY2           REESTABLISH READ SEQ                      
         GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
         B     REQF60              GO READ SEQ FOR PERSON RECD                  
*                                                                               
*****************************************************************               
* READ TIME SHEET WEEKLY POINTER RECORD TSWRECD  X'0E0F' RECD   *               
* USING REGISTER R7                                             *               
*****************************************************************               
*                                                                               
REQTMS10 DS    0H                                                               
*                                                                               
         BAS   RE,READ1R     READ 1R RECORD FOR OFFICE DEPT SUB-DPT             
         BAS   RE,BLDWORK          PUT OFFICE/DEPT/SUBDPT IN WORK               
*                                                                               
         USING TSWRECD,R7                                                       
         LA    R7,SVKEY            CHECK FOR ANY TIME AT LOCATION               
         XC    SVKEY,SVKEY                                                      
         MVC   TSWKEY,SPACES                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E0F' TIMESHEET WEEKLY POINTER             
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,RCCOMPFL    COMPANY                                      
         MVC   TSWKPER,SVPERSON    PERSON CODE FROM PERSON RECORD               
         XC    TSWKEND,TSWKEND                                                  
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQTMS20                                                         
*                                                                               
REQTMS15 GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
REQTMS20 CLC   IOKEY(TSWKEND-TSWKEY),SVKEY                                      
         MVC   PDESC,SPACES                                                     
*        MVC   PDESC,=CL30'TIMESHEET WEEK PNTR NOT FOUND'                       
         BE    REQTMS30                                                         
         XC    SVDISP,SVDISP                                                    
         B     REQCST90            READ NEXT PERSON RECORD                      
REQTMS30 DS    0H                                                               
*                                                                               
*        MVC   SVKEY4,SVKEY                                                     
*        MVC   IOKEYSV,IOKEY       SAVE IOKEY                                   
*        BAS   RE,READ1R     READ 1R RECORD FOR OFFICE DEPT SUB-DPT             
*        BAS   RE,BLDWORK          PUT OFFICE/DEPT/SUBDPT IN WORK               
*        MVC   IOKEY,IOKEYSV                                                    
*                                                                               
T        USING TSWRECD,RF                                                       
         LA    RF,IOKEY                                                         
         CLC   T.TSWKODS,WORK      IS IT SAME OFFICE/DPT/SUBDPT                 
         BE    REQTMS32                                                         
         B     REQTMS15                                                         
*                                                                               
*        MVC   SVKEY,SVKEY4                                                     
*        GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
*        B     REQTMS15            GO READ SEQ FOR TIME RECD                    
*                                                                               
REQTMS32 DS    0H                                                               
*        LA    RF,IOKEY                                                         
         ZICM  R1,T.TSWKEND,3      CONVERT TSWKEND DATE TO BINARY               
         DROP  T                                                                
         LNR   R1,R1                                                            
         STCM  R1,7,SVTSWEND                                                    
*                                                                               
         CLC   SVTSWEND,SVTMPEND   CMPR TIME END WITH PRD END DATE              
         MVC   PDESC,SPACES                                                     
         BH    REQTMS35                                                         
         B     *+10                                                             
REQTMS35 MVC   PDESC,=CL30'TIME EXIST PAST END DATE'                            
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   REQTMS40                                                         
         GOTO1 DATCON,DMCB,(1,SVTSWEND),(8,PTSWEND)   TIME END DATE             
*                                                                               
REQTMS40 DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   REQTMS45                                                         
         CLC   PDESC,SPACES                                                     
         BE    REQTMS48                                                         
REQTMS45 GOTO1 ACREPORT                                                         
         AP    PKCOUNT,=P'1'        INC COMPANY TOTAL                           
         AP    PKRECT,=P'1'         INC RECD TOTAL                              
REQTMS48 MVC   SVKEY,SVKEY2     READ FOR NEXT LOCELQ X'83' IF EXIST             
         GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
         B     REQF70               MAY BE MORE LOCELQ ELEMS                    
*                                                                               
REQF200  DS    0H                                                               
         MVC   SVDISP,=F'0'                                                     
         MVC   SVKEY,SVKEY1           REESTABLISH READ SEQ                      
         GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
         B     REQF10                                                           
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    REQLX                                                            
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
         MVC   XP(L'TOTMSG),TOTMSG                                              
         EDIT  (P4,PKCOUNT),(8,XP+L'TOTMSG+1)                                   
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
*                                                                               
         CP    PKRECT,=P'0'                                                     
         BE    RUNLX                                                            
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
         MVC   XP(L'TOTMSG1),TOTMSG1                                            
         EDIT  (P4,PKRECT),(8,XP+L'TOTMSG1+1)                                   
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* READ1R  SUBROUTINE READS UNIT LEDGER 1R AND RETURNS LENGTH OF      *          
* OFFICE, DEPARTMENT AND SUB DEPARTMENT                              *          
**********************************************************************          
READ1R   NTR1                                                                   
         USING ACTRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'1R'    READ UNIT LEDGER 1R                         
*                                                                               
         GOTO1 =A(DMREADDR),DMCB,(RC)   READ                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'ACTKEY),SVKEY                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO         SET R5 TO POINT TO THE RECORD                      
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,ACTRFST-ACTRECD(R5)                                           
READ1R10 CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),ACLELQ                                                     
         BE    READ1R20                                                         
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     READ1R10                                                         
READ1R20 BAS   RE,GETLEVS                                                       
*                                                                               
READ1RX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* GETLEVS SUBROUTINE MAKES LEVELS SOFT, RETURNS INDIVIDUAL LEVEL     *          
* LENGTHS AND GIVES NUMBER OF NESTED LEVELS IN LEDGER RECDS          *          
**********************************************************************          
GETLEVS  NTR1                                                                   
         USING ACLELD,R5                                                        
*        LA    R5,ELEM                                                          
         MVC   LEVELS(LEVLNQ),SPACES   CLEAR LEVELS LENGTH/DISC                 
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LHI   R0,LEVELQ                 R0 = MAXIMUM NUMBER OF LEVELS          
         STC   R0,LEVNUM                 ASSUME 4 LEVEL STRUCTURE               
         LA    R1,ACLVALS                R1 = FIRST LEVEL LENGTH                
         LA    RE,LEVELS                 STORE ACCUMULATIVE LNTH HERE           
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
*                                                                               
GLEV10   ICM   R4,1,0(R1)                CURRENT LEVEL LENGTH                   
         BZ    GLEV20                    NO MORE LEVELS - ADJUST LEVNUM         
         STC   R4,0(RE)                                                         
         SR    R4,R3                     SUBTRACT CURRENT FROM PREVIOUS         
*                                                                               
         STC   R4,0(R2)                  CURRENT INDIVIDUAL LENGTH              
         IC    R3,0(R1)                  UPDATE R3                              
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ                 R1 = MAXIMUM NUMBER OF LEVELS          
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* BLDWORK SUBROUTINE PUTS CORRECT OFFICE DEPT AND SUB DEPT IN WORK   *          
**********************************************************************          
BLDWORK  NTR1                                                                   
         LA    R2,LEVLNQS          GET INDIVIDUAL LENGTH                        
         LA    R3,LEVELS           GET ACCUMULATIVE LENGTH                      
         LA    R1,WORK                                                          
         MVC   WORK,SPACES                                                      
*                                                                               
         ZIC   R4,0(R2)            INDIVIDUAL LEN IN R4                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVWRKOFF    MOVE OFFICE TO WORK                          
*                                                                               
         AHI   R4,1                                                             
         AR    R1,R4               GET NEXT AVAILABLE SPACE IN WORK             
         LA    R3,1(R3)            GET NEXT LEVEL'S ACCUMULTV LEN               
         LA    R2,1(R2)            GET NEXT LEVEL'S INDIVDUL LEN                
*                                                                               
         ZIC   R4,0(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVDEPT      MOVE DEPT TO WORK                            
*                                                                               
         AHI   R4,1                                                             
         AR    R1,R4               GET NEXT AVAILABLE SPACE IN WORK             
         LA    R3,1(R3)            GET NEXT LEVEL'S ACCUMULTV LEN               
         LA    R2,1(R2)            GET NEXT LEVEL'S INDIVDUL LEN                
*                                                                               
         ZIC   R4,0(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVSUBDPT    MOVE SUB DEPARTMENT TO WORK                  
*                                                                               
                                                                                
BLDWRKX  XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* EXTERNAL ADDRESS LIST                                          *              
******************************************************************              
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'9000'                                                        
ACCOUNT  DC    CL8'ACCOUNT'                                                     
TOTMSG   DC    C'TOTAL NUMBER OF RECORDS FOR THIS COMPANY ='                    
TOTMSG1  DC    C'RECORD TOTALS FOR THIS ACC FILE ='                             
EMPTIES  DC    CL198' '            SPACES                                       
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL #2                                                            *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R2,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)            RESET RC                                     
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*                                                                               
*        MVC   MSG,=CL10'SAPER REC'                                             
*        SR    R5,R5                                                            
*        ICM   R5,3,SAPELEN                                                     
*        GOTO1 ADUMP,DMCB,(RC),IO,(R5)                                          
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IOKEY,0                    
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IOKEY,0                    
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IOKEY,0                    
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   DSKADR,ACCKDA       SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',DSKADR,IO,DMWORK                  
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',DSKADR,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PPERSON  DS    CL8                 PERSON                                       
         DS    CL2                                                              
PSTARTDT DS    CL8                 LOCATION START DATE                          
         DS    CL2                                                              
PENDDT   DS    CL8                 LOCATION END DATE                            
         DS    CL2                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL6                                                              
PDEPT    DS    CL6                 DEPARTMENT                                   
         DS    CL2                                                              
PSUBDEPT DS    CL6                 SUB DEPARTMENT                               
         DS    CL4                                                              
PLOCDATE DS    CL8                 TIMESHEET LOCK DATE                          
         DS    CL2                                                              
PDESC    DS    CL30            DESCRIPTION WETHER TIME EXIST OR NOT             
         DS    CL2                                                              
PCALSTRT DS    CL8                 CALENDAR START DATE                          
         DS    CL2                                                              
PCALEND  DS    CL8                 CALENDAR END DATE                            
         DS    CL2                                                              
PPEREND  DS    CL8                 PERIOD END DATE                              
         DS    CL2                                                              
PTSWEND  DS    CL8                 TIME RECORD END DATE                         
         DS    CL2                                                              
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE                                       *         
***********************************************************************         
*                                                                               
ACLDD    DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
HELLO    DS    A                                                                
PRNTBL   DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DSKADR   DS    F                   DISK ADDRESS                                 
DISP2    DS    H                                                                
*                                                                               
ELEM     DS    CL255                                                            
MSG      DS    CL10                MESSAGE FOR DUMP                             
*                                                                               
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
SVTMPEND DS    XL3                 SAVED PERIOD ENDING DATE                     
SVTSWEND DS    XL3                 SAVED TIME SHEET RECORD END DATE             
SVLOCEND DS    PL3                 SAVED LOC    ENDING DATE                     
SVPERSON DS    CL8                 SAVED PERSON                                 
SVOFFICE DS    CL2                 SAVED OFFICE FROM PERSON RECD                
SVWRKOFF DS    CL2                 SAVED OFFICE TO BUILD WORK AREA              
SVDEPT   DS    CL6                 SAVED DEPARTMENT  PERSON RECD                
SVSUBDPT DS    CL6                 SAVED SUB DEPT    PERSON RECD                
SVKEY    DS    CL42                                                             
SVKEY1   DS    CL42                SAVE COMPANY RECORD KEY                      
SVKEY2   DS    CL42                SAVE PERSON RECORD KEY                       
SVKEY3   DS    CL42                SAVE COST CALENDAR RECORD KEY                
SVKEY4   DS    CL42                SAVE TIME RECORD                             
IOKEYSV  DS    CL42                SAVE IOKEY                                   
ELCODE   DS    XL1                                                              
*                                                                               
SVDISP   DS    F                                                                
SVPID    DS    XL2                                                              
SVAID    DS    CL2                 SAVED AREA FOR ALPHA/SECURITY ID             
SVPERNAM DS    CL8                 PERSONAL ID NAME                             
*                                                                               
SVNAM    DS    0C               LAST AND FIRST NAMES MUST BE TOGETHER           
SVLSTNM  DS    CL36                                                             
SVFSTNM  DS    CL36                                                             
SVNMLNQ  EQU   *-SVNAM                                                          
NMBYTE   DS    X                                                                
FSTNM    EQU   X'80'                                                            
MIDNM    EQU   X'40'                                                            
LSTNM    EQU   X'20'                                                            
NMOK     EQU   X'10'                                                            
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
PKCOUNT  DS    PL4                       RECORD/COMPANY TOTAL                   
PKRECT   DS    PL4                       RECORD TOTAL                           
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  DDMASTD                                                                      
*  ACBIGPRNTD                                                                   
*  DDBIGBOX                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'224ACREPLD02 05/01/01'                                      
         END                                                                    
