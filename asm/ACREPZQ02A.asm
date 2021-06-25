*          DATA SET ACREPZQ02A AT LEVEL 049 AS OF 08/16/00                      
*PHASE ACZQ02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'CHANGE MOA IN TMS RECORDS (NON BILL)'                           
         PRINT NOGEN                                                            
ACZQ02   CSECT                                                                  
         NMOD1 0,**ACZQ**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZQD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS     RELOCATE ADDRESSES                    
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         ZAP   PKDMPMAX,=P'100'                                                 
         ZAP   PKDMPCNT,=P'0'                                                   
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     RE,ACALTAB          RE=A(CALENDAR TABLE)                         
         LA    RF,MAXCAL*CALLNQ    RF=(LENGTH OF CALTAB)                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     RE,ACALTAB          RE=A(CALENDAR TABLE)                         
         MVI   0(RE),EOF           INITIALIZE TO X'FF'                          
*                                                                               
RUNFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST (MAIN PROCESSING)                                    *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   TOTHRS,=P'0'                                                     
         ZAP   TOTACC,=P'0'                                                     
         XC    SVLMOA(L'SVLMOA+L'SVHMOA),SVLMOA   CLEAR FIELDS FOR NEXT         
         XC    NTSH,NTSH                                                        
         LA    R4,P                                                             
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,PKDAT)                                   
*                                                                               
         BAS   RE,GETCAL           GET CALENDAR INFO                            
*                                                                               
         USING TIMRECD,R6                                                       
         LA    R6,SVKEY                                                         
         MVC   TIMKEY,SPACES                                                    
         MVC   TIMKCPY,RCCOMPFL                                                 
         MVC   TIMKUNT(2),=C'1R'                                                
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)     READ SEQUENTIAL                        
REQF20   CLC   SVKEY(3),IOKEY            SAME KEY??                             
         BNE   REQF100                                                          
*                                                                               
         LA    R6,IOKEY                                                         
*                                                                               
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   REQF10                                                           
         CLC   TIMKPEDT(2),=X'9901'                                             
         BL    REQF10                                                           
         CLC   TIMKPEDT(2),=X'9912'                                             
         BH    REQF10                                                           
         CLC   TIMKHMOS,=X'9912'                                                
         BNH   REQF10                                                           
*                                                                               
         MVC   MSG,=CL10'KEY IN'                                                
         GOTO1 ADUMP,DMCB,(RC),(R6),50                                          
*                                                                               
         USING CALD,R3                                                          
         L     R3,ACALTAB                                                       
REQF30   CLI   CALST,X'FF'                                                      
         BE    REQF10                                                           
         CLC   TIMKPEDT,CALEND                                                  
         BH    REQF40                                                           
         CLC   TIMKPEDT,CALST                                                   
         BNL   REQF50                                                           
REQF40   LA    R3,CALLNQ(R3)                                                    
         B     REQF30                                                           
*                                                                               
REQF50   CLC   TIMKLMOS,TIMKHMOS      THIS IS NOT EXSPECTED SO DIE              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TIMKLMOS,CALMNTH       THIS ONE IS OK                            
         BE    REQF10                                                           
*                                                                               
         MVC   DIO,IOKEY              SAVE OFF KEY FOR LATER WRITE              
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
         LA    R6,IO                                                            
*                                                                               
         MVC   MSG,=CL10'SVDA IN'                                               
         GOTO1 ADUMP,DMCB,(RC),SVDA,L'SVDA                                      
*                                                                               
         MVC   MSG,=CL10'REC IN'                                                
         SR    R2,R2                                                            
         ICM   R2,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R6),(R2)                                        
*                                                                               
         LR    R5,R6                                                            
         AH    R5,DISP2                                                         
REQF60   CLI   0(R5),0                                                          
         BE    REQF90                                                           
         CLI   0(R5),X'8B'                                                      
         BE    REQF80                                                           
REQF70   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     REQF60                                                           
*                                                                               
         USING TIMELD,R5                                                        
REQF80   DS    0H                                                               
         OC    SVLMOA(L'SVLMOA+L'SVHMOA),SVLMOA  ANY DATES YET?                 
         BNZ   *+16                                                             
         MVC   SVLMOA,CALMNTH                                                   
         MVC   SVHMOA,CALMNTH                                                   
*                                                                               
         CLI   TIMETYP,TIMEINP     INPUT DETAIL                                 
         BNE   REQF70                                                           
         CLI   TIMLN,TIMILN1Q                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        CLC   TIMADAT,CALST                                                    
*        BNL   *+6                                                              
*        DC    H'0'                                                             
*        CLC   TIMADAT,CALEND                                                   
*        BNH   *+6                                                              
*        DC    H'0'                                                             
         CLC   TIMMOA,CALMNTH      ALREADY CORRECT WRONG LOGIC                  
         BE    REQF10                                                           
*                                                                               
         MVC   PACCT,TIMKULA                                                    
         GOTO1 DATCON,DMCB,(1,TIMKPEDT),(0,PDATE)                               
         MVC   PCACCT,TIMACC       SJ OR 1N                                     
         XC    FULL,FULL                                                        
         MVC   FULL(L'TIMMOA),TIMMOA                                            
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(14,PMNTH)                                  
         MVC   TIMMOA,CALMNTH                                                   
         CLC   SVLMOA,TIMMOA       UPDATE LOW AND HIGH                          
         BL    *+10                                                             
         MVC   SVLMOA,TIMMOA       SAVE OFF LOWER MOA                           
         CLC   SVHMOA,TIMMOA                                                    
         BH    *+10                                                             
         MVC   SVHMOA,TIMMOA       SAVE OFF HIGHER MOA                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(L'TIMMOA),TIMMOA                                            
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(14,PNEWMNTH)                               
         EDIT  TIMHRS,(14,PHOURS),2,MINUS=YES                                   
         AP    TOTHRS,TIMHRS                                                    
         MVC   PTYPE,=CL15'UNKNOWN'                                             
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   *+10                                                             
         MVC   PTYPE,=CL15'BILLABLE'                                            
         CLI   TIMTTYP,TIMTCR                                                   
         BNE   *+10                                                             
         MVC   PTYPE,=CL15'REALIZATION'                                         
         CLI   TIMTTYP,TIMTCN                                                   
         BNE   *+10                                                             
         MVC   PTYPE,=CL15'NON-BILLABLE'                                        
         CLI   TIMTTYP,TIMTNC                                                   
         BNE   *+10                                                             
         MVC   PTYPE,=CL15'NON-CLIENT'                                          
         GOTO1 ACREPORT                                                         
         BAS   RE,ADDTBL                                                        
         B     REQF70              GET NEXT EL                                  
*                                                                               
REQF90   DS    0H                                                               
         LA    R6,DIO                                                           
         MVC   TIMKLMOS,SVLMOA                                                  
         MVC   TIMKHMOS,SVHMOA                                                  
         MVC   MSG,=CL10'KEY OUT'                                               
         GOTO1 ADUMP,DMCB,(RC),DIO,L'DIO                                        
*                                                                               
         LA    R6,IO                                                            
         MVC   TIMRLMOS,SVLMOA                                                  
         MVC   TIMRHMOS,SVHMOA                                                  
         XC    SVLMOA(L'SVLMOA+L'SVHMOA),SVLMOA   CLEAR FIELDS FOR NEXT         
*                                                                               
         MVC   MSG,=CL10'SVDA IN'                                               
         GOTO1 ADUMP,DMCB,(RC),SVDA,L'SVDA                                      
*                                                                               
         MVC   MSG,=CL10'REC OUT'                                               
         SR    R2,R2                                                            
         ICM   R2,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R6),(R2)                                        
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   REQF10                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    REQF10                                                           
         GOTO1 =A(DMWRTDR),DMCB,(RC)      WRITE BACK DIR                        
         GOTO1 =A(DMPUTREC),DMCB,(RC)     PUT RECORD BACK                       
         B     REQF10                                                           
*                                                                               
REQF100  MVC   PACCT,=C'TOTAL HOURS   '                                         
         EDIT  TOTHRS,(14,PHOURS),2,MINUS=YES                                   
         GOTO1 ACREPORT                                                         
         MVC   PACCT,=C'TOTAL ACCOUNTS'                                         
         EDIT  TOTACC,(14,PHOURS)                                               
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         L     R3,ATSHTBL                                                       
         USING TSHD,R3                                                          
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         LA    RF,TSHLNQ                                                        
         GOTO1 XSORT,DMCB,(0,(R3)),(R0),(RF),(RF),0                             
REQF110  MVC   PACCT,TSHACC                                                     
         GOTO1 DATCON,DMCB,(1,TSHDTE),(0,PDATE)                                 
         GOTO1 ACREPORT                                                         
         LA    R3,TSHLNQ(R3)                                                    
         BCT   R0,REQF110                                                       
*                                                                               
         MVC   PACCT(16),=C'TOTAL TIMESHEETS'                                   
         EDIT  (B2,NTSH),(14,PHOURS)                                            
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* GET CALENDAR                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CALD,R3                                                          
         USING CASRECD,R6          R6=A(CALENDAR RECORD)                        
GETCAL   NTR1                                                                   
         L     R3,ACALTAB                                                       
*                                                                               
GETC10   LA    R6,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE(1),PKDAT                                                
GETC15   GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     GETC30                                                           
*                                                                               
GETC20   GOTO1 =A(DMSEQDR),DMCB,(RC)     READ SEQUENTIAL                        
GETC30   CLC   SVKEY(3),IOKEY            SAME KEY??                             
         BNE   GETCX                                                            
*                                                                               
         LA    R6,IOKEY                                                         
*                                                                               
         CLC   CASKEMOA,PKDAT                                                   
         BH    GETC20                                                           
*        CLC   CASKSMOA,PKDAT                                                   
*        BL    GETC20                                                           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
*                                                                               
         LA    R6,IO                                                            
         AH    R6,DISP2                                                         
GETC70   CLI   0(R6),0                                                          
         BE    GETCX                                                            
         CLI   0(R6),TMPELQ        X'88' TS PERIODS ELEMENT                     
         BE    GETC100                                                          
GETC80   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GETC70                                                           
*                                                                               
         USING TMPELD,R6                                                        
GETC100  MVC   CALST,TMPSTART      SAVE PERIOD START DATE                       
         MVC   CALEND,TMPEND       SAVE PERIOD END DATE                         
         MVC   CALMNTH,TMPMTH      SAVE PERIOD MONTH                            
         LA    R3,CALLNQ(R3)                                                    
         MVI   0(R3),EOF           ALWAYS SET EOF MARKER                        
         B     GETC80                                                           
*                                                                               
GETCX    B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
*********************************************************************           
* ADD TO TABLE                                                        *         
*     R6 = A(TIME RECORD)                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R6                                                       
ADDTBL   NTR1  ,                                                                
         CLC   TIMKCULA,LASTACC                                                 
         BE    *+16                                                             
         MVC   LASTACC,TIMKCULA                                                 
         AP    TOTACC,=P'1'                                                     
         L     R3,ATSHTBL                                                       
         USING TSHD,R3                                                          
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         BZ    ADDTBL5                                                          
ADDTBL1  CLC   TSHACC,LASTACC                                                   
         BNE   ADDTBL3                                                          
         CLC   TSHDTE,TIMKPEDT                                                  
         BNE   ADDTBL3                                                          
         B     EXIT                                                             
ADDTBL3  LA    R3,TSHLNQ(R3)                                                    
         BCT   R0,ADDTBL1                                                       
ADDTBL5  MVC   TSHACC,LASTACC                                                   
         MVC   TSHDTE,TIMKPEDT                                                  
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         AH    R0,=H'1'                                                         
         STCM  R0,3,NTSH                                                        
         CLC   NTSH,=Y(MAXTSH)                                                  
         BNH   *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DUMP)             DUMPING RECORDS                              
         DC    A(CALTAB)           CALENDAR TABLE                               
         DC    A(TSHTBL)           TIMESHEET TABLE                              
*                                                                               
         DC    V(PRNTBL)                                                        
*                                                                               
LASTACC  DC    CL15' '                                                          
*                                                                               
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCMST   DC    CL8'ACCMST'                                                      
DIR      DC    CL6'ACCDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
MAXCAL   EQU   200                                                              
MAXTSH   EQU   1000                                                             
EOF      EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
         DC    C'**CALTAB**'                                                    
CALTAB   DS    XL(MAXCAL*CALLNQ)                                                
         DC    AL1(EOF)                                                         
*                                                                               
         DC    C'**TSHTAB**'                                                    
TSHTBL   DS    XL(MAXTSH*TSHLNQ)                                                
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
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',DIO,DIO                
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    PKDMPCNT,PKDMPMAX                                                
         BH    DUMPX                                                            
         AP    PKDMPCNT,=P'1'                                                   
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
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DSECT FOR PROGRAM                                                  *          
**********************************************************************          
         SPACE 1                                                                
ACZQD    DSECT                                                                  
VTYPES   DS    0A                                                               
ADUMP    DS    A                   DUMPING RECORDS                              
ACALTAB  DS    A                   CALENDAR TABLE                               
ATSHTBL  DS    A                   TIMESHEET TABLE                              
*                                                                               
PRNTBL   DS    V                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ACTIVE   DS    CL1                                                              
PKDAT    DS    PL3                                                              
*                                                                               
SVLMOA   DS    PL2                 SAVED AREA FOR LOW MOA                       
SVHMOA   DS    PL2                 SAVED AREA FOR HIGH MOA                      
*                                                                               
TOTHRS   DS    PL6                                                              
TOTACC   DS    PL6                                                              
NTSH     DS    H                                                                
DA       DS    F                                                                
DMWRK    DS    12D                                                              
*                                                                               
DA2      DS    F                                                                
DMWRK2   DS    12D                                                              
*                                                                               
SVDA     DS    F                   SAVED AREA FOR DISK ADDRESS                  
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
DIO      DS    CL(CACRFST-CACRECD)                                              
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
MSG      DS    CL10                DUMP MESSAGE                                 
PKDMPCNT DS    PL8                 NUMBER OF DUMPS                              
PKDMPMAX DS    PL4                 MAXIMUM NUMBER OF DUMPS                      
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
         SPACE 1                                                                
CALD     DSECT                     CALENDAR DSECT                               
CALST    DS    XL3                                                              
CALEND   DS    XL3                                                              
CALMNTH  DS    XL2                                                              
CALLNQ   EQU   *-CALD                                                           
*                                                                               
*                                                                               
*                                                                               
PLINED   DSECT                     PRINT DSECT                                  
PACCT    DS    CL14                                                             
         DS    CL5                                                              
PCACCT   DS    CL14                                                             
         DS    CL5                                                              
PDATE    DS    CL8                                                              
         DS    CL5                                                              
PHOURS   DS    CL14                                                             
         DS    CL5                                                              
PMNTH    DS    CL6                                                              
         DS    CL1                                                              
PTYPE    DS    CL15                                                             
         DS    CL5                                                              
PNEWMNTH DS    CL6                                                              
*                                                                               
*                                                                               
*                                                                               
TSHD     DSECT                     TIMESHEET DSECT                              
TSHACC   DS    XL15                ACCOUNT                                      
TSHDTE   DS    XL3                 DATE                                         
TSHLNQ   EQU   *-TSHD                                                           
         EJECT                                                                  
**********************************************************************          
* INCLUDES                                                           *          
**********************************************************************          
         SPACE 1                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACMASTD                                                                
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACREPZQ02A08/16/00'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
