*          DATA SET ACREPTE02  AT LEVEL 004 AS OF 01/08/02                      
*PHASE ACTE02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACTE - 1099 TAX ERROR REPORT'                                   
***********************************************************************         
*              OPT1:'N' OR ' '= TEST RUN (WITH REGISTER AT END).      *         
*                   'Y'= USE LIVE FORMS (NO REGISTER).                *         
*                   'R'= TEST RUN BUT SUPRESS FORMS PRINTING.         *         
*              OPT2:'Y'= SUPPRESS AMOUNTS < $600                      *         
*              OPT3:'C'= CORRECTION RQST. (PRODUCE 'G' RECORDS)       *         
*                   'T'= TEST RUN.                                    *         
*                   'R'= REPLACEMENT RUN.                             *         
*                   ' '= REGULAR RUN.                                 *         
*              OPT4:'R'= PRODUCE REGISTER ONLY. (READ A TAPE)         *         
*                   'T'= PRODUCE TAPE.     (1ST PASS)                 *         
*              OPT6:' '= DDS IS TRANSMITTER FOR THE TAPE.             *         
*                   'C'= COMPANY IS ITS OWN TRANSMITTER.              *         
*              QSEL:   = REPLACEMENT ALPHA REQUIRED FOR ALL REP TAPES *         
***********************************************************************         
         SPACE 1                                                                
ACTE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTE**,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACTED,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                LOOKUP COMPANY INFO                          
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
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
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R7                                                        
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R7,P                                                             
         MVC   PRTLNE(PRLNQ),SPACES                                             
*                                                                               
         XC    SVSDAT,SVSDAT                                                    
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,SVSDAT)                                
*                                                                               
REQF10   MVC   SVEDAT,=X'FFFFFF'   MARK INFINITY                                
         CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,SVEDAT)                                  
*                                                                               
REQF20   GOTO1 DATCON,DMCB,(5,0),(20,SVCDTE)                                    
         CLC   QEND,SPACES                                                      
         BE    REQF30                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(20,SVCDTE)                                 
*                                                                               
         USING T99RECD,R4                                                       
REQF30   LA    R4,SVKEY                                                         
         MVI   T99KTYP,T99KTYPQ    X'3E' - 1099 TAX INFO TYPE                   
         MVI   T99KSUB,T99KSUBQ    X'15' - 1099 TAX INFO SUB TYPE               
         MVC   T99KCPY,RCCOMPFL    COMPANY                                      
         MVC   T99KYEAR,SVCDTE     CURRENT YEAR                                 
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     REQF50                                                           
*                                                                               
REQF40   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
REQF50   CLC   SVKEY(T99KOID-T99KEY),IOKEY  SAME ACCT?                          
         BNE   REQFX                                                            
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
         XC    FLAG,FLAG                                                        
*                                                                               
         LA    R2,T99RFST                                                       
REQF51   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    REQF52                                                           
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     REQF51                                                           
*                                                                               
         USING NAMELD,R2                                                        
REQF52   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         CHI   R1,L'PRTNAME-1                                                   
         BNH   *+8                                                              
         LA    R1,L'PRTNAME-1                                                   
         EX    R1,*+4                                                           
         MVC   PRTNAME(0),NAMEREC                                               
         DROP  R2                                                               
*                                                                               
         USING CTIREC,R5                                                        
         LA    R5,CTKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,T99KOID                                                  
         GOTO1 =A(DMCTFIL),DMCB,(RC)                                            
         CLC   CTKEY(CTILEN-CTIKEY),IOKEY2    SAME KEY?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO2                                                           
*                                                                               
         LA    RE,CTIDATA                                                       
REQF54   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'02'         DESCRIPTION ELEMENT                          
         BE    REQF55                                                           
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     REQF54                                                           
*                                                                               
         USING CTDSCD,RE                                                        
REQF55   MVC   SVLOGO,CTDSC        MOVE IN LOGO                                 
         DROP  RE                                                               
*                                                                               
         MVC   MSG,=CL10'1099  REC'                                             
         SR    R6,R6                                                            
         ICM   R6,3,T99RLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R4),(R6)                                        
*                                                                               
         LA    R2,T99RFST          POINT TO FIRST ELEMENT                       
REQF60   CLI   0(R2),0             END OF RECORD?                               
         BE    REQF100                                                          
         CLI   0(R2),X'DB'         FREE FORM TEXT ELEMENT                       
         BE    REQF80                                                           
REQF70   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF60                                                           
*                                                                               
         USING FFTELD,R2                                                        
REQF80   CLI   FFTTYPE,FFTTCNAM    NAME ELEMENT?                                
         BNE   REQF90                                                           
         OI    FLAG,FLGNM          MARK THAT NAME WAS FOUND                     
         CLC   FFTTNAME,SPACES     CONTACT NAME                                 
         BH    *+12                                                             
         MVI   PRTPER,C'I'                                                      
         OI    FLAG,FLGERR         MARK AS ERROR                                
         CLC   FFTTPHON,SPACES     CONTACT PHONE #                              
         BH    REQF70                                                           
         MVI   PRTPHON,C'I'                                                     
         OI    FLAG,FLGERR         MARK AS ERROR                                
         B     REQF70                                                           
*                                                                               
REQF90   CLI   FFTTYPE,FFTTTNNI    1099 TAX INFO                                
         BNE   REQF70                                                           
         OC    FFTTNOF,FFTTNOF     # OF FORMS                                   
         BNZ   REQF70                                                           
         MVI   PRTFRMS,C'I'                                                     
         OI    FLAG,FLGERR         MARK AS ERROR                                
         B     REQF70                                                           
*                                                                               
REQF100  TM    FLAG,FLGNM          DID WE GET A NAME ELEMENT?                   
         BO    REQF110                                                          
         MVI   PRTPER,C'I'         NO NAME ELEMENT - MARK PER/PHONE             
         MVI   PRTPHON,C'I'                                                     
         B     *+12                                                             
REQF110  TM    FLAG,FLGERR                                                      
         BNO   REQF40                                                           
         MVC   PRTAGY,SVLOGO                                                    
         MVC   PRTYR,SVCDTE                                                     
         GOTO1 ACREPORT                                                         
         B     REQF40                                                           
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT 1099 FORM                                                    *          
**********************************************************************          
         SPACE 1                                                                
PRNT     NTR1                      CLEAR OUT BUFFER                             
*                                                                               
PRNTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R2,DISP2,ELCODE,2                                               
         EJECT                                                                  
**********************************************************************          
* TAPE NAMES & ADDRESS CONSTANTS                                     *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)                                                        
         DC    A(DUMP)             DUMP ROUTINE                                 
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(L'SPACES),SPACES                                         
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXCOLS+3,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PRTYR-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PRTNAME-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTPER-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PRTPHON-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTFRMS-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PRLNQ,C'R'                                               
         B     BX500                                                            
*                                                                               
BX100    MVI   BOXCOLS+10,C'C'     ALL OTHER REPORTS                            
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+45,C'R'                                                  
*                                                                               
BX500    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
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
         CLI   QOPT10,C'Y'                                                      
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
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         EJECT                                                                  
**********************************************************************          
* LTERALS                                                            *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',CTKEY,IOKEY2                 
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),=C'ACCDIR ',SVKEY,IOKEY,0            
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
DMX      DS    0H                                                               
*        CLI   DMCB+8,0            MAKE SURE THERE IS NO ERROR                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACTED    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX STORAGE                       
*                                                                               
VTYPES   DS    0A                                                               
PRNTBL   DS    A                                                                
ADUMP    DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SVDA     DS    F                   SAVED AREA FOR DISK ADDRESS                  
SVLOGO   DS    CL7                 SAVED AREA FOR CPYLOGO                       
SVKEY    DS    CL42                                                             
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
*                                                                               
CTKEY    DS    CL25                SAVED AREA FOR CONTROL KEY                   
*                                                                               
MSG      DS    CL10                MESSAGE FOR PRINTABLE                        
ELCODE   DS    CL1                                                              
SVSDAT   DS    PL3                 PACKED START DATE                            
SVEDAT   DS    PL3                 PCAKED END DATE                              
SVCDTE   DS    CL8                 SAVED AREA FOR DATE (INCLD CENTURY)          
*                                                                               
FLAG     DS    XL1                                                              
FLGERR   EQU   X'80'               ERROR FOUND                                  
FLGNM    EQU   X'40'               NAME ELEMENT FOUND?                          
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
IO2      DS    0CL2042                                                          
IOKEY2   DS    CL42                                                             
         DS    CL2000              DATA                                         
IO2LNQ   EQU   *-IO2               LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* PRINT DSECT                                                        *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                                                               
         DS    CL5                                                              
PRTAGY   DS    CL7                                                              
         DS    CL5                                                              
PRTYR    DS    CL4                                                              
         DS    CL5                                                              
PRTNAME  DS    CL33                                                             
         DS    CL5                                                              
PRTPER   DS    CL1                                                              
         DS    CL5                                                              
PRTPHON  DS    CL1                                                              
         DS    CL5                                                              
PRTFRMS  DS    CL1                                                              
         DS    CL5                                                              
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
*ACREPWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*DDMASTD                                                                        
*DDBIGBOX                                                                       
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPTE02 01/08/02'                                      
         END                                                                    
