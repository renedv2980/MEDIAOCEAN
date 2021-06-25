*          DATA SET ACREPSC02  AT LEVEL 217 AS OF 05/01/02                      
*PHASE ACSC02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*   READS COST PERSON RECS AND SECURITY RECORDS AND MATCH FOR NAMES   *         
*   QOPT4=Y SHOW ONLY RECDS WHERE NAMES DO NOT MATCH                  *         
*   QOPT5=Y SHOW PIDS                                                 *         
*   QOPT6=Y SHOW ALL RECS INCLUDING THOSE RECORDS WHERE NAMES MATCH   *         
*   QOPT7=Y DUMP RECORDS                                              *         
***********************************************************************         
         TITLE 'PERSON RECORD SECURITY RECORD NAME MATCH'                       
ACSC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSC**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACSCD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST              REQUEST LAST                           
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVI   FCSUPOFC,C'Y'                                                    
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
*                                                                               
         USING CPYELD,R3                                                        
         L     R3,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         MVC   CPYSV,CPYLOGO       GET LOGIN ID / CONNECT ID                    
         DROP  R3                                                               
*                                                                               
         USING PERRECD,R3                                                       
         LA    R3,SVKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F' - PERSON RECORD                        
         MVC   PERKCPY,RCCOMPFL                                                 
         MVC   PERKCODE(L'QSELECT),QSELECT                                      
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
REQF20   LA    R1,2                                                             
         CLC   QSELECT,SPACES                                                   
         BNH   *+8                                                              
         LA    R1,10                                                            
         BCTR  R1,0                                                             
         EXCLC R1,IOKEY,SVKEY                                                   
         BNE   REQFX                                                            
*                                                                               
         LA    R3,IO         SET R3 TO POINT TO THE RECORD                      
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
*        MVC   XP,SPACES                                                        
         LA    RE,XP               CLEAR XP                                     
         LA    RF,L'XP                                                          
         LR    R1,RF                                                            
         LA    R0,EMPTIES                                                       
         MVCL  RE,R0                                                            
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R3                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL2                                                        
         BNE   REQF10                                                           
*                                                                               
         MVC   SVPID,PIDNO                SAVE OFF PID NUM FOR LATER            
         MVC   SVNAM(SVNMLNQ),SPACES      CLEAR OUT NAME FIELDS                 
*                                                                               
         MVC   PPERSON,PERKCODE    PERSON CODE                                  
*                                                                               
         CLI   QOPT5,C'Y'          IS USER REQUESTING FOR PID                   
         BNE   REQF25                                                           
         SR    R5,R5                                                            
         ICM   R5,3,PIDNO                                                       
         EDIT  (R5),(5,PPIDNUM)                                                 
*                                                                               
         USING GPNELD,R2                                                        
REQF25   LR    R2,R3                                                            
         MVI   ELCODE,GPNELQ                                                    
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
REQF30   BAS   RE,NEXTEL2                                                       
         BNE   REQF35                                                           
*                                                                               
         LA    RE,SVLSTNM                                                       
         CLI   GPNTYP,1            IS IT LAST NAME                              
         BE    *+8                                                              
         LA    RE,SVFSTNM          IT'S FIRST NAME                              
         IC    R1,GPNLN                                                         
         SH    R1,=Y(GPNLNQ+1)     FOR EX MOVE INS                              
         EX    R1,*+4                                                           
         MVC   0(0,RE),GPNNME      SAVE PERSON'S NAME                           
         B     REQF30                                                           
*                                                                               
         USING EMPELD,R2                                                        
REQF35   LR    R2,R3                                                            
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVTERM,SPACES                                                    
         OC    EMPTRM,EMPTRM                                                    
         BZ    REQF40                                                           
         GOTO1 DATCON,DMCB,(1,EMPTRM),(8,SVTERM) TERMINATION DATE               
*                                                                               
REQF40   DS    0H                                                               
         MVC   PLSTCST,SVLSTNM                                                  
         MVC   PFSTCST,SVFSTNM                                                  
         CLC   SVNAM(SVNMLNQ),SPACES                                            
         BH    REQF50                                                           
         MVC   PDESC,=CL30'NO NAMES ON COST ERROR'                              
         BAS   RE,PRINTIT                                                       
*        GOTO1 ACREPORT                                                         
*                                                                               
* READ CONTROL FILE CT5 RECORD                                                  
*                                                                               
REQF50   MVC   SVKEY1,0(R3)        SAVE KEY FOR NEXT SEQ                        
*                                                                               
         USING CT5REC,R3                                                        
         LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   CT5KTYP,CT5KTYPQ    C'5' - SYSTEM ACCESS RECORDS                 
         MVC   CT5KALPH,ALPHAID                                                 
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(CT5LEN-CT5KEY),SVKEY                                       
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
*                                                                               
         LA    R3,IO                                                            
         LA    R2,CT5DATA                                                       
         MVC   SVAID,ALPHAID       DEFAULT TO ALPHA ID                          
REQF60   CLI   0(R2),0                                                          
         BE    REQF80                                                           
         CLI   0(R2),X'B8'         SECURITY ALPHA ID ELM                        
         BE    REQF70                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF60                                                           
*                                                                               
         USING CTSEAD,R2                                                        
REQF70   MVC   SVAID,CTSEAAID      SECURITY ID                                  
         DROP  R2                                                               
*                                                                               
* READ SECURITY SA0REC AND SAPEREC                                              
*                                                                               
         USING SA0REC,R3                                                        
REQF80   LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ    C'0'-PERSONAL AUTHORIZATION RECORDS          
         MVC   SA0KAGY,SVAID       ALPHA/SECURITY ID                            
         MVC   SA0KNUM,SVPID                                                    
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(SA0LEN-SA0KEY),SVKEY                                       
         BE    REQF85                                                           
         MVC   PDESC,=CL30'SA0REC NOT FOUND'                                    
         B     REQF190                                                          
*                                                                               
REQF85   LA    R3,IO                                                            
         LA    R2,SA0DATA                                                       
REQF90   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'C3'         PERSON PERSONAL-ID                           
         BE    REQF100                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF90                                                           
*                                                                               
         USING SAPALD,R2                                                        
REQF100  MVC   SVPERNAM,SAPALPID   PERSON ID NAME                               
         DROP  R2                                                               
*                                                                               
* CHECK SECURITY PERSON RECORD                                                  
*                                                                               
         USING SAPEREC,R3                                                       
         LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   SAPETYP,SAPETYPQ    C'F' - PERSON RECORD                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SVAID       AGENCY ALPHA/SECURITY ID                     
         MVC   SAPEPID,SVPERNAM    PERSON ID                                    
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(SAPEDEF-SAPEKEY),SVKEY                                     
         BE    REQF109                                                          
*                                                                               
         CLI   QOPT4,C'Y'       IF YES SKIP THIS RECORD                         
         BE    REQF200          THAT IS WHERE SEC SIDE NOT EXIST                
*                                                                               
         MVC   PDESC,=CL30'SECURITY REC NOT FOUND' SAPEREC F04 NOT FND          
         MVC   PTERM,SVTERM                                                     
         B     REQF190                                                          
*                                                                               
REQF109  LA    R3,IO                                                            
         MVC   PPIDNM,SAPEPID      PERSON ID FROM SECURITY 8 BYTES              
         LA    R2,SAPEDATA                                                      
REQF110  CLI   0(R2),0                                                          
*        BE    REQF170                                                          
         BNE   *+14                                                             
         MVC   PDESC,=CL30'NO NAMES AT SECURITY SIDE'                           
         B     REQF190                                                          
         CLI   0(R2),X'C5'         PERSON NAME ELEMENT                          
         BE    REQF120                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF110                                                          
*                                                                               
         USING SANAMD,R2                                                        
REQF120  LA    R6,SANAME           POINT TO NAMES                               
         LA    R7,SANAMELN         POINT TO NAME LENGTH                         
         OI    NMBYTE,X'FF'                                                     
REQF130  DS    0H                                                               
         TM    NMBYTE,FSTNM        DID WE DO FST NAME BEFORE                    
         BNO   REQF140             YES,CHECK NEXT                               
         TM    SANAMIND,SANAMIFN   FIRST NAME                                   
         BNO   REQF140                                                          
*                                                                               
         LA    RE,SVFSTNM          POINT TO FST  NAME                           
         LA    RF,PFSTSEC                                                       
         NI    NMBYTE,X'FF'-FSTNM                                               
         B     REQF160                                                          
REQF140  DS    0H                                                               
         TM    NMBYTE,MIDNM        DID WE DO MID NAME BEFORE                    
         BNO   REQF150             YES,CHECK NEXT                               
         TM    SANAMIND,SANAMIMN   MIDDLE NAME                                  
         BNO   REQF150                                                          
*                                                                               
         ZIC   R1,0(R7)            GET SANAMELN                                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PMIDSEC(0),0(R6)    PRINT MIDDLE NAME                            
         AHI   R1,1                                                             
         AR    R6,R1               BUMP TO NEXT NAME                            
         LR    R7,R6                                                            
         LA    R6,1(R6)            POINT R6 TO NAME                             
         NI    NMBYTE,X'FF'-MIDNM   MIDDLE NAME DONE                            
         B     REQF130                                                          
*                                                                               
REQF150  DS    0H                                                               
         TM    NMBYTE,LSTNM        DID WE DO LST NAME BEFORE                    
         BNO   REQF170             YES,START PRINTING                           
         TM    SANAMIND,SANAMILN   LAST NAME                                    
         BNO   REQF170                                                          
*                                                                               
         LA    RE,SVLSTNM                                                       
         LA    RF,PLSTSEC          LAST NAME                                    
         NI    NMBYTE,X'FF'-LSTNM  LAST    NAME DONE                            
*                                                                               
REQF160  SR    R1,R1                                                            
         IC    R1,0(R7)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+4                                                           
         MVC   0(0,RF),0(R6)       SANAME                                       
*                                                                               
         EX    R1,*+4                                                           
         OC    0(0,RF),SPACES      UPPER CASE THE NAME                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),0(RE)       IS IT SAME AS COST SIDE                      
         BE    *+14                                                             
         NI    NMBYTE,X'FF'-NMOK                                                
         MVC   PDESC,=CL30'NAMES DO NOT MATCH'                                  
         ZIC   R1,0(R7)                                                         
         AR    R6,R1                                                            
         LR    R7,R6                                                            
         LA    R6,1(R6)            POINT TO NAME                                
         B     REQF130                                                          
         DROP  R2                                                               
*                                                                               
REQF170  DS    0H                                                               
         TM    NMBYTE,NMOK         DOES NAMES MATCH                             
         BNO   REQF180                                                          
*        MVC   XP,SPACES                                                        
*        LA    RE,XP               CLEAR XP                                     
*        LA    RF,L'XP                                                          
*        LR    R1,RF                                                            
*        LA    R0,EMPTIES                                                       
*        MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT6,C'Y'          WANT TO PRINT NAMES THAT MATCH               
         BE    REQF190                                                          
         LA    RE,XP               CLEAR XP                                     
         LA    RF,L'XP                                                          
         LR    R1,RF                                                            
         LA    R0,EMPTIES                                                       
         MVCL  RE,R0                                                            
         B     REQF200                                                          
REQF180  AP    PKCOUNT,=P'1'        INC RECD TOTAL                              
REQF190  BAS   RE,PRINTIT                                                       
*EQF190  GOTO1 ACREPORT                                                         
*                                                                               
REQF200  MVC   SVKEY,SVKEY1           REESTABLISH READ SEQ                      
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
*        BAS   RE,PRINTIT                                                       
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
         MVC   XP(L'TOTMSG),TOTMSG                                              
         EDIT  (P4,PKCOUNT),(8,XP+L'TOTMSG+1)                                   
*        BAS   RE,PRINTIT                                                       
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD5(9),=C'LOGIN ID:'                                          
         MVC   XHEAD5+10(L'CPYSV),CPYSV      COMPANY'S MAIN CONNECT ID          
*                                                                               
         B     EXIT                                                             
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
TOTMSG   DC    C'TOTAL NUMBER OF BAD RECORDS ='                                 
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
         DS    CL1                                                              
PPERSON  DS    CL8                                                              
         DS    CL3                                                              
PPIDNUM  DS    CL5                                                              
         DS    CL5                                                              
PLSTCST  DS    CL15                                                             
         DS    CL3                                                              
PFSTCST  DS    CL15                                                             
         DS    CL6                                                              
PPIDNM   DS    CL8                                                              
         DS    CL3                                                              
PLSTSEC  DS    CL15                                                             
         DS    CL3                                                              
PFSTSEC  DS    CL15                                                             
         DS    CL3                                                              
PMIDSEC  DS    CL15                MIDDLE NAME SECURITY SIDE                    
         DS    CL3                                                              
PDESC    DS    CL30                                                             
         DS    CL2                                                              
PTERM    DS    CL8                 TERMINATION DATE                             
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE                                       *         
***********************************************************************         
*                                                                               
ACSCD    DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
HELLO    DS    A                                                                
PRNTBL   DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DSKADR   DS    F                   DISK ADDRESS                                 
DISP2    DS    H                                                                
*                                                                               
MSG      DS    CL10                MESSAGE FOR DUMP                             
*                                                                               
SVKEY    DS    CL42                                                             
SVKEY1   DS    CL42                                                             
ELCODE   DS    XL1                                                              
CPYSV    DS    CL7                                                              
*                                                                               
SVPID    DS    XL2                                                              
SVAID    DS    CL2                 SAVED AREA FOR ALPHA/SECURITY ID             
SVPERNAM DS    CL8                 PERSONAL ID NAME                             
SVTERM   DS    CL8                 TERMINATION DATE                             
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
**PAN#1  DC    CL21'217ACREPSC02 05/01/02'                                      
         END                                                                    
