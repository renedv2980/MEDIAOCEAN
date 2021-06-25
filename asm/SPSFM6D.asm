*          DATA SET SPSFM6D    AT LEVEL 011 AS OF 12/04/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T2176DA                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE DUMPOUT                                                                
*INCLUDE PRTREC                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2176D - DMTEST VALIDATION AND REPORT                 *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS REPORT ACTION ONLY                           *         
*                                                                     *         
*  INPUTS       SCREEN T2973F (REPORT)                                *         
*                                                                     *         
*  OUTPUTS      PQ REPORT                                             *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- 2ND PROGRAM BASE                                *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- 1ST PROGRAM BASE                                *         
*               RC -- GEND                                            *         
*               RD -- SUBROUTINE CALL CHAIN                           *         
*               RE -- WORK                                            *         
*               RF -- WORK                                            *         
*                                                                     *         
***********************************************************************         
SPSFM6D  TITLE '- DEMAND (DEGET) TEST PROGRAM'                                  
SPSFM6D  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SF6D**,R7,RR=RE                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
OKEXIT   CR    RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         XC    DEMMSG,DEMMSG       CLEAR MY MESSAGE FIELD                       
         OI    DEMMSGH+6,X'80'     XMIT                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 (RF),DMCB,0,X'D9000A03'       DAYPAK                             
         MVC   VDAYPAK,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000A0E'       TIMVAL                             
         MVC   VTIMVAL,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000A29'       REGETIUN                           
         MVC   VREGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A24'       SPGETIUN                           
         MVC   VSPGTIUN,0(R1)                                                   
*                                                                               
* SPOOF DOESN'T OPEN THE PAV FILES FOR THE SPOT SYSTEM, SO WE NEED              
* TO DO IT (USING THE SOFT LIST).                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VK10                                                             
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'SPOT',                   +        
               =C'NPAVDIR NL=PAVFLX',AIO3,0                                     
*                                                                               
VK10     DS    0H                                                               
         MVI   DEMOLIST,X'FF'                                                   
         MVC   MAXIO,=F'500'       SET MAXIMUM DEMO TRACE COUNT                 
         MVI   PCTRL2,C' '         SET TO INITIAL DEFAULT VALUE                 
         MVI   PCTRLI,C' '                                                      
         XC    EFLTLST(EFLTLSTQ),EFLTLST   INIT ELEM FILT LIST                  
*                                                                               
***********************************************************************         
*CLRTABS - SPECIAL CALL TO CLEAR DEMO TABLES IN DATASPACE                       
***********************************************************************         
*                                                                               
         LA    R1,DEMFUNCH                                                      
         CLI   5(R1),0                                                          
         JE    SP00                                                             
         CLC   =C'CLRTAB',DEMFUNC                                               
         JNE   SP00                                                             
         TM    WHEN,X'40'          ONLY VALID "NOW"                             
         BO    *+12                                                             
         ST    R1,FADR                                                          
         B     ONLYNOW                                                          
*                                                                               
         XC    TBLPARM,TBLPARM                                                  
         OC    DEMFILE,SPACES      BLANK PAD                                    
         LA    R1,DEMFILEH                                                      
         ST    R1,FADR                                                          
         LAY   R2,CLRTAB                                                        
*                                                                               
CLR10    CLI   0(R2),X'FF'                                                      
         JE    EIIF                                                             
         CLC   DEMFILE,0(R2)                                                    
         JE    *+12                                                             
         LA    R2,L'CLRTAB(R2)                                                  
         J     CLR10                                                            
         MVC   TBLPARM(1),8(R2)                                                 
         MVI   TBLPARM+4,X'FF'                                                  
         L     RF,ACOMFACS                                                      
         L     RF,CDEMADDR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'FE',TBLPARM),ACOMFACS,,C'$BLD'                      
         MVC   DEMMSG(8),0(R2)                                                  
         MVC   DEMMSG+8(20),=C' CLEARED AND REBUILT'                            
*                                                                               
         B     VKX                                                              
         EJECT                                                                  
* VALIDATE ALL SCREEN FIELDS                                                    
*                                                                               
         EJECT                                                                  
SP00     DS    0H                                                               
         LA    R1,DEMMJKYH         MAJOR KEY FIELD                              
         ST    R1,FADR                                                          
         CLI   5(R1),0             SPECIFIC MAJOR KEY PROVIDED?                 
         BE    *+16                NO                                           
         TM    WHEN,X'20'          YES: ONLY VALID "SOON" (NOT "NOW")           
         BO    VKX                                                              
         B     NOTNOW                                                           
*                                                                               
         LAY   R2,INPTBL                                                        
         USING INPTBLD,R2          R2=A(INPUT FIELD TABLE)                      
SP2      CLI   0(R2),X'FF'         TEST E-O-T                                   
         JE    VKX                                                              
*                                                                               
         LH    R1,INPIFLD                                                       
         LA    R1,T217FFD(R1)                                                   
         ST    R1,FADR             SET A(INPUT FIELD HEADER)                    
         LH    RE,INPOFLD                                                       
         LA    RE,DBLOCK(RE)                                                    
         ST    RE,OADR             SET A(OUTPUT FIELD)                          
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         MVC   FLDH,0(R1)          MOVE FIELD HEADER TO W/S                     
         SR    RE,RE                                                            
         ICM   RE,1,FLDH+5                                                      
         BZ    SP4                                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)        MOVE FIELD TO W/S                            
         TM    FLDH+4,X'08'                                                     
         BZ    SP4                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         STCM  RE,15,FLDH                                                       
*                                                                               
SP4      CLI   FLDH+5,0            TEST IF FIELD INPUT                          
         BNE   SP5                                                              
         TM    INPINDS,X'01'       NO - TEST IF REQUIRED                        
         BZ    SP14                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
SP5      DS    0H                                                               
         CLC   FLDH+5(1),INPIMIN   TEST INPUT LENGTH                            
         BL    TOOSHORT                                                         
         CLC   FLDH+5(1),INPIMAX                                                
         BH    TOOLNG                                                           
         MVC   DUB,FLD                                                          
         TM    INPINDS,X'F0'                                                    
         BZ    SP12                                                             
         TM    INPINDS,X'80'       TEST FIELD NUMERIC                           
         BZ    SP6                                                              
         TM    FLDH+4,X'08'        YES - TEST IF INPUT NUMERIC                  
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         OC    FLDH(4),FLDH                                                     
         BZ    EIIF                                                             
         CLC   FLDH(4),=F'99999'                                                
         BH    EIIF                                                             
         MVC   DUB,FLDH+2                                                       
         B     SP12                                                             
*                                                                               
SP6      SR    RF,RF                                                            
         ICM   RF,7,INPADDR                                                     
         A     RF,RELO                                                          
         TM    INPINDS,X'40'       TEST IF TABLE SEARCH                         
         BZ    SP10                                                             
         SR    R1,R1                                                            
         ICM   R1,1,INPIMAX                                                     
         SR    RE,RE                                                            
         ICM   RE,1,INPOLEN                                                     
         LA    R0,0(R1,RE)         R0=TOTAL ENTRY LENGTH                        
         BCTR  R1,0                R1=PART 1 TABLE LENGTH                       
         LLC   R3,FLDH+5                                                        
         BCTR  R3,0                R3=L'INPUT-1                                 
*                                  LOOK-UP FIELD IN TABLE                       
SP8      CLI   0(RF),X'FF'                                                      
         BE    EIIF                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),FLD                                                      
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     SP8                                                              
         LA    RE,1(RF,R1)                                                      
         MVC   DUB,0(RE)                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),FLD                                                      
         BE    SP12                                                             
         L     RE,FADR             OUTPUT FULL FIELD VALUE                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),0(RF)                                                    
         OI    6(RE),X'80'                                                      
         B     SP12                                                             
*                                                                               
SP10     BASR  RE,RF               GO TO VALIDATION ROUTINE                     
         BNE   OKEXIT                                                           
         TM    INPINDS,X'10'                                                    
         BO    SP14                                                             
*                                                                               
SP12     L     RF,OADR             MOVE OUTPUT TO DBLOCK                        
         LLC   RE,INPOLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
*                                                                               
SP14     LA    R2,L'INPTBL(R2)     BUMP TO NEXT FIELD                           
         B     SP2                                                              
         DROP  R2                                                               
*                                                                               
VKX      DS    0H                                                               
         J     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*SP00 -  REGULAR DEMT PROCESSING                                                
***********************************************************************         
PR       DS    0H                                                               
         CLC   =C'CLRTAB',DEMFUNC  CLEARING A DATASPACE TABLE?                  
         BE    EXIT                                                             
*                                                                               
         CLI   DEMMJKYH+5,0        SPECIFIC MAJOR KEY PROVIDED?                 
         BE    SP16                NO                                           
*                                                                               
         LA    R1,DEMMJKYH         MAJOR KEY FIELD                              
         ST    R1,FADR                                                          
         LAY   R2,DFILTAB          DEMO DIRECTORY TABLE                         
         USING DFILTABD,R2                                                      
PR10     CLI   0(R2),X'FF'         EOT?                                         
         BE    EIIF                YES: NO SUCH DIRECTORY                       
         LLC   R1,DEMFILEH+5       L'PROVIDED FILE NAME                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DEMFILE(0),DFILDIRN FIND MATCH ON DIRECTORY NAME                 
         BE    *+12                                                             
         AHI   R2,DFILTBLQ                                                      
         B     PR10                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         LLC   R1,DEMMJKYH+5       L'PROVIDED KEY                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DEMMJKY                                                  
         CLC   =C'NTIBITMAPS',DEMMJKY SPECIAL CALL TO DUMP BITMAP RECS?         
         BNE   PR15                NO                                           
*                                                                               
         MVC   WORK,SPACES         YES: BUILD THE BITMAP KEY TO DECODE          
         MVI   WORK,C'('           CONSTRUCT THIS: '(PNNPPPP)01'                
BITM     USING PRKEY,WORK+1        NOTE THE LOCATION "WORK+1"                   
         MVI   BITM.PRCODE,PRCODEQU                                             
         MVI   BITM.PRMED,C'N'                                                  
         MVI   BITM.PRSRC,C'N'                                                  
         MVC   BITM.PRSTAT(4),=C'PPPP'                                          
         DROP  BITM                                                             
         MVC   WORK+8(3),=C')01'   LOWEST POSSIBLE NTI BITMAP KEY               
*                                                                               
PR15     DS    0H                                                               
         GOTO1 =V(DECODE),DMCB,(DFILDKYL,WORK),(DFILKFLC,MAJORKEY),0,0,+        
               RR=RELO                                                          
         CLI   8(R1),X'FF'                                                      
         BNE   PR20                                                             
         XC    CONHEAD,CONHEAD                                                  
         L     RF,8(R1)            A(30-BYTE DECODE ERROR MESSAGE)              
         MVC   CONHEAD(30),0(RF)                                                
         B     TRAPERR2                                                         
*                                                                               
PR20     DS    0H                                                               
         CLC   =C'NTIBITMAPS',DEMMJKY SPECIAL CALL TO DUMP BITMAP RECS?         
         BE    PR40                YES                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),DFILDIRN,MAJORKEY,AIO2               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2             RETURNED DIRECTORY RECORD                    
         LLC   R0,DFILDKYL         DISPLAY FULL KEY IN TWA                      
         GOTO1 HEXOUT,DMCB,AIO2,DEMMJKY,(R0),=C'TOG'                            
         OI    DEMMJKYH+6,X'80'    XMIT                                         
*                                                                               
         TM    18(R3),X'40'        EXTENDED PASSIVE?                            
         BO    PRX                 YES: DON'T TRY TO READ THE FILE              
         MVC   FULL,19(R3)         SAVE DISK ADDRESS                            
         OC    FULL,FULL           REGULAR PASSIVE?                             
         BZ    PRX                 YES: DON'T TRY TO READ THE FILE              
*                                                                               
         IF (CLC,MAJORKEY(18),EQ,0(R3))   TEST KEY FOUND FROM DMRDHI            
           MVC P(21),=C'EXACT MAJOR KEY FOUND'                                  
         ELSE ,                                                                 
           MVC P(44),=C'EXACT MAJOR KEY NOT FOUND. DMRDHI PERFORMED.'           
         ENDIF ,                                                                
         BRAS  RE,GOSPOOL          PRINT THE MESSAGE                            
         MVI   P,0                 SKIP A LINE                                  
         BRAS  RE,GOSPOOL                                                       
*                                                                               
         MVC   22(1,R3),18(R3)                                                  
         XC    18(4,R3),18(R3)                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),DFILFILN,FULL,AIO2,0                 
*                                                                               
PR30     CLI   8(R1),X'80'         NO MORE MINOR KEYS?                          
         BE    PRX                                                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRTREC),DMCB,AIO2,(23,20),VPRINT,HEXOUT,RR=RELO               
         MVI   P,0                                                              
         BRAS  RE,GOSPOOL                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),DFILFILN,FULL,AIO2,0                 
         B     PR30                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
PR40     DS    0H                                                               
*                                                                               
* PRODUCE A SEQUENTIAL FILE CONSISTING OF NOTHING BUT THE NTI BITMAP            
* RECORDS. THIS IS USED DURING THE OVERNIGHT PROCESS TO INITIALIZE THE          
* TEST NTIFIL. IT IS ALSO USED TO MONITOR THE USAGE OF THE NTI #S, SO           
* THAT WE HAVE TIME TO RECYCLE OLD NUMBERS BEFORE WE RUN OUT.                   
*                                                                               
         OPEN  (BITMAPS,OUTPUT)                                                 
*                                                                               
         MVI   BYTE,C'H'           KEY = PNNPPPPH                               
         LR    R3,RC               PRESERVE A(GEND)                             
         BRAS  RE,RDBITMAP         PROCESS BITMAP                               
*                                                                               
         MVI   BYTE,C'N'           KEY = PNNPPPPN                               
         LR    R3,RC               PRESERVE A(GEND)                             
         BRAS  RE,RDBITMAP         PROCESS BITMAP                               
*                                                                               
         MVI   BYTE,C'S'           KEY = PNNPPPPS                               
         LR    R3,RC               PRESERVE A(GEND)                             
         BRAS  RE,RDBITMAP         PROCESS BITMAP                               
*                                                                               
         CLOSE BITMAPS                                                          
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE A BOOK                                                               
*                                                                               
VALBOOK  NTR1                                                                   
         CLC   =C'LAT',FLD         LATEST BOOK REQUEST                          
         BE    VALBKL               YES - VALIDATE MULTI                        
         GOTO1 DATVAL,DMCB,(2,FLD),WORK                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    VALBOOK2                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB),0                                   
         B     OKEXIT                                                           
VALBOOK2 TM    FLDH+4,X'08'                                                     
         BZ    EIIF                                                             
         CLI   FLDH+5,4                                                         
         BNE   EIIF                                                             
         PACK  DUB,FLD(2)          YEAR                                         
         CVB   R0,DUB                                                           
         CHI   R0,27                                                            
         BH    *+8                                                              
         AHI   R0,100              ADD X'64' TO ADJUST FOR Y2K                  
         PACK  DUB,FLD+2(2)        WEEK                                         
         CVB   R1,DUB                                                           
         STC   R0,DUB                                                           
         STC   R1,DUB+1                                                         
         B     OKEXIT                                                           
*                                                                               
* VALIDATE LATEST BOOK EXPRESSIONS                                              
*                                                                               
VALBKL   XC    DUB(2),DUB                                                       
         TM    FLD+3,X'F0'         MULTI BOOK AVERAGE                           
         BNO   OKEXIT                                                           
         CLI   FLD+3,C'1'          MUST BE AT LEAST 1 BUT                       
         BL    EIIF                                                             
         CLI   FLD+3,C'4'          NO MORE THAN 4                               
         BH    EIIF                                                             
         MVI   DUB,X'FF'           INDICATE LATEST N                            
         MVC   DUB+1(1),FLD+3                                                   
         B     OKEXIT                                                           
         EJECT                                                                  
* VALIDATE BOOKTYPE                                                             
*                                                                               
VALBTYP  NTR1                                                                   
         CLI   FLDH+5,1                                                         
         BE    VBT1CH                                                           
         CLI   FLDH+5,2                                                         
         BE    VBT2CH                                                           
         B     EIIF                                                             
*                                                                               
VBT1CH   DS    0H                  VALIDATE FOR 1-CHAR INPUT                    
         MVC   DUB(1),FLD           TAKE INPUT AS IS                            
         B     VBTX                                                             
*                                                                               
VBT2CH   DS    0H                  VALIDATE FOR 2-CHAR INPUT                    
         TM    FLDH+4,X'02'         VALIDATE AS HEX INPUT                       
         BZ    EIIF                                                             
                                                                                
         GOTO1 HEXIN,DMCB,FLD,DUB,2,0                                           
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     VBTX                                                             
*                                                                               
VBTX     DS    0H                                                               
         B     OKEXIT                                                           
*                                                                               
* VALIDATE ACTUAL DATE (1WEEK OPTION)                                           
*                                                                               
VAL1WK   NTR1                                                                   
         GOTO1 DATVAL,DMCB,(0,FLD),WORK                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    EIIF                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(2,DUB)                                     
         B     OKEXIT                                                           
*                                                                               
* VALIDATE ACTUAL WEEK (1 THRU 4 OR ALL)                                        
*                                                                               
VALWKN   NTR1                                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+12                                                             
         MVI   DUB,X'FF'                                                        
         B     OKEXIT                                                           
         TM    FLDH+4,X'08'                                                     
         BZ    EIIF                                                             
         CLI   FLDH+5,1                                                         
         BNE   EIIF                                                             
         MVC   DUB(1),FLD                                                       
         NI    DUB,X'0F'                                                        
         B     OKEXIT                                                           
*                                                                               
* VALIDATE DAY EXPRESSION                                                       
*                                                                               
VALDAY   NTR1                                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BNE   VALDAY10                                                         
         CLI   DBSELSRC,C'R'                                                    
         BNE   *+12                                                             
         MVI   DUB,X'81'                                                        
         B     *+8                                                              
         MVI   DUB,X'FF'                                                        
*                                                                               
         B     OKEXIT                                                           
VALDAY10 CLC   FLD(3),=C'VAR'                                                   
         BNE   *+12                                                             
         MVI   DUB,X'90'                                                        
         B     OKEXIT                                                           
*                                  VALIDATE FOR "AVN" OR "AVGN"                 
         LLC   RF,FLDH+5                                                        
         SHI   RF,2                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   FLD(0),=C'AVG'       LOOK FOR "AV" OR "AVG"                      
         BNE   VDAYAVGX              NOPE                                       
         LA    RE,FLD+1(RF)         RE-->LAST CHAR OF INPUT                     
         CLI   0(RE),C'2'           SHOULD BE BETWEEN 2                         
         BL    VDAYAVGX                                                         
         CLI   0(RE),C'6'            AND 6 DAYS                                 
         BH    VDAYAVGX                                                         
         MVC   DUB(1),0(RE)                                                     
         NI    DUB,X'9F'            X'90' BITS ==> VAR OR AVG-N                 
         B     OKEXIT                                                           
VDAYAVGX EQU   *                                                                
*                                                                               
         LLC   R0,FLDH+5                                                        
         GOTO1 VDAYPAK,DMCB,((R0),FLD),DUB,=X'17'                               
         CLI   DUB,0                                                            
         BE    EIIF                                                             
         B     OKEXIT                                                           
*                                                                               
* VALIDATE TIME EXPRESSION                                                      
*                                                                               
VALTIME  NTR1                                                                   
         CLI   DBFUNCT,DBGETOPI                                                 
         BNE   *+14                                                             
         MVC   DUB(6),FLD                                                       
         B     OKEXIT                                                           
*                                                                               
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+14                                                             
         MVC   DUB(4),=AL2(0600,2600)                                           
         B     OKEXIT                                                           
         LLC   R0,FLDH+5                                                        
         GOTO1 VTIMVAL,DMCB,((R0),FLD),DUB                                      
         CLI   0(R1),X'FF'                                                      
         BE    EIIF                                                             
         B     OKEXIT                                                           
*                                                                               
* VALIDATE PURE NUMBER                                                          
*                                                                               
VALPURE  NTR1                                                                   
         TM    FLDH+4,X'08'                                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         PACK  DUB,FLD(2)                                                       
         CVB   R1,DUB                                                           
         PACK  DUB,FLD+2(1)                                                     
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         PACK  DUB,FLD+3(1)                                                     
         CVB   RE,DUB                                                           
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         STC   RE,DUB+2                                                         
         OC    DUB+1(1),DUB+2                                                   
         B     OKEXIT                                                           
*                                                                               
* VALIDATE DEMOS                                                                
*                                                                               
VALDEMO  NTR1                                                                   
         LA    R1,DEMDEMOH                                                      
         ST    R1,FADR                                                          
         CLC   FLD(6),=C'MAXIO='   TEST MAXIO=NNNN INPUT                        
         BNE   VALDEMO2                                                         
         LLC   R1,FLDH+5                                                        
         SHI   R1,7                                                             
         BM    EIIF                                                             
         MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD+6                                                     
         CLC   DUB,=8C'0'                                                       
         BNE   EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+6(0)                                                     
         CVB   R1,DUB                                                           
         ST    R1,MAXIO                                                         
         B     OKEXIT                                                           
VALDEMO2 CLI   FLDH+5,4            TEST FOR XALL (X=MODIFIER)                   
         BNE   VALDEMO6                                                         
         CLC   FLD+1(3),=C'ALL'                                                 
         BNE   VALDEMO6                                                         
         LA    RE,DEMOLIST         BUILD LIST CONTAING ALL DEMOS                
         LAY   R1,ALLDEMOS                                                      
VALDEMO4 CLI   0(R1),255                                                        
         BE    OKEXIT                                                           
         MVI   0(RE),0             PRECISION                                    
         MVC   1(1,RE),FLD         MODIFIER                                     
         MVC   2(1,RE),0(R1)       DEMO                                         
         LA    RE,3(RE)            BUMP TO NEXT DEMOLIST ENTRY                  
         MVI   0(RE),X'FF'         SET E-O-L                                    
         LA    R1,1(R1)                                                         
         B     VALDEMO4                                                         
VALDEMO6 MVI   DBDEMTYP,0                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,DEMDEMOH,(10,DEMOLIST),DBLOCK                          
         CLI   4(R1),0                                                          
         BNE   OKEXIT                                                           
         B     EIIF                                                             
*                                                                               
* VALIDATE PRINT CONTROL #2                                                     
*                                                                               
VALPRINT NTR1                                                                   
         CLI   FLD,PKEYONLY        KEYS ONLY                                    
         BE    *+12                                                             
         CLI   FLD,PDEFAULT        DEFAULT TO PRINT ENTIRE RECORD               
         BNE   EIIF                                                             
         MVC   PCTRL2,FLD                                                       
         B     OKEXIT                                                           
*                                                                               
* VALIDATE ELEMENT FILTER LIST                                                  
*                                                                               
VALEFILT NTR1                                                                   
         CLI   PCTRL2,C' '         CAN NOT HAVE BOTH PRINT?                     
         BNE   ELPRERR              AND ELEM FILTER FIELD                       
*                                                                               
         L     RE,AIO3             USE AIO3 FIELD AS A SCAN BLOCK               
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         GOTO1 SCANNER,DMCB,DEMEFLTH,(7,AIO3),C',=,-'                           
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LLC   R0,4(R1)            R0 = # OF ENTRIES                            
         STC   R0,EFLTLST                                                       
         L     R2,AIO3                                                          
         LA    R3,EFLTLST+1                                                     
*                                                                               
VALEFLT2 CLI   0(R2),0             MUST HAVE SOMETHING IN 1ST FIELD             
         BE    EIIF                                                             
         CLI   0(R2),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         BH    EIIF                                                             
         TM    2(R2),X'20'          MUST BE VALID HEX                           
         BZ    EIIF                                                             
         LLC   R4,0(R2)            R4 = L(ENTRY)                                
         GOTO1 HEXIN,DMCB,12(R2),0(R3),(R4),0                                   
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         MVC   1(1,R3),0(R3)       FILL IN DEFAULT END OF RANGE                 
*                                                                               
         CLI   1(R2),0             IS THERE A SECOND HALF?                      
         BE    VALEFLT4             NOPE                                        
         CLI   1(R2),2              YEP                                         
         BH    EIIF                                                             
         TM    3(R2),X'20'         MUST BE VALID HEX                            
         BZ    EIIF                                                             
         LLC   R4,1(R2)                                                         
         GOTO1 HEXIN,DMCB,22(R2),1(R3),(R4),0                                   
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         CLC   0(1,R3),1(R3)       MAKE SURE LO <= HI                           
         BH    EIIF                                                             
*                                                                               
VALEFLT4 LA    R2,32(R2)           DO NEXT ENTRY                                
         LA    R3,2(R3)                                                         
         BCT   R0,VALEFLT2                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
IUNFILT  NTR1                                                                   
         MVI   IUNFLG,C'Y'                                                      
         L     RE,AIO3             USE AIO3 FIELD AS A SCAN BLOCK               
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         CLI   DEMIUNFH+5,1                                                     
         BNE   IUNFLT1                                                          
         CLI   DEMIUNF,C'K'        ONLY PRINT KEY?                              
         BNE   *+12                                                             
         MVI   PCTRLI,C'K'                                                      
         B     OKEXIT                                                           
         CLI   DEMIUNF,C'Y'        CONVERT TO IUN FORMAT?                       
         BE    OKEXIT                                                           
*                                                                               
IUNFLT1  GOTO1 SCANNER,DMCB,DEMIUNFH,(7,AIO3),C',=,-'                           
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LLC   R0,4(R1)            R0 = # OF ENTRIES                            
         STC   R0,IUNFLST                                                       
         L     R2,AIO3                                                          
         LA    R3,IUNFLST+1                                                     
*                                                                               
IUNFLT2  CLI   0(R2),0             MUST HAVE SOMETHING IN 1ST FIELD             
         BE    EIIF                                                             
         CLI   0(R2),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         BH    EIIF                                                             
         TM    2(R2),X'20'          MUST BE VALID HEX                           
         BZ    EIIF                                                             
         LLC   R4,0(R2)            R4 = L(ENTRY)                                
         GOTO1 HEXIN,DMCB,12(R2),0(R3),(R4),0                                   
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         MVC   1(1,R3),0(R3)       FILL IN DEFAULT END OF RANGE                 
*                                                                               
         CLI   1(R2),0             IS THERE A SECOND HALF?                      
         BE    IUNFLT4              NOPE                                        
         CLI   1(R2),2              YEP                                         
         BH    EIIF                                                             
         TM    3(R2),X'20'         MUST BE VALID HEX                            
         BZ    EIIF                                                             
         LLC   R4,1(R2)                                                         
         GOTO1 HEXIN,DMCB,22(R2),1(R3),(R4),0                                   
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         CLC   0(1,R3),1(R3)       MAKE SURE LO <= HI                           
         BH    EIIF                                                             
*                                                                               
IUNFLT4  LA    R2,32(R2)           DO NEXT ENTRY                                
         LA    R3,2(R3)                                                         
         BCT   R0,IUNFLT2                                                       
         B     OKEXIT                                                           
         EJECT                                                                  
* GENERATE REPORT                                                               
*                                                                               
SP16     DS    0H                                                               
         CLI   DBFUNCT,0           SPECIAL FUNCTION REQUESTED?                  
         BNE   SP18X                NOPE                                        
         LAY   RE,SPFNCTAB                                                      
         LLC   R1,DEMFUNCH+5                                                    
         BCTR  R1,0                                                             
SP18A    EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   DEMFUNC(0),0(RE)                                                 
         BE    SP18B                                                            
         LA    RE,L'SPFNCTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   SP18A                                                            
         DC    H'0'                                                             
SP18B    MVC   BYTE1,11(RE)         GET THE FLAG                                
         SR    RF,RF                                                            
         ICM   RF,7,8(RE)                                                       
         A     RF,RELO              AND ADDRESS OF SPECIAL FUNCTION             
         BASR  RE,RF                                                            
         B     SP20                                                             
*                                                                               
SP18X    DS    0H                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,AIO3                                                          
         ST    RE,DBAREC                                                        
         TM    DBBTYPE,X'F0'                                                    
         BO    *+8                                                              
         B     *+8                                                              
         NI    DBBTYPE,X'0F'                                                    
*                                                                               
*********CLI   DBSELMED,C'N'                                                    
*********BNE   *+20                                                             
*********TM    DBSTYPE,X'F0'                                                    
*********BO    *+8                                                              
*********B     *+8                                                              
*********NI    DBSTYPE,X'0F'                                                    
*                                                                               
         XC    LACTUAL,LACTUAL                                                  
         XC    LSELECT,LSELECT                                                  
         LAY   RE,DYTMLIST         TEST DAY/TIME LIST                           
         J     *+8                                                              
         ST    RE,DBEXTEND                                                      
*********LAY   RE,PURELIST         TEST PURE LIST                               
*********J     *+8                                                              
*********ST    RE,DBEXTEND                                                      
*********LAY   RE,MBKSLIST         TEST MULTI-BOOKS LIST                        
*********J     *+8                                                              
*********ST    RE,DBEXTEND                                                      
         LAY   RE,SPOTLIST         TEST SPOT PRECISION LIST                     
         J     *+8                                                              
         ST    RE,DBEXTEND                                                      
         LAY   RE,UIDLIST          TEST USER ID PARAMETER                       
         J     *+8                                                              
         ST    RE,DBEXTEND                                                      
         CLC   DBFILE,=C'IUN'                                                   
         JNE   *+12                                                             
         LA    RE,DBXTND1                                                       
         ST    RE,DBEXTEND                                                      
*                                                                               
         CLI   DBSELSTA,C'H'                                                    
         JNE   SP19A                                                            
         CLC   DBSELSTA+1(4),=C'0000'                                           
         JL    SP19A                                                            
         CLC   DBSELSTA+1(4),=C'9999'                                           
         JH    SP19A                                                            
*                                                                               
         MVC   PACK8,=8C'0'                                                     
         MVC   PACK8(4),DBSELSTA+1                                              
         PACK  DUB(4),PACK8(5)                                                  
         MVC   DBSELSTA+1(3),DUB   PWOS                                         
         MVI   DBSELSTA+4,C'C'                                                  
*                                                                               
         MVI   DBSELDUR,X'FF'                                                   
*                                                                               
SP19A    L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,MYRTN,C'*IO*',MYTRC                             
*                                                                               
SP19     CLI   DBERROR,0                                                        
         JNE   *+8                                                              
         MVI   DBERROR,X'80'                                                    
         BAS   RE,MYRTN                                                         
*                                                                               
SP20     DS    0H                  EXIT APP                                     
         J     OKEXIT                                                           
         EJECT                                                                  
* DEMAND TRACE ROUTINE                                                          
*                                                                               
MYRTN    NTR1  WORK=(R6,MYTRCDQ)                                                
         USING MYTRCD,R6                                                        
         LA    R2,P                                                             
         CLI   DBERROR,0                                                        
         BE    MYRTN2                                                           
         LAY   RF,ERRTAB                                                        
         LA    R1,DBERROR                                                       
         BAS   RE,GETNTRY                                                       
         MVC   0(6,R2),=C'ERROR='                                               
         MVC   6(8,R2),1(RF)                                                    
         LA    R2,15(R2)                                                        
         BAS   RE,GETNEXT                                                       
         LAY   RF,MODTAB                                                        
         LA    R1,DBERRMOD                                                      
         BAS   RE,GETNTRY                                                       
         MVC   0(7,R2),=C'ERRMOD='                                              
         MVC   7(8,R2),1(RF)                                                    
         LA    R2,15(R2)                                                        
         B     MYRTN3                                                           
*                                                                               
MYRTN2   DS    0H                                                               
         LAY   RF,RECTAB                                                        
         LA    R1,DBRECTYP                                                      
         BAS   RE,GETNTRY                                                       
         MVC   0(8,R2),=C'RECTYPE='                                             
         MVC   8(8,R2),1(RF)                                                    
         LA    R2,17(R2)                                                        
         BAS   RE,GETNEXT                                                       
         MVC   0(7,R2),=C'MODE=0,'                                              
         OC    5(1,R2),DBMODE                                                   
         LA    R2,7(R2)                                                         
         MVC   0(7,R2),=C'FACTOR='                                              
         SR    R1,R1                                                            
         ICM   R1,3,DBFACTOR                                                    
         CVD   R1,MYTDUB                                                        
         UNPK  7(4,R2),MYTDUB                                                   
         OI    10(R2),X'F0'                                                     
         MVI   11(R2),C','                                                      
         LA    R2,12(R2)                                                        
         MVC   0(7,R2),=C'ELDISP='                                              
         L     R1,DBAQUART                                                      
         S     R1,DBAREC                                                        
         BM    MYRTN3+4                                                         
         CVD   R1,MYTDUB                                                        
         UNPK  7(4,R2),MYTDUB                                                   
         OI    10(R2),X'F0'                                                     
         LA    R2,12(R2)                                                        
*                                                                               
MYRTN3   BAS   RE,GETNEXT                                                       
         MVC   0(5,R2),=C'OPTS='                                                
         OC    DBOPTS,DBOPTS                                                    
         BNZ   *+14                                                             
         MVC   5(6,R2),=C'*NONE*'                                               
         B     *+10                                                             
         MVC   5(24,R2),DBOPTS                                                  
         OC    5(24,R2),SPACES                                                  
         LA    R2,29(R2)                                                        
         BAS   RE,GETNEXT                                                       
         MVC   0(7,R2),=C'DIVSOR='                                              
         SR    R1,R1                                                            
         ICM   R1,3,DBDIVSOR                                                    
         CVD   R1,MYTDUB                                                        
         UNPK  7(4,R2),MYTDUB                                                   
         OI    10(R2),X'F0'                                                     
         MVI   P2,0                                                             
         BRAS  RE,GOSPOOL                                                       
*                                                                               
         CLI   DBFUNCT,DBACSALL                                                 
         BNE   MYRTN3A                                                          
         CLI   DBERROR,0                                                        
         BNE   MYRTN3A                                                          
         L     R3,DBAREC                                                        
         SR    R0,R0                                                            
         ICM   R0,7,10(R3)         LENGTH OF ENTRY                              
         BZ    MYRTN3A                                                          
         CHI   R0,1000                                                          
         BH    MYRTN3A                                                          
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   P+7(0),0(R3)                                                     
         GOTO1 HEXOUT,DMCB,(R3),P2+7,(R0),=C'TOG',0                             
         MVC   P3+7(7),SPACES                                                   
         BRAS  RE,GOSPOOL                                                       
*                                                                               
MYRTN3A  MVC   P(7),=C'SELECT='                                                 
         LA    R2,DBSELECT                                                      
         LA    R3,LSELECT                                                       
         LA    R0,2                                                             
*                                                                               
MYRTN4   CLC   0(64,R2),0(R3)                                                   
         BE    MYRTN5                                                           
         MVC   P+7(64),0(R2)                                                    
         GOTO1 HEXOUT,MYTDMCB,P+7,MYTWORK1,64,=C'SEP'                           
         GOTO1 =V(DUMPOUT),MYTDMCB,(64,P+7),0,0,RR=RELO                         
         MVC   P2+7(64),MYTWORK1                                                
         MVC   P3+7(64),MYTWORK1+64                                             
         MVI   P4,0                                                             
         BRAS  RE,GOSPOOL                                                       
*                                                                               
MYRTN5   MVC   0(64,R3),0(R2)                                                   
         LA    R3,64(R3)                                                        
         LA    R2,64(R2)                                                        
         MVC   P(7),=C'ACTUAL='                                                 
         BCT   R0,MYRTN4                                                        
*                                                                               
         CLC   =AL2(DBDQUXTD),DBDQD                                             
         BNE   MYRTNDQX                                                         
         LA    RF,DBEXTEND-4                                                    
MYRTNDQG DS    0H                     LOOK THROUGH DBEXTEND                     
         ICM   RF,15,4(RF)                                                      
         BZ    MYRTNDQX                                                         
         CLC   0(4,RF),DBDQD+2        FOR NAME GIVEN HERE                       
         BNE   MYRTNDQG                                                         
         LA    R2,8(RF)                                                         
*                                                                               
         MVC   P+0(07),=C'XDBDQD='                                              
         MVC   P+7(64),0(R2)                                                    
         GOTO1 =V(DUMPOUT),MYTDMCB,(64,P+7),0,0,RR=RELO                         
         GOTO1 HEXOUT,MYTDMCB,(R2),MYTWORK1,64,=C'SEP'                          
         MVC   P2+7(64),MYTWORK1                                                
         MVC   P3+7(64),MYTWORK1+64                                             
         MVI   P4,0                                                             
         BRAS  RE,GOSPOOL                                                       
MYRTNDQX EQU   *                                                                
*                                                                               
         MVC   P(7),SPACES                                                      
         MVI   P2,0                                                             
         BRAS  RE,GOSPOOL                                                       
         CLI   DEMOLIST,X'FF'                                                   
         BE    OKEXIT                                                           
         CLI   DBRECTYP,DBRECNTI                                                
         BE    *+8                                                              
         CLI   DBRECTYP,DBRECDEM                                                
         BNE   OKEXIT                                                           
         CLI   DBERROR,0                                                        
         BNE   OKEXIT                                                           
*                                                                               
         MVI   IUNPRT,0                                                         
         XC    DEMOVALS(240),DEMOVALS                                           
         XC    DEMOVALS+240(240),DEMOVALS+240                                   
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DEMOVALS                        
         LA    R2,DEMOLIST                                                      
MYRTN5A  LA    R3,DEMOVALS                                                      
         LA    R4,P                                                             
         LA    R5,6                MAX 6 DEMOS PER LINE                         
*                                                                               
MYRTN6   CLI   0(R2),X'FF'                                                      
         BE    MYRTN7                                                           
         MVC   0(1,R4),1(R2)                                                    
         OI    0(R4),X'40'                                                      
         EDIT  (B1,2(R2)),(3,1(R4)),FILL=0                                      
         MVI   4(R4),C'='                                                       
         EDIT  (B4,0(R3)),(7,5(R4)),ALIGN=LEFT                                  
         LA    R4,13(R4)                                                        
         BCT   R5,MYRTN6A                                                       
         BRAS  RE,GOSPOOL          PRINT LINE & SET-UP FOR NEXT                 
         LA    R4,P                                                             
         LA    R5,6                                                             
MYRTN6A  LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         B     MYRTN6                                                           
*                                                                               
MYRTN7   CLC   P,SPACES                                                         
         BE    MYRTN7A                                                          
         BRAS  RE,GOSPOOL                                                       
*                                                                               
MYRTN7A  DS    0H                                                               
*********CLI   DBSELMED,C'N'       NETWORK FILE?                                
*********BNE   OKEXIT                                                           
         CLI   IUNFLG,C'Y'         CONVERT TO IUN FORMAT?                       
         BNE   OKEXIT                                                           
         CLI   IUNPRT,0                                                         
         BNE   OKEXIT              ALREADY PROCESSED                            
         BRAS  RE,CNVIUN           CONVERT NET RECD TO IUN FMT                  
         MVI   IUNPRT,1                                                         
         LA    R2,DEMOLST2         USE IUN DEMOLST2 BUILT IN CNVIUN             
         B     MYRTN5A             PRINT IUN DEMOS                              
*                                                                               
GETNTRY  CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLC   0(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    RF,9(RF)                                                         
         B     GETNTRY                                                          
*                                                                               
GETNEXT  CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 3                                                                
GOSPOOL  NTR1                                                                   
         LAY   RF,VALCHARS                                                      
         TR    P,0(RF)             REPLACE DICTATE ESCAPE CHARS W NULLS         
         TR    P2,0(RF)                                                         
         TR    P3,0(RF)                                                         
         TR    P4,0(RF)                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PRINT I/O TRACE                                                    
*                                                                               
MYTRC    NTR1  WORK=(R6,MYTRCDQ)                                                
         USING MYTRCD,R6                                                        
         LR    R2,R1                                                            
*                                  CLEAR WORK AREA                              
         LR    R0,R6                                                            
         LA    R1,MYTRCDQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                  GET PARAMETERS                               
         SR    R1,R1                                                            
         ICM   R1,7,1(R2)                                                       
         MVC   MYTIOCMD,0(R1)       DATAMGR COMMAND                             
         ICM   R1,7,5(R2)                                                       
         MVC   MYTFILNM,0(R1)       NAME OF FILE                                
         ICM   R1,7,9(R2)                                                       
         ST    R1,MYTIADR           A(INPUT TO DATAMGR)                         
         ICM   R1,7,13(R2)                                                      
         ST    R1,MYTOADR           A(OUTPUT FROM DATAMGR)                      
         LA    R0,L'DBKEY                                                       
         LH    R1,DBDTADSP                                                      
         CLC   DBFILE,=C'IUN'                                                   
         BNE   *+12                                                             
         LA    R0,L'RINVKEY                                                     
         LA    R1,RINVPEL-RINVREC                                               
         STH   R0,MYTKYLEN         L(KEY)                                       
         STH   R1,MYTDS1EL         DISPL TO 1ST ELEMENT                         
*                                                                               
** PRINT GENERAL DETAILS OF I/O **                                              
*                                                                               
         LA    R4,P                                                             
         MVC   0(L'MYTIOCMD,R4),MYTIOCMD                                        
         LA    R4,L'MYTIOCMD+1(R4)                                              
         MVC   0(L'MYTFILNM,R4),MYTFILNM                                        
         LA    R4,L'MYTFILNM+1(R4)                                              
         GOTO1 HEXOUT,MYTDMCB,8(R2),(R4),1,=C'TOG'                              
*                                                                               
** PRINT FILE-SPECIFIC DETAILS OF I/O **                                        
*                                                                               
         CLC   DBFILNAM,MYTFILNM                                                
         BE    MYTRC2                                                           
         CLC   DBDIRNAM,MYTFILNM                                                
         BE    MYTRC1B                                                          
         TM    0(R2),X'80'                                                      
         BZ    MYTRC2                                                           
*                                                                               
*** DIRECTORY INFORMATION ***                                                   
*                                                                               
MYTRC1B  DS    0H                                                               
         LA    R4,P+19                                                          
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         L     RF,MYTIADR                                                       
         MVC   0(5,R4),=C'IKEY='                                                
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   5(0,R4),0(RF)                                                    
         LA    R5,1(R5)                                                         
         GOTO1 HEXOUT,MYTDMCB,(RF),MYTWORK1,(R5),=C'SEP'                        
         LA    R4,5(R5,R4)                                                      
*                                                                               
         LA    R5,5(R5)            BUMP LENGTH UP FOR CNTL & D/A                
         BCTR  R5,0                                                             
         L     RF,MYTOADR                                                       
         MVC   0(6,R4),=C',OKEY='                                               
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   6(0,R4),0(RF)                                                    
         LA    R5,1(R5)                                                         
         GOTO1 HEXOUT,MYTDMCB,(RF),MYTWORK2,(R5),=C'SEP'                        
         GOTO1 =V(DUMPOUT),MYTDMCB,(L'P,P),0,0,RR=RELO                          
*                                  DISPLAY HEX OUTPUT                           
         LA    R4,P2+24                                                         
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),MYTWORK1                                                 
         LA    R4,6+1(R5,R4)                                                    
         LA    R5,5(R5)                                                         
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),MYTWORK2                                                 
*                                                                               
         LA    R4,P3+24                                                         
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         LA    RF,MYTWORK1+1(R5)                                                
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),0(RF)                                                    
         LA    R4,6+1(R5,R4)                                                    
         LA    R5,5(R5)                                                         
         LA    RF,MYTWORK2+1(R5)                                                
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),0(RF)                                                    
         MVI   P4,0                                                             
         BRAS  RE,GOSPOOL                                                       
         B     MYTRCX                                                           
*                                                                               
MYTRC2   MVC   P+19(5),=C'NXDA='                                                
         LA    R0,DBNDXDA                                                       
         CLC   DBFILE,=C'IUN'                                                   
         BNE   MYTRC2B                                                          
         L     R0,MYTIADR                                                       
*                                                                               
MYTRC2B  DS    0H                  R0-->DISK ADDRESS                            
         GOTO1 HEXOUT,MYTDMCB,(R0),P+24,4,=C'TOG'                               
*                                                                               
MYTRC4   MVC   P+32(6),=C',IKEY='                                               
         CLC   DBFILE,=C'IUN'                                                   
         BE    MYTRC4B                                                          
         MVC   P+38(18),DBKEY                                                   
         MVC   P+56(2),DBMINKEY                                                 
         MVC   P+60(9),=C'DBSTATUS='                                            
         GOTO1 HEXOUT,MYTDMCB,DBSTATUS,P+69,1,=C'TOG'                           
         GOTO1 HEXOUT,MYTDMCB,P+38,MYTWORK1,20,=C'SEP'                          
         GOTO1 =V(DUMPOUT),MYTDMCB,(20,P+38),0,0,RR=RELO                        
         MVC   P2+38(20),MYTWORK1                                               
         MVC   P3+38(20),MYTWORK1+20                                            
         MVI   P4,0                                                             
         BRAS  RE,GOSPOOL                                                       
         B     MYTRC6                                                           
*                                                                               
MYTRC4B  DS    0H                                                               
         L     R3,MYTOADR          R3-->OUTPUT KEY FROM DATAMGR                 
         MVC   P+38(34),0(R3)                                                   
         GOTO1 HEXOUT,MYTDMCB,P+38,MYTWORK1,34,=C'SEP'                          
         MVC   P2+38(34),MYTWORK1                                               
         MVC   P3+38(34),MYTWORK1+34                                            
         MVI   P4,0                                                             
         BRAS  RE,GOSPOOL                                                       
*                                                                               
MYTRC6   DS    0H                                                               
         CLC   DBFILNAM,MYTFILNM                                                
         BE    MYTRC6B                                                          
         TM    8(R2),X'80'                                                      
         BNZ   MYTRCX                                                           
*                                                                               
MYTRC6B  DS    0H                                                               
         CLI   PCTRL2,PKEYONLY     PRINT KEYS ONLY?                             
         BE    MYTRCX               YES, SO EXIT                                
         MVC   P(4),=C'OREC'                                                    
         L     R3,MYTOADR                                                       
         SR    R5,R5                                                            
         LH    R4,MYTDS1EL                                                      
         MVI   WORK,1                                                           
         BAS   RE,MYTRC8                                                        
         EJECT                                                                  
MYTRC7   CLI   0(R3),0                                                          
         BE    MYTRCX                                                           
         LLC   R4,1(R3)                                                         
         CLI   EFLTLST,0           ANY ELEM TO FILTER?                          
         BE    MYTRC7B              NO, PRINT ELEM                              
         LLC   RF,EFLTLST           YES, CHECK IF ELEM SELECTED                 
         LA    RE,EFLTLST+1        RF=COUNTER, RE-->START OF LIST               
MYTRC7A  CLC   0(1,R3),0(RE)                                                    
         BL    *+14                                                             
         CLC   0(1,R3),1(RE)                                                    
         BNH   MYTRC7B             ELEM SELECTED, GO PRINT ELEMENT              
         LA    RE,2(RE)                                                         
         BCT   RF,MYTRC7A                                                       
         AR    R3,R4               ELEM CODE IS NOT IN FILTER LIST              
         AR    R5,R4               UPDATE POSITION IN RECORD                    
         B     MYTRC7                                                           
MYTRC7B  MVI   WORK,0                                                           
         BAS   RE,MYTRC8                                                        
         B     MYTRC7                                                           
*                                                                               
MYTRC8   ST    RE,FULL             SAVE RETURN ADDRESS                          
MYTRC9   LA    R0,32                                                            
         CR    R4,R0                                                            
         BH    *+6                                                              
         LR    R0,R4                                                            
         MVI   P+4,C'('                                                         
         CVD   R5,MYTDUB                                                        
         UNPK  P+5(3),MYTDUB                                                    
         OI    P+7,X'F0'                                                        
         MVI   P+8,C'-'                                                         
         AR    R5,R0                                                            
         BCTR  R5,0                                                             
         CVD   R5,MYTDUB                                                        
         UNPK  P+9(3),MYTDUB                                                    
         OI    P+11,X'F0'                                                       
         MVI   P+12,C')'                                                        
         LA    R5,1(R5)                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLI   WORK,1                                                           
         BNE   MYTRC15                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),0(R3)                                                    
         B     MYTRC20                                                          
*                                                                               
MYTRC15  DS    0H                                                               
         MVI   WORK,1                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+18(0),2(R3)                                                    
*                                                                               
MYTRC20  DS    0H                                                               
         BRAS  RE,GOSPOOL                                                       
         GOTO1 HEXOUT,MYTDMCB,(R3),P+14,(R0),=C'TOG'                            
         BRAS  RE,GOSPOOL                                                       
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         SR    R4,R0               TEST IF ANY LEFT OF CURRENT                  
         BNZ   MYTRC9                                                           
         BRAS  RE,GOSPOOL                                                       
         L     RE,FULL             RESTORE RETURN ADDRESS                       
         BR    RE                                                               
*                                  EXIT FROM TRACE ROUTINE                      
MYTRCX   L     R1,MAXIO            DECREMENT MAXIMUM I/O COUNT                  
         SHI   R1,1                                                             
         BNP   *+12                                                             
         ST    R1,MAXIO                                                         
         B     OKEXIT                                                           
         MVI   DBERROR,X'FE'       SET FORCED E-O-F                             
         L     RD,SAVERD           RESTORE REGISTERS                            
         LM    RE,RC,12(RD)                                                     
         B     SP19                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*SPECIAL FUNCTION ROUTINES                                                      
***********************************************************************         
SPFNCA   NTR1  WORK=(R5,SPFNCADQ)                                               
         USING SPFNCAD,R5                                                       
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO3                                                      
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),SPFDMCB,DBLOCK,0,0,0                                        
         CLI   DBERROR,0                                                        
         BNE   SPFNCAX                                                          
*                                                                               
         XC    SPFDCNT,SPFDCNT                                                  
         XC    SPFOKEY,SPFOKEY                                                  
         MVC   SPFOKEY(L'DBKEY),DBKEY                                           
         MVC   SPFOSVKY,SPFOKEY                                                 
         GOTO1 DATAMGR,SPFDMCB,=C'DMRDHI',DBDIRNAM,SPFOSVKY,SPFOKEY             
         B     SPFNCA2A                                                         
SPFNCA2  GOTO1 DATAMGR,SPFDMCB,=C'DMRSEQ',DBDIRNAM,SPFOSVKY,SPFOKEY             
SPFNCA2A CLI   8(R1),0                                                          
         BNE   SPFNCAX                                                          
         CLC   SPFOKEY(3),SPFOSVKY                                              
         BNE   SPFNCAX                                                          
         CLI   BYTE1,C'F'           IF GETFIL FUNCTION,                         
         BE    SPFNCA4              THEN READ FILE                              
*                                                                               
         MVC   PLFILNAM,DBDIRNAM                                                
         MVC   PL2KEY,SPFOKEY                                                   
         LA    R0,L'PL2KEY                                                      
         GOTO1 HEXOUT,SPFDMCB,SPFOKEY,PL2HXKEY,(R0),=C'TOG',0                   
         BRAS  RE,GOSPOOL                                                       
         LH    R1,SPFDCNT                                                       
         LA    R1,1(R1)                                                         
         CHI   R1,200              LIST A MAX OF 200 RECORDS                    
         BNL   SPFNCAX                                                          
         STH   R1,SPFDCNT                                                       
         MVC   SPFOSVKY,SPFOKEY                                                 
         B     SPFNCA2                                                          
*                                                                               
SPFNCA4  MVC   SPFDA,DBNDXDA                                                    
         L     RF,AIO3                                                          
         MVC   0(20,RF),SPFOKEY                                                 
         GOTO1 DATAMGR,SPFDMCB,=C'DMRDHI',DBFILNAM,SPFDA,AIO3                   
         B     SPFNCA4B                                                         
SPFNCA4A GOTO1 DATAMGR,SPFDMCB,=C'DMRSEQ',DBFILNAM,SPFDA,AIO3                   
SPFNCA4B CLI   8(R1),0                                                          
         BNE   SPFNCAX                                                          
         MVC   PLFILNAM,DBFILNAM                                                
         MVC   PL1NXDA1,=C'NXDA='                                               
         LA    R0,L'SPFDA                                                       
         GOTO1 HEXOUT,SPFDMCB,SPFDA,PL1NXDA2,(R0),=C'TOG',0                     
         MVI   PL1COMMA,C','                                                    
         LA    R0,L'DBKEY+5                                                     
         LA    R1,PL1KEY                                                        
         L     RF,AIO3                                                          
         BAS   RE,PUTKEY                                                        
         BRAS  RE,GOSPOOL                                                       
         LA    R0,L'DBKEY+5                                                     
         GOTO1 HEXOUT,SPFDMCB,AIO3,PL1HXKEY,(R0),=C'TOG',0                      
         BRAS  RE,GOSPOOL                                                       
         B     SPFNCA4A                                                         
*                                                                               
SPFNCAX  J     EXIT                                                             
         DROP  R4,R5                                                            
         SPACE 3                                                                
PUTKEY   DS    0H                                                               
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,2(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PUTKEY                                                        
         BR    RE                                                               
         EJECT                                                                  
EIIF     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  DS    0H                                                               
         L     R2,FADR                                                          
         GOTO1 ERREX                                                            
*                                                                               
TOOSHORT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOSHRTM),TOOSHRTM                                     
         B     TRAPERR2                                                         
TOOSHRTM DC    C'* ERROR * FIELD LENGTH TOO SHORT'                              
*                                                                               
TOOLNG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOLONGM),TOOLONGM                                     
         B     TRAPERR2                                                         
TOOLONGM DC    C'* ERROR * FIELD LENGTH TOO LONG'                               
*                                                                               
ELPRERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ELPRERRL),ELPRERRL                                     
         B     TRAPERR2                                                         
ELPRERRL DC    C'* ERROR * ELEM FILTER CAN NOT HAVE PRINT? INPUT'               
*                                                                               
NOTNOW   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTNOWL),NOTNOWL                                       
         B     TRAPERR2                                                         
NOTNOWL  DC    C'* ERROR * FUNCTION UNAVAILABLE "NOW" (RUN "SOON")'             
*                                                                               
ONLYNOW  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ONLYNOWL),ONLYNOWL                                     
         B     TRAPERR2                                                         
ONLYNOWL DC    C'* ERROR * FUNCTION ONLY AVAILABLE "NOW"'                       
*                                                                               
TRAPERR2 DS    0H                                                               
         L     R2,FADR                                                          
         GOTO1 ERREX2                                                           
*                                                                               
BITMAPS  DCB   DDNAME=BITMAPS,DSORG=PS,RECFM=VB,LRECL=2000,MACRF=PM,   +        
               BLKSIZE=0                                                        
*                                                                               
         GETEL R5,23,ELCODE                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF ERROR MESSAGES                                                       
*  NOTE: EQUATES ARE PREFIXED WITH "DE$" BECAUSE OF SYMBOL COLLISIONS           
*        WITH DDSPLWORKD                                                        
*                                                                               
ERRTAB   DS    0C                                                               
         DC    AL1(DE$INVCMND),CL8'INVCMND'                                     
         DC    AL1(DE$INVFILE),CL8'INVFILE'                                     
         DC    AL1(DE$INVMED),CL8'INVMED'                                       
         DC    AL1(DE$INVSRC),CL8'INVSRC'                                       
         DC    AL1(DE$INVFM),CL8'INVFM'                                         
         DC    AL1(DE$INVFMS),CL8'INVFMS'                                       
         DC    AL1(DE$INVELEM),CL8'INVELEM'                                     
         DC    AL1(DE$NODSPTAB),CL8'NODSPTAB'                                   
         DC    AL1(DE$NOMAST),CL8'NOMAST'                                       
         DC    AL1(DE$NOFORM),CL8'NOFORM'                                       
         DC    AL1(DE$NOLBOOK),CL8'NOLBOOK'                                     
         DC    AL1(DE$TOODEEP),CL8'TOODEEP'                                     
         DC    AL1(DE$FORMLOOP),CL8'FORMLOOP'                                   
         DC    AL1(DE$DIVZERO),CL8'DIVZERO'                                     
         DC    AL1(DE$RECLONG),CL8'RECLONG'                                     
         DC    AL1(DE$INVMRKT),CL8'INVMRKT'                                     
         DC    AL1(DE$NODQTAB),CL8'NODQTAB'                                     
         DC    AL1(DE$INVMTHB),CL8'INVMTHB'                                     
         DC    AL1(DE$HORREND),CL8'HORREND'                                     
         DC    AL1(DE$NOTFOUND),CL8'NOTFOUND'                                   
         DC    AL1(DE$DISKER),CL8'DISKER'                                       
         DC    AL1(DE$EOF),CL8'EOF'                                             
         DC    X'FE',CL8'FORCEOF'                                               
         DC    X'FF',CL8'UNKNOWN'                                               
*                                                                               
* TABLE OF ERROR MODULES                                                        
*                                                                               
MODTAB   DS    0X                                                               
         DC    AL1(DE$EDEMADDR),CL8'DEMADDR'                                    
         DC    AL1(DE$EDEMAINT),CL8'DEMAINT'                                    
         DC    AL1(DE$EDEMAND),CL8'DEMAND'                                      
         DC    AL1(DE$EDEMEL),CL8'DEMEL'                                        
         DC    AL1(DE$EDEMOUT),CL8'DEMOUT'                                      
         DC    AL1(DE$EDEMOMTH),CL8'DEMOMATH'                                   
         DC    X'FF',CL8'UNKNOWN'                                               
*                                                                               
* TABLE OF RECORD TYPES                                                         
*                                                                               
RECTAB   DS    0X                                                               
         DC    AL1(DBRECSM),CL8'SM'                                             
         DC    AL1(DBRECMS),CL8'MS'                                             
         DC    AL1(DBRECMB),CL8'MB'                                             
         DC    AL1(DBRECMK),CL8'MK'                                             
         DC    AL1(DBRECUNV),CL8'UNV'                                           
         DC    AL1(DBRECNTI),CL8'NTI'                                           
         DC    AL1(DBRECTOT),CL8'TOT'                                           
         DC    AL1(DBRECDEM),CL8'DEM'                                           
         DC    AL1(DBRECTLB),CL8'TLB'                                           
         DC    AL1(DBRECNBK),CL8'NBK'                                           
         DC    AL1(DBRECINV),CL8'INV UPGD'                                      
         DC    X'FF',CL8'UNKNOWN'                                               
         EJECT                                                                  
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*CNVIUN- CONVERT INPUT TO IUN RECD AND DISPLAY RECD AND DEMO ELEMS              
***********************************************************************         
CNVIUN   NTR1  BASE=(*,CNVIUNXX),WORK=(R2,CNVIUNDL)                             
         USING CNVIUND,R2                                                       
         MVC   SVDBLK,DBLOCK      SAVE ORIGINAL DBLOCK                          
         LA    RF,IUNREC           CLEAR IUN RECD AREA                          
         LR    R0,RF                                                            
         LA    R1,IUNRECL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    RF,IUNWORK          CLEAR IUNWORK AREA                           
         LR    R0,RF                                                            
         LA    R1,IUNWORKX-IUNWORK                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OC    DBAQUART,DBAQUART                                                
         BZ    CNVIUNX             NO DBAQUART PASSED                           
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         L     RF,VSPGTIUN         DEFAULT TO SPGETIUN                          
         CLI   FAOVSYS-FACTSD(R1),X'08'   REP SYSTEM?                           
         BNE   *+8                                                              
         L     RF,VREGTIUN         FOR REP SYSTEM USE REGETIUN                  
         GOTO1 (RF),DMCB,(4,DBLOCK),IUNWORK                                     
*                                                                               
         MVC   NEWRTG(NUMVALS*4),OLDRTG                                         
         MVC   NEWIMP(NUMVALS*4),OLDIMP                                         
         MVC   NEWHPT(NUMVALS*4),OLDHPT                                         
         MVC   NEWTOT(NUMVALS*4),OLDTOT                                         
*                                                                               
         LA    RF,IUNREC           BLD KEY OF IUN RECD                          
         USING RINVREC,RF                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVLEN,=Y(RINVPEL-RINVREC)                                      
         MVC   RINVKREP,DBSELAGY                                                
         MVC   RINVKSTA,DBSELSTA                                                
*                                                                               
* DEIS: AS OF MAY/2015, IT SEEMS THIS CODE NO LONGER ASSEMBLES CLEANLY.         
*       IF WE DIE HERE, IT MEANS WE HAVE TO FIX THIS.                           
*                                                                               
         DC    H'0'                                                             
*&&DO                                                                           
         MVC   RINVKSRC,DBSELSRC                                                
*&&                                                                             
         MVC   RINVKBK,DBACTBK                                                  
         DROP  RF                                                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    RF,IUNREC           BLD KEY OF IUN RECD                          
         LA    R1,RINVPEL-RINVREC(RF)                                           
         ST    R1,DBAQUART                                                      
         LA    R1,(IUNWORKX-IUNWORK)/4                                          
         STCM  R1,3,DBNUMVLS                                                    
         LA    R1,IUNREC                                                        
         ST    R1,DBAREC                                                        
         MVC   TMPWRK1(7),=C'INVUIUN'                                           
         MVC   TMPWRK1+7(2),=X'530B'                                            
         LA    R1,SVDBLK                                                        
         CLI   DBTAPEP-DBLOCK(R1),C'Y'                                          
         BNE   *+14                                                             
         MVI   DBTAPEP,C'Y'                                                     
         MVC   TMPWRK1+7(2),=X'5A0B'                                            
         MVI   TMPWRK1+9,0          NO FILTER                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUT',DBLOCK,IUNWORK,TMPWRK1                         
         CLI   DBERROR,0                                                        
         BNE   CNVIUNX             NO DBAQUART PASSED                           
*                                                                               
         LA    R3,IUNREC          OUTPUT IUN KEY                                
         LH    R4,=Y(RINVPEL-RINVREC)                                           
         MVI   WORK,1                                                           
         MVC   P(4),=C'IUN '                                                    
         BAS   RE,CNVPRT                                                        
         CLI   PCTRLI,C'K'         ONLY PRINT KEY?                              
         BE    CNV50                                                            
*                                                                               
CNV40    CLI   0(R3),0                                                          
         BE    CNV50               DONE                                         
         LLC   R4,1(R3)                                                         
         MVI   WORK,0                                                           
         BAS   RE,CNVPRT                                                        
         B     CNV40                                                            
*                                                                               
CNV50    DS    0H                  READ DEMOS FROM IUN RECORD                   
         LA    RE,DEMOLST2                                                      
         MVC   DEMOLST2(DEMLISTQ),DEMOLIST                                      
CNV55    LAY   RF,IUNNHT           CONVERT NET MODIFIERS TO IUN MDFS            
         CLI   0(RE),X'FF'         END OF OF DEMOLIST                           
         BE    CNV60                                                            
CNV56    CLI   0(RF),X'FF'         END OF IUNMODF TABLE?                        
         BE    CNV57               NEXT DEMOLST ENTRY                           
         CLC   0(1,RF),1(RE)       SAME MODIFIER?                               
         BE    *+12                                                             
         LA    RF,L'IUNNHT(RF)     TRY NEXT                                     
         B     CNV56                                                            
         MVC   1(1,RE),1(RF)       REPLACE W/IUN MODIFIER                       
CNV57    LA    RE,3(RE)            NEXT IN DEMOLST                              
         B     CNV55                                                            
*                                                                               
CNV60    DS    0H                                                               
         XC    DEMOVALS(240),DEMOVALS                                           
         XC    DEMOVALS+240(240),DEMOVALS+240                                   
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLST2),DBLOCK,DEMOVALS                        
*                                                                               
CNVIUNX  MVC   DBLOCK,SVDBLK                                                    
         J     EXIT                                                             
*                                                                               
*------> PROCEDURE TO PRINT RECD ELEMS FOR BUILT IUN RECD <-----------          
CNVPRT   LR    R0,RE                                                            
CNVPRT5  LA    R6,32                                                            
         CR    R4,R6                                                            
         BH    *+6                                                              
         LR    R6,R4                                                            
         MVI   P+4,C'('                                                         
         CVD   R5,TMPDUB                                                        
         UNPK  P+5(3),TMPDUB                                                    
         OI    P+7,X'F0'                                                        
         MVI   P+8,C'-'                                                         
         AR    R5,R6                                                            
         BCTR  R5,0                                                             
         CVD   R5,TMPDUB                                                        
         UNPK  P+9(3),TMPDUB                                                    
         OI    P+11,X'F0'                                                       
         MVI   P+12,C')'                                                        
         LA    R5,1(R5)                                                         
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         CLI   WORK,1                                                           
         BNE   CNVPRT10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),0(R3)                                                    
         B     CNVPRT20                                                         
*                                                                               
CNVPRT10 DS    0H                                                               
         MVI   WORK,1                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+18(0),2(R3)                                                    
*                                                                               
CNVPRT20 DS    0H                                                               
         BRAS  RE,GOSPOOL                                                       
         GOTO1 HEXOUT,DMCB,(R3),P+14,(R6),=C'TOG'                               
         BRAS  RE,GOSPOOL                                                       
         AR    R3,R6               POINT TO NEXT ELEMENT                        
         SR    R4,R6               TEST IF ANY LEFT OF CURRENT                  
         BNZ   CNVPRT5                                                          
         BRAS  RE,GOSPOOL                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         SPACE 2                                                                
         DROP  RB                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
CNVIUNXX EQU   *                                                                
         EJECT                                                                  
RDBITMAP NMOD1 (8192+2000+100),**BTMP**,CLEAR=YES                               
*                                                                               
         LR    R2,RC               R2 = A(8K BITMAP BUFFER)                     
         LAY   R4,8192(,R2)        R4 = A(2K I/O BUFFER WITH SPARE)             
         LR    RC,R3               RESTORE SAVED A(GEND)                        
*                                                                               
         MVC   MAJORKEY+(PRSTAT-PRKEY)+4(1),BYTE  H, N, S                       
*                                                                               
* PROCESS ONE SET OF BITMAP RECORDS (FOR A PROGRAM TYPE: H, N, S)               
*                                                                               
         LA    R3,4(,R4)           LEAVE ROOM FOR RDW                           
         USING PRKEY,R3                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'NTIDIR',MAJORKEY,(R3)             
*                                                                               
* LOOP THROUGH ALL (PRESUMABLY 5) BITMAP RECORDS FOR THE CURRENT TYPE           
*                                                                               
RDBM10   DS    0H                                                               
         CLI   8(R1),0                                                          
         JNE   *+2                 FATAL DATAMGR ERROR                          
*                                                                               
         MVC   FULL,19(R3)         SAVE DISK ADDRESS                            
         MVC   22(1,R3),18(R3)     STATUS BYTE                                  
         XC    18(4,R3),18(R3)                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'NTIFIL',FULL,(R3),0               
         CLI   8(R1),0                                                          
         JNE   *+2                 FATAL DATAMGR ERROR                          
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(R3),(23,20),VPRINT,HEXOUT,RR=RELO               
         MVI   P,0                                                              
         BRAS  RE,GOSPOOL          PRINT A TRACE OF THE RECORD                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,B'0011',20(R3)   RECORD LENGTH                                
         AHI   R0,4                +RDW                                         
         STCM  R0,B'0011',0(R4)    VARIABLE RECORD LENGTH                       
         XC    2(2,R4),2(R4)                                                    
         LAY   R5,BITMAPS          A(DCB) FOR BITMAP OUTPUT FILE                
         PUT   (R5),(R4)           PUT THE RECORD TO A FLAT OUTPUT FILE         
*                                                                               
* BUILD THE ENTIRE BITMAP IN CORE, ONE RECORD/ELEMENT AT A TIME                 
*                                                                               
         LR    R5,R3                                                            
         USING NTIELEM,R5                                                       
         MVI   ELCODE,NTICODEQ                                                  
         BRAS  RE,GETEL            R5 PTS TO 1ST NTI ELEM                       
*                                                                               
RDBM20   DS    0H                                                               
         LLC   R6,NTISEQ           ELEMENT SEQUENCE NUMBER (ZERO-BASED)         
         MHI   R6,L'NTIBITS        INDEX INTO BITMAP                            
         AR    R6,R2               R6 = LOCATION TO PUT NEXT BITS               
         LLC   R1,NTIELN           NUMBER OF BYTES TO MOVE                      
         SHI   R1,NTIOVLNQ         MINUS ELEMENT OVERHEAD LENGTH                
         BCTR  R1,0                                                             
         EX    R1,*+8              COPY BYTES INTO MAP                          
         J     *+10                                                             
         MVC   0(0,R6),NTIBITS                                                  
         BRAS  RE,NEXTEL           NEXT NTIELEM                                 
         BE    RDBM20                                                           
         DROP  R5                                                               
*                                                                               
* WE'RE DONE WITH THIS BITMAP RECORD: LOOK FOR MORE                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'NTIDIR',MAJORKEY,(R3)             
         CLI   8(R1),0                                                          
         JNE   *+2                 FATAL DATAMGR ERROR                          
*                                                                               
         CLC   MAJORKEY(PRKMKT-PRKEY),0(R3)  CHANGE OF BITMAP KEY?              
         BE    RDBM10              NO: GET NEXT BITMAP RECORD                   
         DROP  R3                                                               
*                                                                               
* WE'VE PROCESSED ALL (FIVE) BITMAP RECORDS FOR THIS TYPE, AND WE'VE            
* BUILT THE ENTIRE BITMAP IN WORKING STORAGE. NOW COUNT THE NUMBER OF           
* BITS TURNED ON IN THE BITMAP.                                                 
*                                                                               
         LHI   R0,8192/8           R0 = # DOUBLEWORDS IN BITMAP AREA            
         SR    R3,R3               RUNNING BIT TOTAL                            
*                                                                               
RDBM30   DS    0H                                                               
         LG    GRF,0(,R2)          8 BYTES FROM THE BITMAP                      
*                                                                               
* THIS CODE COURTESY THE Z/OS "PRINCIPLES OF OPERATION" MANUAL                  
*                                                                               
         POPCNT GR4,GRF            COUNT BITS SET ON A PER-BYTE BASIS           
         AHHLR R4,R4,R4                                                         
         SLLG  GR5,GR4,16                                                       
         ALGR  GR4,GR5                                                          
         SLLG  GR5,GR4,8                                                        
         ALGR  GR4,GR5                                                          
         SRLG  GR4,GR4,56          R4 = # OF BITS SET IN DOUBLEWORD             
*                                                                               
         AR    R3,R4               ADD TO RUNNING TOTAL                         
         LA    R2,8(,R2)           BUMP TO NEXT DOUBLEWORD                      
         BCT   R0,RDBM30           RUN THROUGH ENTIRE BITMAP AREA               
*                                                                               
         SELECT CLI,BYTE,EQ                                                     
           WHEN (C'H')                                                          
             MVC  P1(11),=CL11'HISPANIC'                                        
           WHEN (C'N')                                                          
             MVC  P1(11),=CL11'BROADCAST'                                       
           WHEN (C'S')                                                          
             MVC  P1(11),=CL11'SYNDICATION'                                     
           OTHRWISE ,                                                           
             J  *+2                IMPOSSIBLE                                   
         ENDSEL ,                                                               
*                                                                               
         MVC   BLOCK(MAILMS1Q),MAILMSG1  BUILD AUTONOTE HERE                    
BLCK     USING MAILMSG1,BLOCK                                                   
         MVC   BLCK.MAILMSGT,P1    BITMAP TYPE                                  
*                                                                               
         MVC   P2,P1                                                            
         MVC   P3,P1                                                            
*                                                                               
         MVC   P1+12(24),=C'USED      NTI NUMBERS = '                           
         EDIT  (R3),(6,P1+36),ZERO=NOBLANK,COMMAS=YES                           
*                                                                               
         MVC   P2+12(24),=C'AVAILABLE NTI NUMBERS = '                           
         L     R5,=F'65535'        MAXIMUM NUMBER OF NTI #S PER TYPE            
         SR    R5,R3               COUNT OF AVAILABLE NTI #S                    
         EDIT  (R5),(6,P2+36),ZERO=NOBLANK,COMMAS=YES                           
         MVC   BLCK.MAILMSG#,P2+36                                              
         LR    R3,R5               SAVE COUNT OF AVAILABLE NTI #S               
*                                                                               
         L     R1,=F'65535'        MAXIMUM NUMBER OF NTI #S PER TYPE            
         SR    R4,R4               PREPARE FOR DIVIDE                           
         MHI   R5,100              PREPARE FOR PERCENTAGE                       
         DR    R4,R1               R5 = % AVAILABLE                             
         MVC   P3+12(24),=C'PERCENTAGE AVAILABLE  = '                           
         EDIT  (R5),(3,P3+36),ZERO=NOBLANK,TRAIL=C'%'                           
*                                                                               
         BRAS  RE,GOSPOOL                                                       
*                                                                               
         CHI   R3,NTI#_WARN_THRESHHOLD  ARE THE NUMBERS RUNNING LOW?            
         BNL   RDBMX               NO: THAT'S GOOD                              
*                                                                               
         CLC   =C'NTIBITMAPS/NOWARN',DEMMJKY  SUPPRESS E-MAIL WARNING?          
         BE    RDBMX               YES                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('MAILMS1Q',BLOCK)  SEND AUTONOTE         
         DROP  BLCK                                                             
*                                                                               
RDBMX    DS    0H                                                               
         MVI   P1,0                 SKIP LINES                                  
         MVI   P2,0                                                             
         BRAS  RE,GOSPOOL                                                       
*                                                                               
         J     EXIT                                                             
*                                                                               
NTI#_WARN_THRESHHOLD EQU 1000       ISSUE WARNING IF WE DROP TOO LOW            
*                                                                               
MAILMSG1 DC    C'AUTONOTE*US-DEMOSTEAM:** AVAILABLE '                           
MAILMSGT DS    CL11                                                             
         DC    C' NTI NUMBERS = '                                               
MAILMSG# DS    CL6                                                              
         DC    C' **'                                                           
MAILMS1Q EQU   *-MAILMSG1                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF INPUT FIELDS (SEE INPTBLD)                                           
*                                                                               
INPTBL   DS    0XL11                                                            
         DC    AL2(DEMFUNCH-T217FFD,DBFUNCT-DBLOCK)                             
         DC    AL1(4,8,L'DBFUNCT),X'41',AL3(FUNCTAB)                            
         DC    AL2(DEMFILEH-T217FFD,DBFILE-DBLOCK)                              
         DC    AL1(1,3,L'DBFILE),X'40',AL3(FILETAB)                             
         DC    AL2(DEMMEDH-T217FFD,DBSELMED-DBLOCK)                             
         DC    AL1(1,5,L'DBSELMED),X'40',AL3(MEDTAB)                            
         DC    AL2(DEMSRCH-T217FFD,DBSELSRC-DBLOCK)                             
         DC    AL1(1,3,L'DBSELSRC),X'40',AL3(SRCTAB)                            
         DC    AL2(DEMSTAH-T217FFD,DBSELSTA-DBLOCK)                             
         DC    AL1(3,5,L'DBSELSTA),X'00',AL3(0)                                 
         DC    AL2(DEMSTYPH-T217FFD,DBSTYPE-DBLOCK)                             
         DC    AL1(1,1,L'DBSTYPE),X'00',AL3(0)                                  
         DC    AL2(DEMAMKTH-T217FFD,DBSELALF-DBLOCK)                            
         DC    AL1(1,3,L'DBSELALF),X'00',AL3(0)                                 
         DC    AL2(DEMRMKTH-T217FFD,DBSELRMK-DBLOCK)                            
         DC    AL1(1,4,L'DBSELRMK),X'80',AL3(0)                                 
         DC    AL2(DEMKMKTH-T217FFD,DBSELMK-DBLOCK)                             
         DC    AL1(1,5,L'DBSELMK),X'80',AL3(0)                                  
         DC    AL2(DEMUMKTH-T217FFD,DBSELUMK-DBLOCK)                            
         DC    AL1(1,4,L'DBSELUMK),X'80',AL3(0)                                 
         DC    AL2(DEMPROGH-T217FFD,DBSELPRG-DBLOCK)                            
         DC    AL1(1,5,L'DBSELPRG),X'80',AL3(0)                                 
         DC    AL2(DEMINVH-T217FFD,DBSELINV-DBLOCK)                             
         DC    AL1(1,4,L'DBSELINV),X'00',AL3(0)                                 
         DC    AL2(DEMPUREH-T217FFD,DBSELPUR-DBLOCK)                            
         DC    AL1(4,4,L'DBSELPUR),X'20',AL3(VALPURE)                           
         DC    AL2(DEMBOOKH-T217FFD,DBSELBK-DBLOCK)                             
         DC    AL1(4,6,L'DBSELBK),X'20',AL3(VALBOOK)                            
         DC    AL2(DEMBTYPH-T217FFD,DBBTYPE-DBLOCK)                             
         DC    AL1(1,2,L'DBBTYPE),X'20',AL3(VALBTYP)                            
         DC    AL2(DEMB1WKH-T217FFD,DBSEL1WK-DBLOCK)                            
         DC    AL1(8,8,L'DBSEL1WK),X'20',AL3(VAL1WK)                            
         DC    AL2(DEMBWKNH-T217FFD,DBSELWKN-DBLOCK)                            
         DC    AL1(1,3,L'DBSELWKN),X'20',AL3(VALWKN)                            
         DC    AL2(DEMBLIMH-T217FFD,DBSELDAT-DBLOCK)                            
         DC    AL1(4,6,L'DBSELDAT),X'20',AL3(VALBOOK)                           
         DC    AL2(DEMAGYH-T217FFD,DBSELAGY-DBLOCK)                             
         DC    AL1(2,2,L'DBSELAGY),X'00',AL3(0)                                 
         DC    AL2(DEMCLIH-T217FFD,DBSELCLI-DBLOCK)                             
         DC    AL1(2,3,L'DBSELCLI),X'00',AL3(0)                                 
         DC    AL2(DEMDAYSH-T217FFD,DBSELDAY-DBLOCK)                            
         DC    AL1(1,15,L'DBSELDAY),X'20',AL3(VALDAY)                           
         DC    AL2(DEMTIMSH-T217FFD,DBSELTIM-DBLOCK)                            
         DC    AL1(2,12,L'DBSELTIM),X'20',AL3(VALTIME)                          
         DC    AL2(DEMBESTH-T217FFD,DBBEST-DBLOCK)                              
         DC    AL1(1,1,L'DBBEST),X'00',AL3(0)                                   
         DC    AL2(DEMTPTTH-T217FFD,DBTPTT-DBLOCK)                              
         DC    AL1(1,1,L'DBTPTT),X'00',AL3(0)                                   
         DC    AL2(DEMDEMOH-T217FFD,0)                                          
         DC    AL1(1,L'DEMDEMO,0),X'30',AL3(VALDEMO)                            
         DC    AL2(DEMPRNTH-T217FFD,0)                                          
         DC    AL1(1,L'DEMPRNT,0),X'30',AL3(VALPRINT)                           
         DC    AL2(DEMTAPEH-T217FFD,DBTAPEP-DBLOCK)                             
         DC    AL1(1,1,L'DBTAPEP),X'00',AL3(0)                                  
         DC    AL2(DEMEFLTH-T217FFD,0)                                          
         DC    AL1(1,L'DEMEFLT,0),X'30',AL3(VALEFILT)                           
         DC    AL2(DEMPTTH-T217FFD,DBSELPTT-DBLOCK)                             
         DC    AL1(1,1,L'DEMPTT),X'40',AL3(PTTTAB)                              
         DC    AL2(DEMIUNFH-T217FFD,0)                                          
         DC    AL1(1,L'DEMIUNF,0),X'30',AL3(IUNFILT)                            
         DC    X'FF'                                                            
         EJECT                                                                  
* FUNCTION TABLE                                                                
*                                                                               
FUNCTAB  DS    0X                                                               
         DC    C'VALST   ',AL1(DBVLST)                                          
         DC    C'VALSTBK ',AL1(DBVLSTBK)                                        
         DC    C'GETSM   ',AL1(DBGETSM)                                         
         DC    C'GETMS   ',AL1(DBGETMS)                                         
         DC    C'GETMB   ',AL1(DBGETMB)                                         
         DC    C'GETMK   ',AL1(DBGETMK)                                         
         DC    C'GETMKB  ',AL1(DBGETMKB)   MARKETS FOR A BOOK                   
         DC    C'GETMKN  ',AL1(DBGETMKN)   MARKETS FOR A SERVICE                
         DC    C'GETUNV  ',AL1(DBGETUNV)                                        
         DC    C'GETNTI  ',AL1(DBGETNTI)                                        
         DC    C'GETNET  ',AL1(DBGETDEM)                                        
         DC    C'GETTOT  ',AL1(DBGETTOT)                                        
         DC    C'GETDEM  ',AL1(DBGETDEM)                                        
         DC    C'GETPUR  ',AL1(DBGETPUR)                                        
         DC    C'GETTLB  ',AL1(DBGETTLB)                                        
         DC    C'VALNBK  ',AL1(DBVLNBK)                                         
         DC    C'GETDIR  ',AL1(0)                                               
         DC    C'GETFIL  ',AL1(0)                                               
         DC    C'CLRTAB  ',AL1(0)                                               
         DC    C'TSTACS  ',AL1(DBTSTACS)                                        
         DC    C'CNVA2N  ',AL1(DBCNVA2N)                                        
         DC    C'CNVN2A  ',AL1(DBCNVN2A)                                        
         DC    C'GETAMB  ',AL1(DBGETAMB)                                        
         DC    C'GETASB  ',AL1(DBGETASB)                                        
         DC    C'GETAB   ',AL1(DBGETAB)                                         
         DC    C'GETASM  ',AL1(DBGETASM)                                        
         DC    C'ACSALL  ',AL1(DBACSALL)                                        
         DC    C'GETOPI  ',AL1(DBGETOPI)                                        
         DC    X'FF'                                                            
*                                                                               
* SPECIAL FUNCTION TABLE                                                        
*                                                                               
SPFNCTAB DS    0XL(L'SFNAM+L'SFDSP+L'SFFLG)                                     
SFNAM    DS    CL8                 NAME OF SPECIAL FUNCTION                     
SFDSP    DS    AL3                 ADDRESS OF FUNCTION PROCEDURE                
SFFLG    DS    XL1                 FLAG OF SPECIAL FUNCTION                     
         ORG   SPFNCTAB                                                         
         DC    CL8'GETDIR  ',AL3(SPFNCA),C'D'                                   
         DC    CL8'GETFIL  ',AL3(SPFNCA),C'F'                                   
         DC    X'FF'                                                            
*                                                                               
CLRTAB   DS    0XL9                                                             
         DC    CL8'DBOOK   ',X'D1'                                              
         DC    CL8'T00AD1  ',X'D1'                                              
         DC    CL8'DSTATION',X'D2'                                              
         DC    CL8'T00AD2  ',X'D2'                                              
         DC    CL8'DMASTER ',X'D3'                                              
         DC    CL8'T00AD3  ',X'D3'                                              
         DC    CL8'DNAME   ',X'D5'                                              
         DC    CL8'T00AD5  ',X'D5'                                              
         DC    CL8'DCODE   ',X'D6'                                              
         DC    CL8'T00AD6  ',X'D6'                                              
         DC    CL8'DCONTROL',X'D7'                                              
         DC    CL8'T00AD7  ',X'D7'                                              
         DC    CL8'DADJUST ',X'D8'                                              
         DC    CL8'T00AD8  ',X'D8'                                              
         DC    CL8'DFMTAB  ',X'E2'                                              
         DC    CL8'T00AE2  ',X'E2'                                              
         DC    CL8'DALPHMKT',X'E3'                                              
         DC    CL8'T00AE3  ',X'E3'                                              
         DC    CL8'DNADUNV ',X'E4'                                              
         DC    CL8'T00AE4  ',X'E4'                                              
         DC    CL8'DFUSION ',X'E5'                                              
         DC    X'FF'                                                            
         SPACE 3                                                                
* FILE TABLE                                                                    
*                                                                               
FILETAB  DS    0X                                                               
         DC    C'TP ',C'TP '                                                    
         DC    C'PAV',C'PAV'                                                    
         DC    C'MPA',C'MPA'                                                    
         DC    C'NAD',C'NAD'                                                    
         DC    C'NTI',C'NTI'                                                    
         DC    C'EVN',C'EVN'                                                    
         DC    C'DPT',C'DPT'                                                    
         DC    C'RDP',C'RDP'                                                    
         DC    C'RTP',C'RTP'                                                    
         DC    C'CAB',C'CAB'                                                    
         DC    C'IUN',C'IUN'                                                    
         DC    X'FF'                                                            
*                                                                               
* MEDIA TABLE                                                                   
*                                                                               
MEDTAB   DS    0X                                                               
         DC    C'USTV ',C'T'                                                    
         DC    C'CANTV',C'C'                                                    
         DC    C'NETTV',C'N'                                                    
         DC    C'RADIO',C'R'                                                    
         DC    C'DPT  ',C'D'                                                    
         DC    C'WTP  ',C'W'                                                    
         DC    C'CABLE',C'C'                                                    
         DC    C'HISPW',C'W'                                                    
         DC    C'UPGRD',C'U'                                                    
         DC    C'OVRNT',C'O'                                                    
         DC    X'FF'                                                            
*                                                                               
* SOURCE TABLE                                                                  
*                                                                               
SRCTAB   DS    0X                                                               
         DC    C'ARB',C'A'                                                      
         DC    C'NSI',C'N'                                                      
         DC    C'SRC',C'S'                                                      
         DC    C'BBM',C'M'                                                      
         DC    C'MFX',C'M'                                                      
         DC    C'NHT',C'H'         REP REQST FOR NHTI DATA                      
         DC    C'NTI',C'K'               "       NTI                            
         DC    C'NAD',C'D'               "       NAD                            
         DC    C'CBL',C'C'               "       CBL                            
         DC    C'RAR',C'R'         NET RADIO RADAR                              
         DC    C'FUS',C'F'                                                      
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* SELPTT TABLE - CABLE PROGRAMS FILTER OPTIONS                                  
*                                                                               
PTTTAB   DS    0X                                                               
         DC    C'P',C'P'           PROGRAM ONLY                                 
         DC    C'T',C'T'           TRACK ONLY                                   
         DC    C'E',C'E'           EPISODE (TELECAST) ONLY                      
         DC    X'FF'                                                            
         EJECT                                                                  
IUNNHT   DS    0CL2                NHTI MODIFIERS = IUN MODIFIER                
         DC    C'R',C'B'           RATINGS                                      
         DC    C'Y',C'K'           IMPRESSIONS                                  
         DC    C'T',C'K'               "                                        
         DC    C'I',C'K'               "                                        
         DC    C'K',C'U'           UNIVERSES                                    
         DC    C'P',C'G'           PUTS                                         
         DC    C'Z',C'G'            "                                           
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
*                                  LIST OF DEMOS FOR XALL                       
ALLDEMOS DC    AL1(001,002,003,025,028,029,030,031,032,033)                     
         DC    AL1(040,041,042,043,044,045,046,048,049,050)                     
         DC    AL1(051,052,053,054,055,056,057,058,059,060)                     
         DC    AL1(065,075,078,079,080,081,082,083,090,091)                     
         DC    AL1(092,093,094,095,096,097,098,099,100,101)                     
         DC    AL1(102,103,104,105,106,107,108,109,110,121)                     
         DC    AL1(122,123,125,127,128,129,130,131,132,133)                     
         DC    AL1(140,141,142,143,144,145,146,147,148,149)                     
         DC    AL1(150,151,152,153,154,155,156,157,158,159)                     
         DC    AL1(160,172,173,174,175,176,177,179,180,181)                     
         DC    AL1(182,183,184,185,255,000,000,000,000,000)                     
         DC    AL1(000,000,000,000,000,000,000,000,000,000)                     
*                                                                               
* EXTENDED LIST SUPPORT                                                         
DYTMLIST DC    C'DYTM',AL4(0),X'00',X'40',AL2(2000),AL2(2030)                   
         DC    X'20',AL2(2100,2130)                                             
         DC    X'0000000000'                                                    
*                                                                               
PURELIST DC    C'PURE',AL4(0),X'00',AL2(441,443,484)                            
         DC    X'FFFF'                                                          
*                                                                               
MBKSLIST DC    C'MBKS',AL4(0),X'00'                                             
         DC    AL1(100,01)                                                      
         DC    AL1(099,11)                                                      
         DC    X'0000'                                                          
*                                                                               
SPOTLIST DC    C'SPOT',AL4(0)                                                   
         DC     X'02'               RTG/PUT                                     
         DC     X'01'               SHARE                                       
         DC     X'02'               IMP                                         
*                                                                               
UIDLIST  DC    C'UID ',AL4(0)      USER ID PARAMETER                            
         DC     C'02'               FUDGE THIS FOR TESTING                      
         EJECT                                                                  
* TABLE OF DEMO FILES (SEE DFILTABD)                                            
*                                                                               
DFILTAB  DS    0X                                                               
         DC    C'DEMDIR ',C'DEMFIL ',AL1(23),AL1(18),X'00'                      
         DC    C'PAVDIR ',C'PAVFIL ',AL1(23),AL1(18),X'00'                      
         DC    C'NTIDIR ',C'NTIFIL ',AL1(23),AL1(18),X'00'                      
         DC    X'FF'                                                            
         SPACE 3                                                                
DFILTABD DSECT                                                                  
DFILDIRN DS    CL7                 DIRECTORY NAME                               
DFILFILN DS    CL7                 FILE NAME                                    
DFILDRCL DS    AL1                 DIRECTORY RECORD LENGTH                      
DFILDKYL DS    AL1                 DIRECTORY KEY LENGTH                         
DFILKFLC DS    X                   KEY FILL CHARACTER                           
DFILTBLQ EQU   *-DFILTABD                                                       
         SPACE 3                                                                
SPSFM6D  CSECT                                                                  
         EJECT                                                                  
* REPLACE DICTATE ESCAPE CHARACTERS WITH NULLS                                  
*                                                                               
VALCHARS DC    XL16'000102030405060708090A0B0C0D0E0F'  00-0F                    
         DC    XL16'101112131415161718191A1B1C1D1E1F'  10-1F                    
         DC    XL16'00000000000000000000000000002E2F'  20-2F                    
         DC    XL16'303132333435363738393A3B3C3D3E3F'  30-3F                    
         DC    XL16'404142434445464748494A4B4C4D4E4F'  40-4F                    
         DC    XL16'505152535455565758595A5B5C5D5E5F'  50-5F                    
         DC    XL16'606162636465666768696A6B6C6D6E6F'  60-6F                    
         DC    XL16'707172737475767778797A7B7C7D7E7F'  70-7F                    
         DC    XL16'808182838485868788898A8B8C8D8E8F'  80-8F                    
         DC    XL16'909192939495969798999A9B9C9D9E9F'  90-9F                    
         DC    XL16'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'  A0-AF                    
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'  D0-DF                    
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'  F0-FF                    
         EJECT                                                                  
* DSECT TO COVER INPTBL                                                         
*                                                                               
INPTBLD  DSECT                                                                  
INPIFLD  DS    AL2                 DISP. TO INPUT FIELD                         
INPOFLD  DS    AL2                 DISP. TO OUTPUT FIELD                        
INPIMIN  DS    AL1                 MIN L'INPUT                                  
INPIMAX  DS    AL1                 MAX L'INPUT                                  
INPOLEN  DS    AL1                 L'OUTPUT FIELD                               
INPINDS  DS    XL1                 X'80'=NUMERIC FIELD                          
*                                  X'40'=INPADDR IS A(TABLE)                    
*                                  X'20'=INPADDR IS A(ROUTINE)                  
*                                  X'10'=VALIDATE DEMOS                         
*                                  X'01'=REQUIRED FIELD                         
INPADDR  DS    AL3                 A(TABLE) OR A (ROUTINE)                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM3FD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
*                                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
PACK8    DS    PL8                 TEMP FIELD USED IN PACK OPERATION            
*                                                                               
VDAYPAK  DS    V                                                                
VTIMVAL  DS    V                                                                
VSPGTIUN DS    V                                                                
VREGTIUN DS    V                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
FADR     DS    A                                                                
OADR     DS    A                                                                
MAXIO    DS    F                                                                
MAJORKEY DS    XL23                                                             
IUNPRT   DS    X                                                                
IUNFLG   DS    X                                                                
FERN     DS    XL1                                                              
FLDH     DS    XL8                                                              
FLD      DS    XL80                                                             
TBLPARM  DS    XL5                 PARAMETER TO DEMADDR                         
LSELECT  DS    CL64                                                             
LACTUAL  DS    CL64                                                             
DEMOLIST DS    60XL3,X             WAS 120XL3 BUT MAX DEMOS=10 ?                
DEMOLST2 DS    60XL3,X                                                          
DEMLISTQ EQU   *-DEMOLST2          LENGTH OF DEMOLIST BUFFER                    
DEMOVALS DS    120F                                                             
         EJECT                                                                  
         DS    0D                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PCTRLI   DS    CL1                 CNV IUN: Y=PRINT RECORD                      
PCTRL2   DS    CL1                 K=KEYS ONLY, R=DEFAULT                       
PKEYONLY EQU   C'K'                                                             
PDEFAULT EQU   C'R'                PRINT ENTIRE RECORD (DEFAULT)                
EFLTLST  DS    XL1,7XL2            7 RANGES OF ELEM TO FILTER                   
EFLTLSTQ EQU   *-EFLTLST            (E.G. LO-HI,LO-HI, ETC.)                    
IUNFLST  DS    XL1,7XL2            7 RANGES OF ELEM TO FILTER                   
IUNFLSTQ EQU   *-IUNFLST            (E.G. LO-HI,LO-HI, ETC.)                    
BYTE1    DS    XL1                                                              
DBXTND1  DS    4XL(DBXINVWL)                                                    
SPTSTDX  EQU   *                                                                
*                                                                               
*******************************************************************             
* DSECT TO COVER I/O TRACE W/S                                                  
*******************************************************************             
*                                                                               
MYTRCD   DSECT                                                                  
MYTDUB   DS    D                                                                
MYTDMCB  DS    6F                                                               
MYTIADR  DS    A                   A(INPUT TO DATAMGR)                          
MYTOADR  DS    A                   A(OUTPUT FROM DATAMGR)                       
MYTIOCMD DS    CL7                 COMMAND TO DATAMGR                           
MYTFILNM DS    CL7                 FILE READ BY DATAMGR                         
MYTKYLEN DS    H                   KEY LENGTH                                   
MYTDS1EL DS    H                   DISPL TO 1ST ELEMENT                         
*                                                                               
MYTWORK1 DS    CL128                                                            
MYTWORK2 DS    CL128                                                            
*                                                                               
MYTRCDQ  EQU   *-MYTRCD                                                         
*                                                                               
*******************************************************************             
* DSECT TO COVER SPECIAL FUNCTION ROUTINES                                      
*******************************************************************             
*                                                                               
SPFNCAD  DSECT                                                                  
SPFDA    DS    F                                                                
SPFDMCB  DS    6F                                                               
SPFDCNT  DS    H                                                                
SPFOKEY  DS    XL24                                                             
SPFOSVKY DS    XL24                                                             
SPFNAME  DS    CL7                                                              
SPFNCADQ EQU   *-SPFNCAD                                                        
         EJECT                                                                  
*******************************************************************             
*CNVIUND - COVERS WORKG STRG FOR ROUTINE TO CONVERT NETW-> IUN FMT              
*******************************************************************             
CNVIUND  DSECT                     WORKG STRG FOR CNV NET TO IUN FMT            
         DS    0D                                                               
AELEM    DS    F                                                                
TMPDUB   DS    D                                                                
TMPWRK1  DS    CL64                                                             
TMPWRK2  DS    CL64                                                             
*                                                                               
NUMVALS  EQU   32                                                               
IUNWORK  DS    0F                                                               
OLDUNV   DS    (NUMVALS)F          UNIVERSES                                    
OLDUNVX  EQU   *                                                                
OLDRTG   DS    (NUMVALS)F          RATINGS                                      
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                                  
OLDRTGX  EQU   *                                                                
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                                   
OLDHPTX  EQU   *                                                                
NEWRTG   DS    (NUMVALS)F          RATINGS                                      
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                                  
NEWRTGX  EQU   *                                                                
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                                   
NEWHPTX  EQU   *                                                                
HOMSHR   DS    3F                  ORIGINAL HOMES SHARES                        
HOMSHRX  EQU   *                                                                
HOMSHRLN EQU   *-HOMSHR                                                         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                                 
LUNVX    EQU   *                                                                
IUNWORKX DS    0F                                                               
*                                                                               
         DS    0H                                                               
SVDBLK   DS    CL256               SAVE MAIN PROCESING DBLOCK                   
IUNREC   DS    2000C               BUILD IUN RECD HERE                          
IUNRECL  EQU   2000                                                             
         DS    0D                                                               
CNVIUNDL EQU   *-CNVIUND                                                        
         EJECT                                                                  
*******************************************************************             
*DSECT TO COVER P                                                               
*******************************************************************             
*                                                                               
PLINED   DSECT                                                                  
PLFILNAM DS    CL(L'DBDIRNAM)                                                   
         DS    CL1                                                              
PL1NXDA1 DS    CL5                                                              
PL1NXDA2 DS    CL(2*L'DBNDXDA)                                                  
PL1COMMA DS    CL1                                                              
         DS    CL1                                                              
PL1KEY   DS    CL(2*(L'DBKEY+5))                                                
         ORG   PL1KEY                                                           
PL1HXKEY DS    CL(L'PL1KEY)                                                     
PL1LEN   EQU   *-PLINED                                                         
         ORG   PL1NXDA1                                                         
PL2KEY   DS    CL(L'DBKEY+5)                                                    
         DS    CL2                                                              
PL2HXKEY DS    CL(2*L'PL2KEY)                                                   
PL2LEN   EQU   *-PLINED                                                         
         EJECT                                                                  
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
*PREFIX=DE$                                                                     
       ++INCLUDE DEDEMEQUS                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
* REGENINV                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENINV                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPSFM6D   12/04/20'                                      
         END                                                                    
