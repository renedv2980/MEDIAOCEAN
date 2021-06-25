*          DATA SET SPTST00C   AT LEVEL 131 AS OF 05/01/02                      
*PHASE T21F00C,+0                                                               
*INCLUDE TWANG                                                                  
*INCLUDE DUMPOUT                                                                
SPTST00  TITLE '- DEMAND (DEGET) TEST PROGRAM'                                  
SPTST00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SPTSTDX-SPTSTD,**SPTST,R7,RR=RE                                  
         USING SPTSTD,RC           RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         L     RA,4(R1)                                                         
         USING SPTSTFFD,RA         RA=A(TWA)                                    
         L     R9,16(R1)                                                        
         USING COMFACSD,R9         R9=A(COMFACS)                                
         MVC   ATIA,12(R1)                                                      
         XC    DBLOCK,DBLOCK                                                    
         ST    R9,DBCOMFCS                                                      
         GOTO1 CCALLOV,DMCB,0,X'D9000A03'                                       
         MVC   VDAYPAK,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000A0E'                                          
         MVC   VTIMVAL,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000A29'       REGETIUN                           
         MVC   VREGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A24'       SPGETIUN                           
         MVC   VSPGTIUN,0(R1)                                                   
         L     RE,=V(DUMPOUT)                                                   
         A     RE,RELO                                                          
         ST    RE,VDUMPOUT                                                      
         MVI   DEMOLIST,X'FF'                                                   
         MVI   FERN,0                                                           
         MVC   MAXIO,=F'500'       SET MAXIMUM DEMO TRACE COUNT                 
         MVI   PCTRL2,C' '         SET TO INITIAL DEFAULT VALUE                 
         MVI   PCTRLI,C' '                                                      
         XC    EFLTLST(EFLTLSTQ),EFLTLST   INIT ELEM FILT LIST                  
*                                                                               
***********************************************************************         
*CLRTABS - SPECIAL CALL TO CLEAR DEMO TABLES IN DATASPACE                       
***********************************************************************         
CLRTABS  DS    0H                                                               
         LA    R1,DEMFUNCH                                                      
         CLI   5(R1),0                                                          
         BE    SP00                                                             
         CLC   =C'CLRTAB',DEMFUNC                                               
         BNE   SP00                                                             
         XC    TBLPARM,TBLPARM                                                  
         OC    DEMFILE,SPACES      BLANK PAD                                    
         LA    R1,DEMFILEH                                                      
         ST    R1,FADR                                                          
         L     R2,=A(CLRTAB)                                                    
         A     R2,RELO                                                          
CLR10    CLI   0(R2),X'FF'                                                      
         BE    EIIF                                                             
         CLC   DEMFILE,0(R2)                                                    
         BE    *+12                                                             
         LA    R2,L'CLRTAB(R2)                                                  
         B     CLR10                                                            
         MVC   TBLPARM(1),8(R2)                                                 
         MVI   TBLPARM+4,X'FF'                                                  
         GOTO1 CDEMADDR,DMCB,(X'FE',TBLPARM),COMFACSD                           
         XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(8),0(R2)                                                  
         MVC   DEMMSG+8(20),=C' CLEARED AND REBUILT'                            
         OI    DEMMSGH+6,X'C8'                                                  
         OI    DEMREPTH+6,X'C1'                                                 
                                                                                
CLRTABSX B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*SP00 -  REGULAR DEMT PROCESSING                                                
***********************************************************************         
SP00     L     R2,=A(INPTBL)                                                    
         A     R2,RELO                                                          
         B     SP2                                                              
         EJECT                                                                  
* VALIDATE ALL SCREEN FIELDS                                                    
*                                                                               
         USING INPTBLD,R2          R2=A(INPUT FIELD TABLE)                      
SP2      CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    SP16                                                             
         LH    R1,INPIFLD                                                       
         LA    R1,SPTSTFFD(R1)                                                  
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
         BNE   *+16                                                             
         TM    INPINDS,X'01'       NO - TEST IF REQUIRED                        
         BNZ   EMIF                                                             
         B     SP14                                                             
         CLC   FLDH+5(1),INPIMIN   TEST INPUT LENGTH                            
         BL    EFTS                                                             
         CLC   FLDH+5(1),INPIMAX                                                
         BH    EFTL                                                             
         MVC   DUB,FLD                                                          
         TM    INPINDS,X'F0'                                                    
         BZ    SP12                                                             
         TM    INPINDS,X'80'       TEST FIELD NUMERIC                           
         BZ    SP6                                                              
         TM    FLDH+4,X'08'        YES - TEST IF INPUT NUMERIC                  
         BZ    EFNN                                                             
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
         ZIC   R3,FLDH+5                                                        
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
         BNE   EXIT                                                             
         TM    INPINDS,X'10'                                                    
         BO    SP14                                                             
*                                                                               
SP12     L     RF,OADR             MOVE OUTPUT TO DBLOCK                        
         ZIC   RE,INPOLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
*                                                                               
SP14     LA    R2,L'INPTBL(R2)     BUMP TO NEXT FIELD                           
         B     SP2                                                              
         DROP  R2                                                               
         EJECT                                                                  
* VALIDATE A BOOK                                                               
*                                                                               
VALBOOK  NTR1                                                                   
         CLC   =C'LAT',FLD         LATEST BOOK REQUEST                          
         BE    VALBKL               YES - VALIDATE MULTI                        
         GOTO1 CDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    VALBOOK2                                                         
*&&DO                                                                           
         PACK  DUB,WORK(2)                                                      
         CVB   R1,DUB                                                           
         PACK  DUB,WORK+2(2)                                                    
         CVB   R0,DUB                                                           
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
*&&                                                                             
         GOTO1 CDATCON,DMCB,(0,WORK),(3,DUB),0                                  
         B     EXIT                                                             
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
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE LATEST BOOK EXPRESSIONS                                              
VALBKL   XC    DUB(2),DUB                                                       
         TM    FLD+3,X'F0'         MULTI BOOK AVERAGE                           
         BNO   EXIT                                                             
         CLI   FLD+3,C'1'          MUST BE AT LEAST 1 BUT                       
         BL    EIIF                                                             
         CLI   FLD+3,C'4'          NO MORE THAN 4                               
         BH    EIIF                                                             
         MVI   DUB,X'FF'           INDICATE LATEST N                            
         MVC   DUB+1(1),FLD+3                                                   
         B     EXIT                                                             
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
                                                                                
         GOTO1 CHEXIN,DMCB,FLD,DUB,2,0                                          
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     VBTX                                                             
                                                                                
*                                                                               
VBTX     DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE ACTUAL DATE (1WEEK OPTION)                                           
*                                                                               
VAL1WK   NTR1                                                                   
         GOTO1 CDATVAL,DMCB,(0,FLD),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    EIIF                                                             
         GOTO1 CDATCON,DMCB,(0,WORK),(2,DUB)                                    
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE ACTUAL WEEK (1 THRU 4 OR ALL)                                        
*                                                                               
VALWKN   NTR1                                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+12                                                             
         MVI   DUB,X'FF'                                                        
         B     EXIT                                                             
         TM    FLDH+4,X'08'                                                     
         BZ    EIIF                                                             
         CLI   FLDH+5,1                                                         
         BNE   EIIF                                                             
         MVC   DUB(1),FLD                                                       
         NI    DUB,X'0F'                                                        
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE DAY EXPRESSION                                                       
*                                                                               
VALDAY   NTR1                                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+12                                                             
         MVI   DUB,X'FF'                                                        
         B     EXIT                                                             
         CLC   FLD(3),=C'VAR'                                                   
         BNE   *+12                                                             
         MVI   DUB,X'90'                                                        
         B     EXIT                                                             
                                                                                
         DS    0H                  VALIDATE FOR "AVN" OR "AVGN"                 
         ZIC   RF,FLDH+5                                                        
         SHI   RF,2                                                             
         EXCLC RF,FLD,=C'AVG'       LOOK FOR "AV" OR "AVG"                      
         BNE   VDAYAVGX              NOPE                                       
         LA    RE,FLD+1(RF)         RE-->LAST CHAR OF INPUT                     
         CLI   0(RE),C'2'           SHOULD BE BETWEEN 2                         
         BL    VDAYAVGX                                                         
         CLI   0(RE),C'6'            AND 6 DAYS                                 
         BH    VDAYAVGX                                                         
         MVC   DUB(1),0(RE)                                                     
         NI    DUB,X'9F'            X'90' BITS ==> VAR OR AVG-N                 
         B     EXIT                                                             
VDAYAVGX EQU   *                                                                
                                                                                
         ZIC   R0,FLDH+5                                                        
         GOTO1 VDAYPAK,DMCB,((R0),FLD),DUB,=X'17'                               
         CLI   DUB,0                                                            
         BE    EIIF                                                             
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE TIME EXPRESSION                                                      
*                                                                               
VALTIME  NTR1                                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+14                                                             
         MVC   DUB(4),=AL2(0600,2600)                                           
         B     EXIT                                                             
         ZIC   R0,FLDH+5                                                        
         GOTO1 VTIMVAL,DMCB,((R0),FLD),DUB                                      
         CLI   0(R1),X'FF'                                                      
         BE    EIIF                                                             
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE PURE NUMBER                                                          
*                                                                               
VALPURE  NTR1                                                                   
         TM    FLDH+4,X'08'                                                     
         BZ    EFNN                                                             
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
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE DEMOS                                                                
*                                                                               
VALDEMO  NTR1                                                                   
         LA    R1,DEMDEMOH                                                      
         ST    R1,FADR                                                          
         CLC   FLD(6),=C'MAXIO='   TEST MAXIO=NNNN INPUT                        
         BNE   VALDEMO2                                                         
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'7'                                                         
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
         B     EXIT                                                             
VALDEMO2 CLI   FLDH+5,4            TEST FOR XALL (X=MODIFIER)                   
         BNE   VALDEMO6                                                         
         CLC   FLD+1(3),=C'ALL'                                                 
         BNE   VALDEMO6                                                         
         LA    RE,DEMOLIST         BUILD LIST CONTAING ALL DEMOS                
         L     R1,=A(ALLDEMOS)                                                  
         A     R1,RELO                                                          
VALDEMO4 CLI   0(R1),255                                                        
         BE    EXIT                                                             
         MVI   0(RE),0             PRECISION                                    
         MVC   1(1,RE),FLD         MODIFIER                                     
         MVC   2(1,RE),0(R1)       DEMO                                         
         LA    RE,3(RE)            BUMP TO NEXT DEMOLIST ENTRY                  
         MVI   0(RE),X'FF'         SET E-O-L                                    
         LA    R1,1(R1)                                                         
         B     VALDEMO4                                                         
VALDEMO6 MVI   DBDEMTYP,0                                                       
         GOTO1 CDEMOVAL,DMCB,DEMDEMOH,(10,DEMOLIST),DBLOCK                      
         CLI   4(R1),0                                                          
         BNE   EXIT                                                             
         B     EIIF                                                             
         SPACE 1                                                                
* VALIDATE PRINT CONTROL #2                                                     
*                                                                               
VALPRINT NTR1                                                                   
         CLI   FLD,PKEYONLY        KEYS ONLY                                    
         BE    *+12                                                             
         CLI   FLD,PDEFAULT        DEFAULT TO PRINT ENTIRE RECORD               
         BNE   EIIF                                                             
         MVC   PCTRL2,FLD                                                       
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE ELEMENT FILTER LIST                                                  
*                                                                               
VALEFILT NTR1                                                                   
         CLI   PCTRL2,C' '         CAN NOT HAVE BOTH PRINT?                     
         BNE   EFNP                 AND ELEM FILTER FIELD                       
                                                                                
         LA    RE,IO               USE IO FIELD AS A SCAN BLOCK                 
         LA    RF,2000                                                          
         XCEF                                                                   
                                                                                
         GOTO1 CSCANNER,DMCB,DEMEFLTH,(7,IO),C',=,-'                            
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         ZIC   R0,4(R1)            R0 = # OF ENTRIES                            
         STC   R0,EFLTLST                                                       
         LA    R2,IO                                                            
         LA    R3,EFLTLST+1                                                     
*                                                                               
VALEFLT2 CLI   0(R2),0             MUST HAVE SOMETHING IN 1ST FIELD             
         BE    EIIF                                                             
         CLI   0(R2),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         BH    EIIF                                                             
         TM    2(R2),X'20'          MUST BE VALID HEX                           
         BZ    EIIF                                                             
         ZIC   R4,0(R2)            R4 = L(ENTRY)                                
         GOTO1 CHEXIN,DMCB,12(R2),0(R3),(R4),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         MVC   1(1,R3),0(R3)       FILL IN DEFAULT END OF RANGE                 
                                                                                
         CLI   1(R2),0             IS THERE A SECOND HALF?                      
         BE    VALEFLT4             NOPE                                        
         CLI   1(R2),2              YEP                                         
         BH    EIIF                                                             
         TM    3(R2),X'20'         MUST BE VALID HEX                            
         BZ    EIIF                                                             
         ZIC   R4,1(R2)                                                         
         GOTO1 CHEXIN,DMCB,22(R2),1(R3),(R4),0                                  
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
         B     EXIT                                                             
         EJECT                                                                  
IUNFILT  NTR1                                                                   
         MVI   IUNFLG,C'Y'                                                      
         LA    RE,IO               USE IO FIELD AS A SCAN BLOCK                 
         LA    RF,2000                                                          
         XCEF                                                                   
                                                                                
         CLI   DEMIUNFH+5,1                                                     
         BNE   IUNFLT1                                                          
         CLI   DEMIUNF,C'K'        ONLY PRINT KEY?                              
         BNE   *+12                                                             
         MVI   PCTRLI,C'K'                                                      
         B     EXIT                                                             
         CLI   DEMIUNF,C'Y'        CONVERT TO IUN FORMAT?                       
         BE    EXIT                                                             
*                                                                               
IUNFLT1  GOTO1 CSCANNER,DMCB,DEMIUNFH,(7,IO),C',=,-'                            
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         ZIC   R0,4(R1)            R0 = # OF ENTRIES                            
         STC   R0,IUNFLST                                                       
         LA    R2,IO                                                            
         LA    R3,IUNFLST+1                                                     
*                                                                               
IUNFLT2  CLI   0(R2),0             MUST HAVE SOMETHING IN 1ST FIELD             
         BE    EIIF                                                             
         CLI   0(R2),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         BH    EIIF                                                             
         TM    2(R2),X'20'          MUST BE VALID HEX                           
         BZ    EIIF                                                             
         ZIC   R4,0(R2)            R4 = L(ENTRY)                                
         GOTO1 CHEXIN,DMCB,12(R2),0(R3),(R4),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         BNZ   EIIF                                                             
         MVC   1(1,R3),0(R3)       FILL IN DEFAULT END OF RANGE                 
                                                                                
         CLI   1(R2),0             IS THERE A SECOND HALF?                      
         BE    IUNFLT4              NOPE                                        
         CLI   1(R2),2              YEP                                         
         BH    EIIF                                                             
         TM    3(R2),X'20'         MUST BE VALID HEX                            
         BZ    EIIF                                                             
         ZIC   R4,1(R2)                                                         
         GOTO1 CHEXIN,DMCB,22(R2),1(R3),(R4),0                                  
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
         B     EXIT                                                             
         EJECT                                                                  
* PRINT REQUEST DETAILS PAGE                                                    
*                                                                               
SP16     GOTO1 PRINT,0                                                          
         MVI   PLINE,C'*'                                                       
         MVC   PLINE+1(81),PLINE                                                
         GOTO1 PRINT,1                                                          
         XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(18),=C'DETAILS OF REQUEST'                                
         L     RF,=V(TWANG)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,SPTSTFFD,IO                                            
         LA    R2,IO                                                            
         LA    R0,24                                                            
SP18     MVI   PLINE,C'*'                                                       
         MVC   PLINE+1(80),0(R2)                                                
         MVI   PLINE+81,C'*'                                                    
         GOTO1 PRINT,1                                                          
         LA    R2,80(R2)                                                        
         BCT   R0,SP18                                                          
         MVI   PLINE,C'*'                                                       
         MVC   PLINE+1(81),PLINE                                                
         GOTO1 PRINT,4                                                          
                                                                                
         CLI   DBFUNCT,0           SPECIAL FUNCTION REQUESTED?                  
         BNE   SP18X                NOPE                                        
         L     RE,=A(SPFNCTAB)                                                  
         A     RE,RELO                                                          
         ZIC   R1,DEMFUNCH+5                                                    
         BCTR  R1,0                                                             
SP18A    EXCLC R1,DEMFUNC,0(RE)                                                 
         BE    SP18B                                                            
         LA    RE,L'SPFNCTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   SP18A                                                            
         DC    H'0'                                                             
SP18B    MVC   BYTE,11(RE)         GET THE FLAG                                 
         SR    RF,RF                                                            
         ICM   RF,7,8(RE)                                                       
         A     RF,RELO              AND ADDRESS OF SPECIAL FUNCTION             
         BASR  RE,RF                                                            
         B     SP20                                                             
                                                                                
SP18X    DS    0H                                                               
         ST    R9,DBCOMFCS                                                      
         LA    RE,IO                                                            
         ST    RE,DBAREC                                                        
         TM    DBBTYPE,X'F0'                                                    
         BO    *+8                                                              
         B     *+8                                                              
         NI    DBBTYPE,X'0F'                                                    
*                                                                               
*        CLI   DBSELMED,C'N'                                                    
*        BNE   *+20                                                             
*        TM    DBSTYPE,X'F0'                                                    
*        BO    *+8                                                              
*        B     *+8                                                              
*        NI    DBSTYPE,X'0F'                                                    
*                                                                               
         XC    LACTUAL,LACTUAL                                                  
         XC    LSELECT,LSELECT                                                  
         L     RE,=A(DYTMLIST)     TEST DAY/TIME LIST                           
         A     RE,RELO                                                          
         B     *+8                                                              
         ST    RE,DBEXTEND                                                      
         L     RE,=A(PURELIST)     TEST PURE LIST                               
         A     RE,RELO                                                          
         B     *+8                                                              
         ST    RE,DBEXTEND                                                      
         L     RE,=A(MBKSLIST)     TEST MULTI-BOOKS LIST                        
         A     RE,RELO                                                          
         B     *+8                                                              
         ST    RE,DBEXTEND                                                      
         L     RE,=A(SPOTLIST)     TEST SPOT PRECISION LIST                     
         A     RE,RELO                                                          
         B     *+8                                                              
         ST    RE,DBEXTEND                                                      
         CLC   DBFILE,=C'IUN'                                                   
         BNE   *+12                                                             
         LA    RE,DBXTND1                                                       
         ST    RE,DBEXTEND                                                      
*                                                                               
         MVI   DBSELSPO,0                                                       
*                                                                               
         CLI   DBSELSTA,C'H'                                                    
         BNE   SP19A                                                            
         CLC   DBSELSTA+1,=C'0000'                                              
         BL    SP19A                                                            
         CLC   DBSELSTA+1,=C'9999'                                              
         BH    SP19A                                                            
*                                                                               
*&&DO                                                                           
         MVC   PACK8,=C'00000000'                                               
*&&                                                                             
         MVC   PACK8,=8C'0'                                                     
         MVC   PACK8(4),DBSELSTA+1                                              
         PACK  DUB(4),PACK8(5)                                                  
         MVC   DBSELSTA+1(3),DUB   PWOS                                         
         MVI   DBSELSTA+4,C'C'                                                  
*                                                                               
         MVI   DBSELDUR,X'FF'                                                   
*                                                                               
SP19A    GOTO1 CDEMAND,DMCB,DBLOCK,MYRTN,C'*IO*',MYTRC                          
*                                                                               
SP19     CLI   DBERROR,0                                                        
         BNE   *+8                                                              
         MVI   DBERROR,X'80'                                                    
         BAS   RE,MYRTN                                                         
SP20     GOTO1 PRINT,4                                                          
         GOTO1 PRINT,5                                                          
         XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(16),=C'ACTION COMPLETED'                                  
         OI    DEMMSGH+6,X'80'                                                  
         XC    DEMREPT,DEMREPT                                                  
         MVC   DEMREPT(4),=C'DEM,'                                              
         SR    R1,R1                                                            
         ICM   R1,3,REPNO                                                       
         LA    R3,DEMREPT+4                                                     
         EDIT  (R1),(4,0(R3)),ALIGN=LEFT                                        
         OI    DEMREPTH+6,X'C1'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PRINT A LINE                                                       
*                                                                               
PRINT    NTR1  WORK=(R8,3)                                                      
         SLL   R1,1                                                             
         LA    R2,PRINTAB(R1)                                                   
         CLI   1(R2),0                                                          
         BNE   *+14                                                             
         ZAP   LINE,=P'0'                                                       
         B     *+10                                                             
         AP    LINE,1(1,R2)                                                     
         CP    LINE,=P'58'                                                      
         BNH   *+14                                                             
         ZAP   LINE,1(1,R2)                                                     
         LA    R2,PRINBC01                                                      
         MVC   PCTRL,0(R2)                                                      
         CLI   PCTRL,0                                                          
         BNE   *+16                                                             
         XC    PLINE,PLINE                                                      
         MVC   PLINE(14),=C'DEMTEST    DEM'                                     
         GOTO1 CDATAMGR,(R8),=C'DMPRINT',=C'PRTQUE',0,PCTRL,ATIA                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0                                                          
         BNE   *+10                                                             
         MVC   REPNO,PLINE+18                                                   
         MVC   PLINE,SPACES                                                     
         B     EXIT                                                             
         SPACE 1                                                                
PRINTAB  DS    0XL2                                                             
         DC    X'00',X'00'                                                      
         DC    X'09',P'1'                                                       
         DC    X'11',P'2'                                                       
         DC    X'19',P'3'                                                       
PRINBC01 DC    X'89',X'00'                                                      
         DC    X'FF',X'00'                                                      
         EJECT                                                                  
* DEMAND TRACE ROUTINE                                                          
*                                                                               
         DS    0H                                                               
*&&DO                                                                           
MYRTN    NTR1  WORK=(R8,20)                                                     
*&&                                                                             
MYRTN    NTR1  WORK=(R8,40)                                                     
         USING MYTRCD,R8                                                        
         LA    R2,PLINE                                                         
         CLI   DBERROR,0                                                        
         BE    MYRTN2                                                           
         LA    RF,ERRTAB                                                        
         LA    R1,DBERROR                                                       
         BAS   RE,GETNTRY                                                       
         MVC   0(6,R2),=C'ERROR='                                               
         MVC   6(8,R2),1(RF)                                                    
         LA    R2,15(R2)                                                        
         BAS   RE,GETNEXT                                                       
         LA    RF,MODTAB                                                        
         LA    R1,DBERRMOD                                                      
         BAS   RE,GETNTRY                                                       
         MVC   0(7,R2),=C'ERRMOD='                                              
         MVC   7(8,R2),1(RF)                                                    
         LA    R2,15(R2)                                                        
         B     MYRTN3                                                           
*                                                                               
MYRTN2   DS    0H                                                               
         LA    RF,RECTAB                                                        
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
         GOTO1 PRINT,2                                                          
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
         EXMVC R1,PLINE+7,0(R3)                                                 
         GOTO1 PRINT,1                                                          
         GOTO1 CHEXOUT,DMCB,(R3),PLINE+7,(R0),=C'TOG',0                         
         GOTO1 PRINT,1                                                          
         MVC   PLINE+7(7),SPACES                                                
         GOTO1 PRINT,1                                                          
*                                                                               
MYRTN3A  MVC   PLINE(7),=C'SELECT='                                             
         LA    R2,DBSELECT                                                      
         LA    R3,LSELECT                                                       
         LA    R0,2                                                             
*                                                                               
MYRTN4   CLC   0(64,R2),0(R3)                                                   
         BE    MYRTN5                                                           
         MVC   PLINE+7(64),0(R2)                                                
         GOTO1 CHEXOUT,MYTDMCB,PLINE+7,MYTWORK1,64,=C'SEP'                      
         GOTO1 VDUMPOUT,MYTDMCB,(64,PLINE+7),0,0                                
         GOTO1 PRINT,1                                                          
         MVC   PLINE+7(64),MYTWORK1                                             
         GOTO1 PRINT,1                                                          
         MVC   PLINE+7(64),MYTWORK1+64                                          
         GOTO1 PRINT,2                                                          
MYRTN5   MVC   0(64,R3),0(R2)                                                   
         LA    R3,64(R3)                                                        
         LA    R2,64(R2)                                                        
         MVC   PLINE(7),=C'ACTUAL='                                             
         BCT   R0,MYRTN4                                                        
*                                                                               
         DS    0H                                                               
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
         DS    0H                                                               
         MVC   PLINE+0(07),=C'XDBDQD='                                          
         MVC   PLINE+7(64),0(R2)                                                
         GOTO1 VDUMPOUT,MYTDMCB,(64,PLINE+7),0,0                                
         GOTO1 PRINT,1                                                          
                                                                                
         GOTO1 CHEXOUT,MYTDMCB,(R2),MYTWORK1,64,=C'SEP'                         
         MVC   PLINE+7(64),MYTWORK1                                             
         GOTO1 PRINT,1                                                          
         MVC   PLINE+7(64),MYTWORK1+64                                          
         GOTO1 PRINT,2                                                          
MYRTNDQX EQU   *                                                                
*                                                                               
         MVC   PLINE(7),SPACES                                                  
         GOTO1 PRINT,2                                                          
         CLI   DEMOLIST,X'FF'                                                   
         BE    EXIT                                                             
         CLI   DBRECTYP,DBRECNTI                                                
         BE    *+8                                                              
         CLI   DBRECTYP,DBRECDEM                                                
         BNE   EXIT                                                             
         CLI   DBERROR,0                                                        
         BNE   EXIT                                                             
*                                                                               
         MVI   IUNPRT,0                                                         
         XC    DEMOVALS(240),DEMOVALS                                           
         XC    DEMOVALS+240(240),DEMOVALS+240                                   
         GOTO1 CDEMOUT,DMCB,(C'L',DEMOLIST),DBLOCK,DEMOVALS                     
         LA    R2,DEMOLIST                                                      
MYRTN5A  LA    R3,DEMOVALS                                                      
         LA    R4,PLINE                                                         
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
         GOTO1 PRINT,1             PRINT LINE & SET-UP FOR NEXT                 
         LA    R4,PLINE                                                         
         LA    R5,6                                                             
MYRTN6A  LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         B     MYRTN6                                                           
*                                                                               
MYRTN7   CLC   PLINE,SPACES                                                     
         BE    MYRTN7A                                                          
         GOTO1 PRINT,1                                                          
*                                                                               
MYRTN7A  DS    0H                                                               
*        CLI   DBSELMED,C'N'       NETWORK FILE?                                
*        BNE   EXIT                                                             
         CLI   IUNFLG,C'Y'         CONVERT TO IUN FORMAT?                       
         BNE   EXIT                                                             
         CLI   IUNPRT,0                                                         
         BNE   EXIT                ALREADY PROCESSED                            
         BAS   RE,CNVIUN           CONVERT NET RECD TO IUN FMT                  
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
         DROP  R8                                                               
         EJECT                                                                  
         EJECT                                                                  
* TABLE OF ERROR MESSAGES                                                       
*                                                                               
ERRTAB   DS    0C                                                               
         DC    AL1(INVCMND),CL8'INVCMND'                                        
         DC    AL1(INVFILE),CL8'INVFILE'                                        
         DC    AL1(INVMED),CL8'INVMED'                                          
         DC    AL1(INVSRC),CL8'INVSRC'                                          
         DC    AL1(INVFM),CL8'INVFM'                                            
         DC    AL1(INVFMS),CL8'INVFMS'                                          
         DC    AL1(INVELEM),CL8'INVELEM'                                        
         DC    AL1(NODSPTAB),CL8'NODSPTAB'                                      
         DC    AL1(NOMAST),CL8'NOMAST'                                          
         DC    AL1(NOFORM),CL8'NOFORM'                                          
         DC    AL1(NOLBOOK),CL8'NOLBOOK'                                        
         DC    AL1(TOODEEP),CL8'TOODEEP'                                        
         DC    AL1(FORMLOOP),CL8'FORMLOOP'                                      
         DC    AL1(DIVZERO),CL8'DIVZERO'                                        
         DC    AL1(RECLONG),CL8'RECLONG'                                        
         DC    AL1(INVMRKT),CL8'INVMRKT'                                        
         DC    AL1(NODQTAB),CL8'NODQTAB'                                        
         DC    AL1(NOTFOUND),CL8'NOTFOUND'                                      
         DC    AL1(EOF),CL8'EOF'                                                
         DC    X'FE',CL8'FORCEOF'                                               
         DC    X'FF',CL8'UNKNOWN'                                               
         SPACE 1                                                                
* TABLE OF ERROR MODULES                                                        
*                                                                               
MODTAB   DS    0X                                                               
         DC    AL1(EDEMADDR),CL8'DEMADDR'                                       
         DC    AL1(EDEMAINT),CL8'DEMAINT'                                       
         DC    AL1(EDEMAND),CL8'DEMAND'                                         
         DC    AL1(EDEMEL),CL8'DEMEL'                                           
         DC    AL1(EDEMOUT),CL8'DEMOUT'                                         
         DC    X'FF',CL8'UNKNOWN'                                               
         SPACE 1                                                                
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
* ROUTINE TO PRINT I/O TRACE                                                    
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
MYTRC    NTR1  WORK=(R8,20)                                                     
         USING MYTRCD,R8                                                        
         LR    R2,R1                                                            
         L     R1,0(R2)                                                         
         MVC   PLINE(7),0(R1)                                                   
         L     R1,4(R2)                                                         
         MVC   PLINE+8(7),0(R1)                                                 
         GOTO1 CHEXOUT,MYTDMCB,8(R2),PLINE+16,1,=C'TOG'                         
         L     R3,12(R2)                                                        
         L     R1,4(R2)                                                         
         CLC   DBFILNAM,0(R1)                                                   
         BE    MYTRC2                                                           
         CLC   DBDIRNAM,0(R1)                                                   
         BE    *+12                                                             
         TM    0(R2),X'80'                                                      
         BZ    MYTRC2                                                           
         CLC   DBFILE,=C'IUN'                                                   
         BE    MYTRC1B                                                          
         MVC   PLINE+19(5),=C'IKEY='                                            
         MVC   PLINE+24(18),DBKEY                                               
         MVC   PLINE+42(6),=C',OKEY='                                           
         MVC   PLINE+48(23),0(R3)                                               
         GOTO1 VDUMPOUT,MYTDMCB,(52,PLINE+19),0,0                               
         GOTO1 PRINT,1                                                          
         GOTO1 CHEXOUT,MYTDMCB,DBKEY,MYTWORK1,18,=C'SEP'                        
         GOTO1 (RF),(R1),(R3),MYTWORK2,23,=C'SEP'                               
         MVC   PLINE+24(18),MYTWORK1                                            
         MVC   PLINE+48(23),MYTWORK2                                            
         GOTO1 PRINT,1                                                          
         MVC   PLINE+24(18),MYTWORK1+18                                         
         MVC   PLINE+48(23),MYTWORK2+23                                         
         GOTO1 PRINT,2                                                          
         B     MYTRCX                                                           
                                                                                
MYTRC1B  DS    0H                                                               
         ZICM  R3,9(R2),(7)        R1-->INPUT KEY TO DATAMGR                    
         MVC   PLINE+19(5),=C'IKEY='                                            
         MVC   PLINE+24(27),0(R3)                                               
         GOTO1 CHEXOUT,MYTDMCB,(R3),MYTWORK1,27,=C'SEP'                         
                                                                                
         ZICM  R3,13(R2),(7)       R1-->OUTPUT KEY TO DATAMGR                   
         MVC   PLINE+51(6),=C',OKEY='                                           
         MVC   PLINE+57(32),0(R3)                                               
         GOTO1 CHEXOUT,MYTDMCB,(R3),MYTWORK2,32,=C'SEP'                         
         GOTO1 PRINT,1                                                          
                                                                                
         MVC   PLINE+24(27),MYTWORK1                                            
         MVC   PLINE+57(32),MYTWORK2                                            
         GOTO1 PRINT,1                                                          
         MVC   PLINE+24(27),MYTWORK1+27                                         
         MVC   PLINE+57(32),MYTWORK2+32                                         
         GOTO1 PRINT,2                                                          
         B     MYTRCX                                                           
*&&                                                                             
         PRINT ON                                                               
         DS    0H                                                               
MYTRC    NTR1  WORK=(R8,40)                                                     
         USING MYTRCD,R8                                                        
         LR    R2,R1                                                            
                                                                                
         DS    0H                  CLEAR WORK AREA                              
         LR    R0,R8                                                            
         LA    R1,MYTRCDQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         DS    0H                  GET PARAMETERS                               
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
         LA    R4,PLINE                                                         
         MVC   0(L'MYTIOCMD,R4),MYTIOCMD                                        
         LA    R4,L'MYTIOCMD+1(R4)                                              
         MVC   0(L'MYTFILNM,R4),MYTFILNM                                        
         LA    R4,L'MYTFILNM+1(R4)                                              
         GOTO1 CHEXOUT,MYTDMCB,8(R2),(R4),1,=C'TOG'                             
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
         LA    R4,PLINE+19                                                      
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         L     R6,MYTIADR                                                       
         MVC   0(5,R4),=C'IKEY='                                                
         EXMVC R5,5(R4),0(R6)                                                   
         LA    R5,1(R5)                                                         
         GOTO1 CHEXOUT,MYTDMCB,(R6),MYTWORK1,(R5),=C'SEP'                       
         LA    R4,5(R5,R4)                                                      
                                                                                
         LA    R5,5(R5)            BUMP LENGTH UP FOR CNTL & D/A                
         BCTR  R5,0                                                             
         L     R6,MYTOADR                                                       
         MVC   0(6,R4),=C',OKEY='                                               
         EXMVC R5,6(R4),0(R6)                                                   
         LA    R5,1(R5)                                                         
         GOTO1 CHEXOUT,MYTDMCB,(R6),MYTWORK2,(R5),=C'SEP'                       
         GOTO1 VDUMPOUT,MYTDMCB,(L'PLINE,PLINE),0,0                             
         GOTO1 PRINT,1                                                          
                                                                                
         DS    0H                  DISPLAY HEX OUTPUT                           
         LA    R4,PLINE+24                                                      
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         EXMVC R5,0(R4),MYTWORK1                                                
         LA    R4,6+1(R5,R4)                                                    
         LA    R5,5(R5)                                                         
         EXMVC R5,0(R4),MYTWORK2                                                
         GOTO1 PRINT,1                                                          
                                                                                
         LA    R4,PLINE+24                                                      
         LH    R5,MYTKYLEN                                                      
         BCTR  R5,0                                                             
         LA    R6,MYTWORK1+1(R5)                                                
         EXMVC R5,0(R4),0(R6)                                                   
         LA    R4,6+1(R5,R4)                                                    
         LA    R5,5(R5)                                                         
         LA    R6,MYTWORK2+1(R5)                                                
         EXMVC R5,0(R4),0(R6)                                                   
         GOTO1 PRINT,2                                                          
         B     MYTRCX                                                           
*                                                                               
MYTRC2   MVC   PLINE+19(5),=C'NXDA='                                            
         LA    R0,DBNDXDA                                                       
         CLC   DBFILE,=C'IUN'                                                   
         BNE   MYTRC2B                                                          
         L     R0,MYTIADR                                                       
                                                                                
MYTRC2B  DS    0H                  R0-->DISK ADDRESS                            
*&&DO                                                                           
         GOTO1 CHEXOUT,MYTDMCB,DBNDXDA,PLINE+24,4,=C'TOG'                       
*&&                                                                             
         GOTO1 CHEXOUT,MYTDMCB,(R0),PLINE+24,4,=C'TOG'                          
*                                                                               
MYTRC4   MVC   PLINE+32(6),=C',IKEY='                                           
         CLC   DBFILE,=C'IUN'                                                   
         BE    MYTRC4B                                                          
         MVC   PLINE+38(18),DBKEY                                               
         MVC   PLINE+56(2),DBMINKEY                                             
         MVC   PLINE+60(9),=C'DBSTATUS='                                        
         GOTO1 CHEXOUT,MYTDMCB,DBSTATUS,PLINE+69,1,=C'TOG'                      
         GOTO1 CHEXOUT,MYTDMCB,PLINE+38,MYTWORK1,20,=C'SEP'                     
         GOTO1 VDUMPOUT,MYTDMCB,(20,PLINE+38),0,0                               
         GOTO1 PRINT,1                                                          
         MVC   PLINE+38(20),MYTWORK1                                            
         GOTO1 PRINT,1                                                          
         MVC   PLINE+38(20),MYTWORK1+20                                         
         GOTO1 PRINT,2                                                          
         B     MYTRC6                                                           
                                                                                
MYTRC4B  DS    0H                                                               
         L     R3,MYTOADR          R3-->OUTPUT KEY FROM DATAMGR                 
         MVC   PLINE+38(34),0(R3)                                               
         GOTO1 CHEXOUT,MYTDMCB,PLINE+38,MYTWORK1,34,=C'SEP'                     
         GOTO1 PRINT,1                                                          
         MVC   PLINE+38(34),MYTWORK1                                            
         GOTO1 PRINT,1                                                          
         MVC   PLINE+38(34),MYTWORK1+34                                         
         GOTO1 PRINT,2                                                          
*                                                                               
MYTRC6   DS    0H                                                               
*&&DO                                                                           
MYTRC6   TM    8(R2),X'80'                                                      
*&&                                                                             
         CLC   DBFILNAM,MYTFILNM                                                
         BE    MYTRC6B                                                          
         TM    8(R2),X'80'                                                      
         BNZ   MYTRCX                                                           
                                                                                
MYTRC6B  DS    0H                                                               
         CLI   PCTRL2,PKEYONLY     PRINT KEYS ONLY?                             
         BE    MYTRCX               YES, SO EXIT                                
         MVC   PLINE(4),=C'OREC'                                                
         L     R3,MYTOADR                                                       
         SR    R5,R5                                                            
*&&DO                                                                           
         LA    R4,DRFRSTEL-DRKEY                                                
*&&                                                                             
         LH    R4,MYTDS1EL                                                      
         MVI   WORK,1                                                           
         BAS   RE,MYTRC8                                                        
         EJECT                                                                  
MYTRC7   CLI   0(R3),0                                                          
         BE    MYTRCX                                                           
         ZIC   R4,1(R3)                                                         
         CLI   EFLTLST,0           ANY ELEM TO FILTER?                          
         BE    MYTRC7B              NO, PRINT ELEM                              
         ZIC   RF,EFLTLST           YES, CHECK IF ELEM SELECTED                 
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
MYTRC8   LR    R0,RE                                                            
MYTRC9   LA    R6,32                                                            
         CR    R4,R6                                                            
         BH    *+6                                                              
         LR    R6,R4                                                            
         MVI   PLINE+4,C'('                                                     
         CVD   R5,MYTDUB                                                        
         UNPK  PLINE+5(3),MYTDUB                                                
         OI    PLINE+7,X'F0'                                                    
         MVI   PLINE+8,C'-'                                                     
         AR    R5,R6                                                            
         BCTR  R5,0                                                             
         CVD   R5,MYTDUB                                                        
         UNPK  PLINE+9(3),MYTDUB                                                
         OI    PLINE+11,X'F0'                                                   
         MVI   PLINE+12,C')'                                                    
         LA    R5,1(R5)                                                         
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         CLI   WORK,1                                                           
         BNE   *+12                                                             
         EX    R1,MYTRCEX1                                                      
         B     *+16                                                             
         MVI   WORK,1                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,MYTRCEX                                                       
         GOTO1 PRINT,1                                                          
         GOTO1 CHEXOUT,MYTDMCB,(R3),PLINE+14,(R6),=C'TOG'                       
         GOTO1 PRINT,1                                                          
         AR    R3,R6               POINT TO NEXT ELEMENT                        
         SR    R4,R6               TEST IF ANY LEFT OF CURRENT                  
         BNZ   MYTRC9                                                           
         GOTO1 PRINT,1                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
MYTRCEX1 MVC   PLINE+14(0),0(R3)                                                
MYTRCEX  MVC   PLINE+18(0),2(R3)                                                
*                                  EXIT FROM TRACE ROUTINE                      
MYTRCX   L     R1,MAXIO            DECREMENET MAXIMUM I/O COUNT                 
         SH    R1,=H'1'                                                         
         BNP   *+12                                                             
         ST    R1,MAXIO                                                         
         B     EXIT                                                             
         MVI   DBERROR,X'FE'       SET FORCED E-O-F                             
         L     RD,SAVERD           RESTORE REGISTERS                            
         LM    RE,RC,12(RD)                                                     
         B     SP19                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
*SPECIAL FUNCTION ROUTINES                                                      
***********************************************************************         
SPFNCA   NTR1  WORK=(R8,15)                                                     
         USING SPFNCAD,R8                                                       
         LA    R4,PLINE                                                         
         USING PLINED,R4                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         ST    R9,DBCOMFCS                                                      
         LA    RE,IO                                                            
         ST    RE,DBAREC                                                        
         GOTO1 CDEMAND,SPFDMCB,DBLOCK,0,0,0                                     
         CLI   DBERROR,0                                                        
         BNE   SPFNCAX                                                          
                                                                                
         XC    SPFDCNT,SPFDCNT                                                  
         XC    SPFOKEY,SPFOKEY                                                  
         MVC   SPFOKEY(L'DBKEY),DBKEY                                           
         MVC   SPFOSVKY,SPFOKEY                                                 
         GOTO1 CDATAMGR,SPFDMCB,=C'DMRDHI',DBDIRNAM,SPFOSVKY,SPFOKEY            
         B     SPFNCA2A                                                         
SPFNCA2  GOTO1 CDATAMGR,SPFDMCB,=C'DMRSEQ',DBDIRNAM,SPFOSVKY,SPFOKEY            
SPFNCA2A CLI   8(R1),0                                                          
         BNE   SPFNCAX                                                          
         CLC   SPFOKEY(3),SPFOSVKY                                              
         BNE   SPFNCAX                                                          
         CLI   BYTE,C'F'           IF GETFIL FUNCTION,                          
         BE    SPFNCA4              THEN READ FILE                              
                                                                                
         MVC   PLFILNAM,DBDIRNAM                                                
         MVC   PL2KEY,SPFOKEY                                                   
         LA    R0,L'PL2KEY                                                      
         GOTO1 CHEXOUT,SPFDMCB,SPFOKEY,PL2HXKEY,(R0),=C'TOG',0                  
         GOTO1 PRINT,1                                                          
         LH    R1,SPFDCNT                                                       
         LA    R1,1(R1)                                                         
         CH    R1,=H'200'          LIST A MAX OF 200 RECORDS                    
         BNL   SPFNCAX                                                          
         STH   R1,SPFDCNT                                                       
         MVC   SPFOSVKY,SPFOKEY                                                 
         B     SPFNCA2                                                          
                                                                                
SPFNCA4  MVC   SPFDA,DBNDXDA                                                    
         MVC   IO(20),SPFOKEY                                                   
         GOTO1 CDATAMGR,SPFDMCB,=C'DMRDHI',DBFILNAM,SPFDA,IO                    
         B     SPFNCA4B                                                         
SPFNCA4A GOTO1 CDATAMGR,SPFDMCB,=C'DMRSEQ',DBFILNAM,SPFDA,IO                    
SPFNCA4B CLI   8(R1),0                                                          
         BNE   SPFNCAX                                                          
         MVC   PLFILNAM,DBFILNAM                                                
         MVC   PL1NXDA1,=C'NXDA='                                               
         LA    R0,L'SPFDA                                                       
         GOTO1 CHEXOUT,SPFDMCB,SPFDA,PL1NXDA2,(R0),=C'TOG',0                    
         MVI   PL1COMMA,C','                                                    
         LA    R0,L'DBKEY+5                                                     
         LA    R1,PL1KEY                                                        
         LA    RF,IO                                                            
         BAS   RE,PUTKEY                                                        
         GOTO1 PRINT,1                                                          
         LA    R0,L'DBKEY+5                                                     
         GOTO1 CHEXOUT,SPFDMCB,IO,PL1HXKEY,(R0),=C'TOG',0                       
         GOTO1 PRINT,1                                                          
         B     SPFNCA4A                                                         
*                                                                               
SPFNCAX  XIT1                                                                   
         DROP  R4,R8                                                            
         SPACE 3                                                                
PUTKEY   DS    0H                                                               
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,2(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PUTKEY                                                        
         BR    RE                                                               
         EJECT                                                                  
* ERRORS & EXIT                                                                 
*                                                                               
EMIF     MVI   FERN,1                                                           
         B     ERROR                                                            
EIIF     MVI   FERN,2                                                           
         B     ERROR                                                            
EFNN     MVI   FERN,3                                                           
         B     ERROR                                                            
EFTS     MVI   FERN,4                                                           
         B     ERROR                                                            
EFTL     MVI   FERN,5                                                           
         B     ERROR                                                            
EFNP     MVI   FERN,6                                                           
         B     ERROR                                                            
*                                                                               
ERROR    XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(15),=C'** ERROR NNN **'                                   
         ZIC   R1,FERN                                                          
         CVD   R1,DUB                                                           
         UNPK  DEMMSG+9(3),DUB                                                  
         OI    DEMMSG+11,X'F0'                                                  
         LA    R0,L'MSGTAB                                                      
         BCTR  R1,0                                                             
         MR    R0,R0                                                            
         L     RF,=A(MSGTAB)                                                    
         A     RF,RELO                                                          
         AR    R1,RF                                                            
*****    LA    R1,MSGTAB(R1)                                                    
         MVC   DEMMSG+16(L'MSGTAB),0(R1)                                        
         OI    DEMMSGH+6,X'80'                                                  
         L     R1,FADR                                                          
         OI    6(R1),X'40'                                                      
*                                                                               
EXIT     CLI   FERN,0                                                           
XIT      XIT1                                                                   
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*CNVIUN- CONVERT INPUT TO IUN RECD AND DISPLAY RECD AND DEMO ELEMS              
***********************************************************************         
DCONPRT  NTR1                                                                   
DCONPRTX XIT1                                                                   
*                                                                               
*&&                                                                             
***********************************************************************         
*CNVIUN- CONVERT INPUT TO IUN RECD AND DISPLAY RECD AND DEMO ELEMS              
***********************************************************************         
CNVIUN   NTR1  WORK=(R8,500)                                                    
         USING CNVIUND,R8                                                       
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
         GOTO1 CGETFACT,DMCB,0                                                  
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
         MVC   RINVKSRC,DBSELSRC                                                
         MVC   RINVKBK,DBACTBK                                                  
         DROP  RF                                                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSELSRC,C'N'                                                    
         ST    R9,DBCOMFCS                                                      
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
         GOTO1 CDEMAINT,DMCB,PUT,DBLOCK,IUNWORK,TMPWRK1                         
         CLI   DBERROR,0                                                        
         BNE   CNVIUNX             NO DBAQUART PASSED                           
*                                                                               
         LA    R3,IUNREC          OUTPUT IUN KEY                                
         LH    R4,=Y(RINVPEL-RINVREC)                                           
         MVI   WORK,1                                                           
         MVC   PLINE(4),=C'IUN '                                                
         BAS   RE,CNVPRT                                                        
         CLI   PCTRLI,C'K'         ONLY PRINT KEY?                              
         BE    CNV50                                                            
*                                                                               
CNV40    CLI   0(R3),0                                                          
         BE    CNV50               DONE                                         
         ZIC   R4,1(R3)                                                         
         MVI   WORK,0                                                           
         BAS   RE,CNVPRT                                                        
         B     CNV40                                                            
*                                                                               
CNV50    DS    0H                  READ DEMOS FROM IUN RECORD                   
         LA    RE,DEMOLST2                                                      
         MVC   DEMOLST2(DEMLISTQ),DEMOLIST                                      
CNV55    L     RF,=A(IUNNHT)       CONVERT NET MODIFIERS TO IUN MDFS            
         A     RF,RELO                                                          
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
         GOTO1 CDEMOUT,DMCB,(C'L',DEMOLST2),DBLOCK,DEMOVALS                     
*                                                                               
CNVIUNX  MVC   DBLOCK,SVDBLK                                                    
         B     XIT                                                              
*                                                                               
*------> PROCEDURE TO PRINT RECD ELEMS FOR BUILT IUN RECD <-----------          
CNVPRT   LR    R0,RE                                                            
CNVPRT5  LA    R6,32                                                            
         CR    R4,R6                                                            
         BH    *+6                                                              
         LR    R6,R4                                                            
         MVI   PLINE+4,C'('                                                     
         CVD   R5,TMPDUB                                                        
         UNPK  PLINE+5(3),TMPDUB                                                
         OI    PLINE+7,X'F0'                                                    
         MVI   PLINE+8,C'-'                                                     
         AR    R5,R6                                                            
         BCTR  R5,0                                                             
         CVD   R5,TMPDUB                                                        
         UNPK  PLINE+9(3),TMPDUB                                                
         OI    PLINE+11,X'F0'                                                   
         MVI   PLINE+12,C')'                                                    
         LA    R5,1(R5)                                                         
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         CLI   WORK,1                                                           
         BNE   *+12                                                             
         EX    R1,CNVMOV2                                                       
         B     *+16                                                             
         MVI   WORK,1                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,CNVMOV1                                                       
         GOTO1 PRINT,1                                                          
         GOTO1 CHEXOUT,DMCB,(R3),PLINE+14,(R6),=C'TOG'                          
         GOTO1 PRINT,1                                                          
         AR    R3,R6               POINT TO NEXT ELEMENT                        
         SR    R4,R6               TEST IF ANY LEFT OF CURRENT                  
         BNZ   CNVPRT5                                                          
         GOTO1 PRINT,1                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CNVMOV1  MVC   PLINE+18(0),2(R3)                                                
CNVMOV2  MVC   PLINE+14(0),0(R3)                                                
*                                                                               
* LITERALS ETC.                                                                 
         LTORG                                                                  
*                                                                               
PUT      DC    C'PUT'                                                           
OFORMAT  DC    C'IUNUIUN',X'530B00'     RATING BASED DEMOS                      
OFORMAT2 DC    C'IUNUIUN',X'5A0B00'     IMP BASED DEMOS                         
*                                                                               
SPACES   DC    CL133' '                                                         
*                                                                               
* TABLE OF INPUT FIELDS (SEE INPTBLD)                                           
*                                                                               
INPTBL   DS    0XL11                                                            
         DC    AL2(DEMFUNCH-SPTSTFFD,DBFUNCT-DBLOCK)                            
         DC    AL1(4,8,L'DBFUNCT),X'41',AL3(FUNCTAB)                            
         DC    AL2(DEMFILEH-SPTSTFFD,DBFILE-DBLOCK)                             
         DC    AL1(1,3,L'DBFILE),X'40',AL3(FILETAB)                             
         DC    AL2(DEMMEDH-SPTSTFFD,DBSELMED-DBLOCK)                            
         DC    AL1(1,5,L'DBSELMED),X'40',AL3(MEDTAB)                            
         DC    AL2(DEMSRCH-SPTSTFFD,DBSELSRC-DBLOCK)                            
         DC    AL1(1,3,L'DBSELSRC),X'40',AL3(SRCTAB)                            
         DC    AL2(DEMSTAH-SPTSTFFD,DBSELSTA-DBLOCK)                            
         DC    AL1(3,5,L'DBSELSTA),X'00',AL3(0)                                 
         DC    AL2(DEMSTYPH-SPTSTFFD,DBSTYPE-DBLOCK)                            
         DC    AL1(1,1,L'DBSTYPE),X'00',AL3(0)                                  
         DC    AL2(DEMAMKTH-SPTSTFFD,DBSELALF-DBLOCK)                           
         DC    AL1(1,3,L'DBSELALF),X'00',AL3(0)                                 
         DC    AL2(DEMRMKTH-SPTSTFFD,DBSELRMK-DBLOCK)                           
         DC    AL1(1,4,L'DBSELRMK),X'80',AL3(0)                                 
         DC    AL2(DEMKMKTH-SPTSTFFD,DBSELMK-DBLOCK)                            
         DC    AL1(1,5,L'DBSELMK),X'80',AL3(0)                                  
         DC    AL2(DEMUMKTH-SPTSTFFD,DBSELUMK-DBLOCK)                           
         DC    AL1(1,4,L'DBSELUMK),X'80',AL3(0)                                 
         DC    AL2(DEMPROGH-SPTSTFFD,DBSELPRG-DBLOCK)                           
         DC    AL1(1,5,L'DBSELPRG),X'80',AL3(0)                                 
         DC    AL2(DEMINVH-SPTSTFFD,DBSELINV-DBLOCK)                            
         DC    AL1(1,4,L'DBSELINV),X'00',AL3(0)                                 
         DC    AL2(DEMPUREH-SPTSTFFD,DBSELPUR-DBLOCK)                           
         DC    AL1(4,4,L'DBSELPUR),X'20',AL3(VALPURE)                           
         DC    AL2(DEMBOOKH-SPTSTFFD,DBSELBK-DBLOCK)                            
         DC    AL1(4,6,L'DBSELBK),X'20',AL3(VALBOOK)                            
         DC    AL2(DEMBTYPH-SPTSTFFD,DBBTYPE-DBLOCK)                            
         DC    AL1(1,2,L'DBBTYPE),X'20',AL3(VALBTYP)                            
         DC    AL2(DEMB1WKH-SPTSTFFD,DBSEL1WK-DBLOCK)                           
         DC    AL1(8,8,L'DBSEL1WK),X'20',AL3(VAL1WK)                            
         DC    AL2(DEMBWKNH-SPTSTFFD,DBSELWKN-DBLOCK)                           
         DC    AL1(1,3,L'DBSELWKN),X'20',AL3(VALWKN)                            
         DC    AL2(DEMBLIMH-SPTSTFFD,DBSELDAT-DBLOCK)                           
         DC    AL1(4,6,L'DBSELDAT),X'20',AL3(VALBOOK)                           
         DC    AL2(DEMAGYH-SPTSTFFD,DBSELAGY-DBLOCK)                            
         DC    AL1(2,2,L'DBSELAGY),X'00',AL3(0)                                 
         DC    AL2(DEMCLIH-SPTSTFFD,DBSELCLI-DBLOCK)                            
         DC    AL1(2,3,L'DBSELCLI),X'00',AL3(0)                                 
         DC    AL2(DEMDAYSH-SPTSTFFD,DBSELDAY-DBLOCK)                           
         DC    AL1(1,15,L'DBSELDAY),X'20',AL3(VALDAY)                           
         DC    AL2(DEMTIMSH-SPTSTFFD,DBSELTIM-DBLOCK)                           
         DC    AL1(2,12,L'DBSELTIM),X'20',AL3(VALTIME)                          
         DC    AL2(DEMBESTH-SPTSTFFD,DBBEST-DBLOCK)                             
         DC    AL1(1,1,L'DBBEST),X'00',AL3(0)                                   
         DC    AL2(DEMTPTTH-SPTSTFFD,DBTPTT-DBLOCK)                             
         DC    AL1(1,1,L'DBTPTT),X'00',AL3(0)                                   
         DC    AL2(DEMDEMOH-SPTSTFFD,0)                                         
         DC    AL1(1,L'DEMDEMO,0),X'30',AL3(VALDEMO)                            
         DC    AL2(DEMPRNTH-SPTSTFFD,0)                                         
         DC    AL1(1,L'DEMPRNT,0),X'30',AL3(VALPRINT)                           
         DC    AL2(DEMTAPEH-SPTSTFFD,DBTAPEP-DBLOCK)                            
         DC    AL1(1,1,L'DBTAPEP),X'00',AL3(0)                                  
         DC    AL2(DEMEFLTH-SPTSTFFD,0)                                         
         DC    AL1(1,L'DEMEFLT,0),X'30',AL3(VALEFILT)                           
         DC    AL2(DEMPTTH-SPTSTFFD,DBSELPTT-DBLOCK)                            
         DC    AL1(1,1,L'DEMPTT),X'40',AL3(PTTTAB)                              
         DC    AL2(DEMIUNFH-SPTSTFFD,0)                                         
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
         DC    X'FF'                                                            
         SPACE 1                                                                
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
         DC    X'FF'                                                            
*                                                                               
         SPACE 1                                                                
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
         DC    C'CAB',C'CAB'                                                    
         DC    C'IUN',C'IUN'                                                    
         DC    X'FF'                                                            
         SPACE 1                                                                
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
         DC    X'FF'                                                            
         SPACE 1                                                                
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
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
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
         SPACE 1                                                                
* TABLE OF INPUT ERROR MESSAGES                                                 
*                                                                               
MSGTAB   DS    0CL40                                                            
         DC    CL40'MISSING INPUT FIELD'                                        
         DC    CL40'INVALID INPUT FIELD'                                        
         DC    CL40'FIELD NOT NUMERIC'                                          
         DC    CL40'FIELD LENGTH TOO SHORT'                                     
         DC    CL40'FIELD LENGTH TOO LONG'                                      
         DC    CL40'ELEM FILTER CAN NOT HAVE PRINT? INPUT'                      
         SPACE 1                                                                
* EXTENDED LIST SUPPORT                                                         
DYTMLIST DC    C'DYTM',AL4(0),X'00',X'40',AL2(2000),AL2(2030)                   
         DC    X'20',AL2(2100,2130)                                             
         DC    X'0000000000'                                                    
PURELIST DC    C'PURE',AL4(0),X'00',AL2(441,443,484)                            
         DC    X'FFFF'                                                          
MBKSLIST DC    C'MBKS',AL4(0),X'00'                                             
         DC    AL1(100,01)                                                      
         DC    AL1(099,11)                                                      
         DC    X'0000'                                                          
SPOTLIST DC    C'SPOT',AL4(0)                                                   
         DC     X'02'               RTG/PUT                                     
         DC     X'01'               SHARE                                       
         DC     X'02'               IMP                                         
         SPACE 1                                                                
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
*******************************************************************             
* DSECT TO COVER W/S                                                            
*******************************************************************             
*                                                                               
SPTSTD   DSECT                                                                  
*                                                                               
PACK8    DS    PL8                 TEMP FIELD USED IN PACK OPERATION            
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
ATIA     DS    A                                                                
VDAYPAK  DS    A                                                                
VTIMVAL  DS    A                                                                
VDUMPOUT DS    V                                                                
VSPGTIUN DS    V                                                                
VREGTIUN DS    V                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
FADR     DS    A                                                                
OADR     DS    A                                                                
IUNPRT   DS    X                                                                
IUNFLG   DS    X                                                                
MAXIO    DS    F                                                                
REPNO    DS    XL2                                                              
FERN     DS    XL1                                                              
FLDH     DS    XL8                                                              
FLD      DS    XL80                                                             
TBLPARM  DS    XL5                 PARAMETER TO DEMADDR                         
LINE     DS    PL2                                                              
PCTRL    DS    X                                                                
PLINE    DS    CL133                                                            
LSELECT  DS    CL64                                                             
LACTUAL  DS    CL64                                                             
DEMOLIST DS    60XL3,X             WAS 120XL3 BUT MAX DEMOS=10 ?                
DEMOLST2 DS    60XL3,X                                                          
DEMLISTQ EQU   *-DEMOLST2          LENGTH OF DEMOLIST BUFFER                    
DEMOVALS DS    120F                                                             
* DEDBLOCK                                                                      
         DS    0D                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
PCTRLI   DS    CL1                 CNV IUN: Y=PRINT RECORD                      
PCTRL2   DS    CL1                 K=KEYS ONLY, R=DEFAULT                       
PKEYONLY EQU   C'K'                                                             
PDEFAULT EQU   C'R'                PRINT ENTIRE RECORD (DEFAULT)                
EFLTLST  DS    XL1,7XL2            7 RANGES OF ELEM TO FILTER                   
EFLTLSTQ EQU   *-EFLTLST            (E.G. LO-HI,LO-HI, ETC.)                    
IUNFLST  DS    XL1,7XL2            7 RANGES OF ELEM TO FILTER                   
IUNFLSTQ EQU   *-IUNFLST            (E.G. LO-HI,LO-HI, ETC.)                    
BYTE     DS    XL1                                                              
DBXTND1  DS    4XL(DBXINVWL)                                                    
IO       DS    2000C                                                            
SPTSTDX  EQU   *                                                                
         SPACE 1                                                                
*******************************************************************             
* DSECT TO COVER I/O TRACE W/S                                                  
*******************************************************************             
*                                                                               
MYTRCD   DSECT                                                                  
MYTDUB   DS    D                                                                
MYTDMCB  DS    6F                                                               
*&&DO                                                                           
MYTWORK1 DS    CL64                                                             
MYTWORK2 DS    CL64                                                             
*&&                                                                             
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
         SPACE 1                                                                
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
CNVIUNDX EQU   *-CNVIUND                                                        
CNVIUNLN EQU   CNVIUNDX/8                                                       
         EJECT                                                                  
*******************************************************************             
*DSECT TO COVER PLINE                                                           
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
         SPACE 1                                                                
         EJECT                                                                  
SPTSTFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SPTSTFFD                                                       
         EJECT                                                                  
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
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
       ++INCLUDE REGENINVA                                                      
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131SPTST00C  05/01/02'                                      
         END                                                                    
