*          DATA SET CTSFM39    AT LEVEL 023 AS OF 02/15/13                      
*PHASE TA0A39A                                                                  
*INCLUDE FATABOFF                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TA0A39 - BROADCAST RECORD REPORT                      *         
*                                                                     *         
*  COMMENTS:    GENERATES PRINTED REPORT OF BROADCAST RECORDS         *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS:      CTSFMD9 (TA0AD9) -- REPORT SCREEN                     *         
*                                                                     *         
*  OUTPUTS:     PRINTED REPORT                                        *         
*                                                                     *         
*  LOCALS:      REGISTER USAGE                                        *         
*               R0 - WORK                                             *         
*               R1 - WORK                                             *         
*               R2 - WORK                                             *         
*               R3 - GETEL REGISTER                                   *         
*               R4 - BROADCAST RECORD DSECT                           *         
*               R5 - WORK                                             *         
*               R6 - WORK                                             *         
*               R7 - WORK                                             *         
*               R8 - SPOOLD                                           *         
*               R9 - SYSD                                             *         
*               RA - TWA                                              *         
*               RB - FIRST BASE                                       *         
*               RC - GEND                                             *         
*               RD - SYSTEM                                           *         
*               RE - SYSTEM                                           *         
*               RF - SYSTEM                                           *         
*                                                                     *         
***********************************************************************         
* MOD LOG:                                                            *         
* --------                                                            *         
*                                                                     *         
* 21APR92  (EFJ) --- URGENCY FIELD REMOVED, DAYS FIELD ADDED          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'TA0A39 BROADCAST RECORD REPORT'                                 
TA0A39   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A39*,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*/                                                                              
*/       MY ROUTINE STARTS HERE                                                 
*/                                                                              
PR       BAS   RE,BUILD            BUILD BROADCAST KEY                          
         BAS   RE,PROC01           CALL MAIN PROCEDURE                          
PRX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*/                                                                              
*/       BUILD - BUILD BROADCAST KEY AND NAME FILTER LIST                       
*/                                                                              
BUILD    NTR1                                                                   
         USING BRDKEYD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   BRDKSYS,BRDKSYSQ    SYSTEM ID X'00'                              
         MVI   BRDKSTYP,BRDKSTYQ   RECORD TYPE X'09'                            
         MVC   BRDKTYPE,SFRTYPE    GET MESSAGE TYPE 'P' OR 'T'                  
         CLI   SFRMSGNH+5,0        MSGN GIVEN?                                  
         BNE   BLD05               YES                                          
         MVC   BRDKMSGN,=H'1'      NO, START FROM FIRST NUM                     
         B     BLD10                                                            
*                                                                               
BLD05    LA    R3,SFRMSGNH         POINT TO INPUT MSGN HEADER                   
         ZIC   R2,5(R3)            GET INPUT LENGTH                             
         BCTR  R2,0                DECREMENT LENGTH                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFRMSGN(0)      CONVERT MSGN TO DECIMAL                      
         CVB   R2,DUB                                                           
         STH   R2,BRDKMSGN         SAVE MESSAGE NUMBER                          
*                                                                               
BLD10    XC    FILTERS,FILTERS     BUILD FILTER LIST                            
         LA    R2,SFRNAMEH         NAME HEADER                                  
         CLI   5(R2),0             ANY FILTER GIVEN?                            
         BE    VK060               NO                                           
         LA    R5,8(R2)            A(NAME)                                      
         LA    R3,FILTERS          A(FILTER LIST)                               
*                                                                               
VK40     CLI   0(R5),C' '          END OF FILTER EXPRESSION?                    
         BNH   VK060               YES                                          
*                                                                               
         CLI   0(R5),C'*'          WILDCARD?                                    
         BNE   *+12                NO                                           
         MVI   0(R3),C'*'          PUT '*' INTO FILTERS                         
         B     VK50                BRANCH MASK IS ALREADY X'00'                 
*                                                                               
         MVI   1(R3),X'70'         ASSUME IT'S A POSITIVE FILTER                
         CLI   0(R5),C'-'          NEGATIVE FILTER?                             
         BNE   *+18                NO                                           
         LA    R5,1(R5)            BUMP PAST MINUS                              
         MVC   0(1,R3),0(R5)       SAVE NEGATIVE FILTER                         
         MVI   1(R3),X'80'         MASK - DO A 'BRANCH EQUAL' LATER             
*                                                                               
         MVC   0(1,R3),0(R5)       SAVE FILTER                                  
*                                                                               
VK50     LA    R5,1(R5)            BUMP PAST FILTER                             
         LA    R3,2(R3)            POINT TO NEXT TABLE ENTRY                    
         B     VK40                                                             
*                                                                               
VK060    MVC   DATE1,=X'0000'      SET DEFAULT DATE RANGE                       
         MVC   DATE2,=X'FFFF'                                                   
         CLI   SFRDATEH+5,0                                                     
         BE    BXIT                                                             
         GOTO1 PERVAL,DMCB,(SFRDATEH+5,SFRDATE),(X'20',WORK)                    
         LA    RF,WORK                                                          
         MVC   DATE1,PVALCSTA-PERVALD(RF)                                       
         TM    DMCB+4,PVRCONE                                                   
         BO    BXIT                                                             
         MVC   DATE2,PVALCEND-PERVALD(RF)                                       
*                                                                               
BXIT     B     EXIT                                                             
         EJECT                                                                  
*/                                                                              
*/       PROC01                                                                 
*/                                                                              
PROC01   NTR1                                                                   
*                                  GET A(FACIDTAB) FOR THIS DSPACE              
         ICM   RF,15,TWAMASTC                                                   
         L     RF,MCSSB-MASTD(RF)                                               
         CLI   SSODSPAC-SSOOFF(RF),C' '                                         
         JH    *+8                                                              
         MVI   SSODSPAC-SSOOFF(RF),C'A'     DEFAULT TO 'A'                      
*                                                                               
         USING FACIDD,R1                                                        
         L     R1,=V(FACIDTAB)                                                  
         A     R1,RELO                                                          
GFAC10   CLI   FACIDSPC,X'FF'      END OF TABLE                                 
         JE    *+2                 DEATH                                        
         CLC   FACIDSPC,SSODSPAC-SSOOFF(RF)                                     
         JE    GFAC20              FOUND THEM                                   
         AHI   R1,FACIDLNQ                                                      
         J     GFAC10                                                           
*                                                                               
GFAC20   L     R0,FACAID                                                        
         A     R0,RELO                                                          
         ST    R0,AFACIDT                                                       
         DROP  R1                                                               
*                                                                               
         LA    R4,GDIO             READ GENDIR                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEY,GDIO                      
         CLI   DMCB+8,0            IS THERE ANY RECORD?                         
         BNE   EXIT                NO                                           
PROCLP   CLC   0(10,R4),KEY        ANY MORE RECORD WITH THIS TYPE?              
         BNE   EXIT                NO                                           
*                                                                               
         LA    R4,GFIO             READ GENFIL                                  
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',GDIO+36,GFIO,DMWORK           
         CLI   DMCB+8,0                                                         
         BE    VR05                                                             
         DC    H'0'                SEVERE ERROR                                 
*                                                                               
VR05     LA    R5,GFIO+42          POINT TO FIRST ELEMENT,BRDFSTEL              
         USING BRDFLTD,R5                                                       
LOOPFIND CLI   BRDFLTC,X'00'       END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                FILTER ELEM MUST EXIST                       
         CLI   BRDFLTC,BRDFLTCQ    ELEMENT CODE X'10'?                          
         BE    VR10                YES                                          
         SR    R2,R2                                                            
         IC    R2,1(,R5)           GET ELEM LENGTH                              
         AR    R5,R2               GOTO NEXT ELEMENT                            
         B     LOOPFIND                                                         
*                                                                               
VR10     CLI   SFRNAMEH+5,0        NAME GIVEN?                                  
         BNE   LR20                YES                                          
VR15     CLI   SFRAPPLH+5,0        APPL-ID GIVEN?                               
         BNE   VR30                YES                                          
*                                                                               
VR20     OC    DATE1,DATE1         ANY DATE FILTER                              
         BZ    PRTREC                                                           
         CLC   DATE2,=X'FFFF'      IGNORE DATE 2 IF FFFF                        
         BE    *+14                                                             
         CLC   BRDFSTDT,DATE2      TEST DATE RANGE                              
         BH    RDSQ                                                             
         CLC   BRDFENDT,DATE1                                                   
         BL    RDSQ                                                             
         B     PRTREC                                                           
*                                                                               
LR20     LA    R6,BRDFNAME         POINT TO NAME IN THE RECORD                  
         LA    R3,FILTERS          FILTER EXPRESSION TABLE                      
         LA    R2,4                MAXIMUM OF FOUR FILTER POSITIONS             
*                                                                               
LR30     CLI   0(R3),0             TEST END OF TABLE                            
         BE    VR15                YES - THIS RECORD MAY BE DISPLAYED           
         ZIC   R7,1(R3)            BRANCH MASK                                  
         CLC   0(1,R3),0(R6)       TEST MATCH ON FILTER. . .                    
         EX    R7,*+8              . . . WITH THE PROPER CONDITION              
         B     *+8                                                              
         BC    0,RDSQ           FILTER FAILED - GET ANOTHER RECORD              
         LA    R3,2(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         LA    R6,1(R6)            BUMP TO NEXT FILTER CHARACTER                
         BCT   R2,LR30             UP TO FOUR FILTERS                           
         B     VR15                CHECK APPL-ID                                
*                                                                               
VR30     L     R2,AFACIDT          POINT TO APPL-ID TABLE                       
         MVC   APPLID,=C'????'                                                  
LP05     CLC   0(4,R2),SFRAPPL     MATCH ON APPL-ID                             
         BE    MOVEID              YES                                          
         LA    R2,L'FACITAB(,R2)   BUMP TO NEXT ENTRY                           
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BNE   LP05                                                             
         B     VR35                                                             
MOVEID   MVC   APPLID,4(R2)                                                     
VR35     CLC   BRDFAPPL,APPLID     DOES APPL-ID MATCH?                          
         BNE   RDSQ                                                             
         B     VR20                YES                                          
*                                                                               
RDSQ     LA    R4,GDIO                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEY,GDIO                      
         CLI   DMCB+8,0            MORE RECORDS?                                
         BNE   EXIT                NO                                           
         B     PROCLP                                                           
         EJECT                                                                  
*                                                                               
* PRINT RECORD                                                                  
*                                                                               
         USING OUTD,R7                                                          
PRTREC   LA    R7,P                                                             
         EDIT  BRDKMSGN,(5,ONUM),ZERO=NOBLANK                                   
*                                                                               
         CLI   BRDFAPPL,0          ALL?                                         
         BNE   PR00                NO, LOOK UP TABLE                            
         MVC   OAPPL(4),=C'ALL '                                                
         B     PR05                                                             
PR00     L     R2,AFACIDT          POINT TO APPL-ID TABLE                       
         MVC   OAPPL,=C'????'                                                   
LP00     CLC   4(1,R2),BRDFAPPL    MATCH APPLI-ID?                              
         BE    PR02                YES                                          
         LA    R2,L'FACITAB(,R2)   BUMP TO NEXT ENTRY                           
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BNE   LP00                                                             
         B     PR05                                                             
PR02     MVC   OAPPL,0(R2)                                                      
*                                                                               
PR05     CLI   BRDFCTRY,X'FF'      'ALL' COUNTRIES?                             
         BNE   *+14                                                             
         MVC   OCTRY(3),=C'ALL'    YES                                          
         B     PR07                                                             
*                                                                               
         LA    R6,CTRYTAB          POINT TO COUNTRY TABLE                       
         USING CTRYTABD,R6                                                      
         LH    R2,0(R6)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R6)            A(END OF TABLE)                              
         LA    R6,6(R6)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFCTRY,CTRYCODE   MATCH ON COUNTRY CODE?                       
         BE    *+10                YES                                          
         BXLE  R6,R2,*-10          NO, TRY NEXT ENTRY                           
         DC    H'0'                                                             
         MVC   OCTRY(6),CTRYSHR    SAVE COUNTRY NAME                            
         DROP  R6                                                               
*                                                                               
PR07     OC    BRDFSTDT,BRDFSTDT   ANY DATES TO PRINT?                          
         BZ    PR10                NO                                           
         LA    RE,BRDFSTDT         A(START DATE)                                
         ST    RE,DMCB                                                          
         MVI   DMCB,2              INPUT TYPE COMPRESSED                        
         CLC   BRDFSTDT,BRDFENDT   START = END DATE?                            
         BE    *+8                 YES                                          
         OI    DMCB,X'10'          SECOND DATE PRESENT AFTER FIRST DATE         
         GOTO1 DATCON,DMCB,,(8,ODATES)                                          
*                                                                               
PR10     OC    BRDFSTTM,BRDFSTTM   ANY TIME TO PRINT?                           
         BZ    PR20                NO                                           
         ZIC   RE,BRDFSTTM         START HOUR                                   
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFSTTM+1       START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL             START TIME (MILITARY)                        
         CLC   BRDFSTTM,BRDFENTM   START TM = END TM?                           
         BNE   PR15                NO                                           
         B     PR17                YES, DON'T PRINT END TIME                    
PR15     ZIC   RE,BRDFENTM         END HOUR                                     
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFENTM+1       END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL+2           END TIME (MILITARY)                          
PR17     GOTO1 UNTIME,DMCB,FULL,OTIME                                           
*                                                                               
* DISPLAY DAYS                                                                  
PR20     DS    0H                                                               
         CLI   SFRTYPE,C'P'        NO DAYS FOR PERM RECS                        
         BE    PR30                                                             
*                                                                               
         CLI   BRDFDAYS,X'00'      IF ZERO, SAME AS ALL DAYS                    
         BNE   *+8                                                              
         MVI   BRDFDAYS,B'01111111' MON-SUN                                     
         L     RF,ACOMFACS                                                      
         L     RF,CDEJAVU-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(7,BRDFDAYS),(X'00',ODAYS),0                           
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR30     CLI   BRDFSNUM,0          DO WE HAVE SPECIFIC SYSTEM NUMBER?           
         BE    DR20                NO                                           
*                                                                               
         L     R6,=V(SELIST)                                                    
         A     R6,RELO                                                          
         USING SELISTD,R6                                                       
         LH    R2,0(R6)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R6)            A(END OF TABLE)                              
         LA    R6,6(R6)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFSNUM,SESYS      MATCH ON SPECIFIC NUMBER?                    
         BE    *+10                YES                                          
         BXLE  R6,R2,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   OGENSYS,SENAME      SYSTEM NAME                                  
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS LIST)                             
         B     DR30                                                             
         DROP  R6                                                               
*                                                                               
DR20     CLI   BRDFOVSY,0          ALL SYSTEMS?                                 
         BNE   *+14                NO                                           
         MVC   OGENSYS,=C'ALL    '                                              
         B     DR30                                                             
*                                                                               
         LA    R6,SYSLST                                                        
         USING SYSLSTD,R6                                                       
         LH    R2,0(R6)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R6)            A(END OF TABLE)                              
         LA    R6,6(R6)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFOVSY,SYSLNUM    MATCH ON SYSTEM OVERLAY NUMBER?              
         BE    *+10                YES                                          
         BXLE  R6,R2,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   OGENSYS,SYSLNAME    SYSTEM NAME                                  
         DROP  R6                                                               
*                                                                               
         L     R6,=V(SELIST)                                                    
         A     R6,RELO                                                          
         USING SELISTD,R6                                                       
         LH    R2,0(R6)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R6)            A(END OF TABLE)                              
         LA    R6,6(R6)            A(FIRST ENTRY)                               
         CLC   BRDFOVSY,SEOVSYS    FIND FIRST ENTRY FOR THIS SYSTEM             
         BE    *+10                                                             
         BXLE  R6,R2,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS LIST)                             
         DROP  R6                                                               
*                                                                               
DR30     CLI   BRDFPROG,0          ALL PROGRAMS?                                
         BNE   *+14                NO                                           
         MVC   OPROGN,=C'ALL    '                                               
         B     DR40                                                             
*                                                                               
         L     R6,ASEPGMS          A(PROGRAM NAME LIST)                         
         A     R6,RELO                                                          
         USING PGMLSTD,R6                                                       
         LH    R2,0(R6)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R6)            A(END OF TABLE)                              
         LA    R6,6(R6)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFPROG,PGMNUM     MATCH ON PROGRAM NAME?                       
         BE    *+18                                                             
         BXLE  R6,R2,*-10          TRY NEXT TABLE ENTRY                         
         MVC   OPROGN(3),=C'...'   BAD PROGRAM NUMBER IN RECORD                 
         B     DR40                                                             
         MVC   OPROGN,PGMNAME                                                   
         DROP  R6                                                               
*                                                                               
DR40     MVC   ODESC(8),BRDFNAME   GET DESCRIPTION                              
*                                                                               
         MVC   OLUID(8),BRDFLUID   GET VTAM LUID                                
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,PRTTXT                                                        
         B     RDSQ                                                             
*        B     EXIT                                                             
         EJECT                                                                  
*/                                                                              
*/       PRTTXT - PRINT TEXT MESSAGE INSIDE THE BOX                             
*/                                                                              
PRTTXT   NTR1                                                                   
         LA    R5,GFIO+42                                                       
         ST    R5,FELMPTR          POINT TO FIRST ELEMENT                       
         CLI   0(R5),X'00'                                                      
         BE    TTEXIT                                                           
         MVI   MULTIPLE,0                                                       
*                                                                               
         MVI   P,STAR                                                           
         MVC   P+1(BXLN),P           DRAW TOP LINE                              
*                                                                               
         MVI   ELEMID,BRDHEDEQ     HEADING ELEMENT                              
         BAS   RE,FINDID           FIND THE A(HEADING ELEM)                     
         CLI   ELEMID,X'00'                                                     
         BE    TTNEXT                                                           
*                                                                               
         L     R5,ELEMPTR          A(HEADING ELEMENT)                           
         USING BRDHEDD,R5                                                       
         LA    R2,BXLN             MAX CHARS ON A LINE                          
         LA    R2,1(R2)            BXLN+1                                       
         ZIC   R3,BRDHEDTL         LENGTH OF HEADING                            
         SR    R2,R3                                                            
         SRL   R2,1                DIVIDE BY 2                                  
         LA    R7,P                                                             
         AR    R7,R2                                                            
*                                                                               
         OR    R2,R2                                                            
         BZ    PNEXT                                                            
         BCTR  R7,0                PUT A SPACE BEFORE THE HEADING               
         MVI   0(R7),C' '                                                       
         LA    R7,1(R7)                                                         
*                                                                               
PNEXT    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),BRDHEDTX                                                 
TTNEXT   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         USING BRDTXTD,R5                                                       
LOOPMORE MVI   ELEMID,BRDTXTEQ     TEXT ELEMENT                                 
         BAS   RE,FINDID                                                        
         CLI   ELEMID,X'00'                                                     
         BE    TTEXIT                                                           
         L     R5,ELEMPTR                                                       
         ZIC   R3,BRDTXTLN         TEXT LENGTH                                  
         LA    R2,7                OVERHEAD LENGTH                              
         SR    R3,R2               LENGTH OF TEXT LINE                          
*                                                                               
         MVI   P,STAR              ENCLOSE TEXT WITH STARS                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),BRDTXTTX                                                  
         MVI   P+BXLN,STAR                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   FELMPTR,ELEMPTR     START FROM CURRENT MULT ELEM                 
         MVI   MULTIPLE,1          MULTIPLE FIELD                               
         B     LOOPMORE                                                         
*                                                                               
TTEXIT   MVI   P,STAR                                                           
         MVC   P+1(BXLN),P           DRAW BOTTOM LINE                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*/                                                                              
*/       FINDID - FIND ADDRESS OF GIVEN ELEMENT ID                              
*/                                                                              
FINDID   NTR1                                                                   
         L     R6,FELMPTR          A(FIRST OR CURRENT ELEM)                     
         CLI   MULTIPLE,0                                                       
         BNE   MULTELEM                                                         
LPFND    CLC   0(1,R6),ELEMID      MATCHING ID?                                 
         BE    EXIT05              YES                                          
         CLI   0(R6),X'00'         END OF RECORD?                               
         BNE   SEENEXT             NO, TRY NEXT ELEMENT                         
         MVI   ELEMID,X'00'        NOT FOUND                                    
         B     EXIT05                                                           
SEENEXT  SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2               BUMP POINTER TO NEXT ELEMENT                 
         B     LPFND                                                            
MULTELEM SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2                                                            
         CLC   0(1,R6),ELEMID                                                   
         BE    EXIT05                                                           
         MVI   ELEMID,X'00'                                                     
EXIT05   ST    R6,ELEMPTR          SAVE CURRENT POINTER                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHK     NTR1                                                                   
*                                                                               
         MVC   H1+51(23),=C'BROADCAST RECORD REPORT'                            
         MVI   H2+51,HO            UNDERLINE                                    
         MVC   H2+52(22),H2+51                                                  
*                                                                               
*        PRTHEAD - PRINT COLUMN HEADINGS, 10COLS AND 108CHARS                   
*                                                                               
         MVC   H3+2(3),=C'NUM'                                                  
         MVC   H3+7(7),=C'APPL-ID'                                              
         MVC   H3+16(7),=C'COUNTRY'                                             
         MVC   H3+25(5),=C'DATES'                                               
         MVC   H3+44(4),=C'TIME'                                                
         MVC   H3+60(4),=C'DAYS'                                                
         MVC   H3+69(7),=C'SYSTEM '                                             
         MVC   H3+78(7),=C'PROGRAM'                                             
         MVC   H3+87(11),=C'DESCRIPTION'                                        
         MVC   H3+100(9),=C'VTAM LUID'                                          
         MVI   SPACING,2                                                        
*                                                                               
         B     EXIT                                                             
         SPACE 5                                                                
HEADING  SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         DC    X'00'                                                            
         SPACE 5                                                                
         GETEL R3,42,ELCODE                                                     
         SPACE 2                                                                
RELO     DS    F                                                                
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
       ++INCLUDE FALANGTAB                                                      
         EJECT                                                                  
       ++INCLUDE FACTRYTAB                                                      
         EJECT                                                                  
* BOX CHARACTER EQUATES                                                         
*                                                                               
STAR     EQU   C'*'                BORDER OF A BOX                              
BXLN     EQU   74                  LENGTH OF BOX                                
*                                                                               
UL       EQU   X'AC'               UPPER LEFT                                   
UR       EQU   X'BC'               UPPER RIGHT                                  
LL       EQU   X'AB'               LOWER LEFT                                   
LR       EQU   X'BB'               LOWER RIGHT                                  
HO       EQU   X'BF'               HORIZONTAL                                   
VE       EQU   X'FA'               VERTICAL                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENBRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FACIDTABD                                                      
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FACTRY                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD9D                                                       
         SPACE 3                                                                
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE CTSFMWORKD                                                     
* MY STORAGE DSECT                                                              
*                                                                               
         ORG   SYSSPARE                                                         
ASEPGMS  DS    A                   A(PROGRAM NAME LIST)                         
AFACIDT  DS    A                   A(FACIDTAB FOR THIS DSAPCE)                  
FELMPTR  DS    A                   POINTER TO FIRST ELEMENT IN GFIO             
ELEMPTR  DS    A                   POINTER TO CURRENT ELEMENT                   
GDIO     DS    40X                 GENDIR RECORD                                
GFIO     DS    2000X               GENFIL RECORD                                
FILTERS  DS    XL16                                                             
APPLID   DS    X                                                                
MULTIPLE DS    X                   TELLS MULTIPLE FIELD                         
ELEMID   DS    X                   CURRENT ELEMENT ID                           
DATE1    DS    H                                                                
DATE2    DS    H                                                                
*                                                                               
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
*                                                                               
OUTD     DSECT                                                                  
ONUM     DS    CL5                                                              
         DS    CL2                                                              
OAPPL    DS    CL4                                                              
         DS    CL5                                                              
OCTRY    DS    CL7                                                              
         DS    CL2                                                              
ODATES   DS    CL17                                                             
         DS    CL2                                                              
OTIME    DS    CL11                                                             
         DS    CL5                                                              
ODAYS    DS    CL7                                                              
         DS    CL2                                                              
OGENSYS  DS    CL7                                                              
         DS    CL2                                                              
OPROGN   DS    CL7                                                              
         DS    CL2                                                              
ODESC    DS    CL11                                                             
         DS    CL2                                                              
OLUID    DS    CL9                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PFLD1    DS    CL7                                                              
         DS    CL2                                                              
PFLD2    DS    CL7                                                              
         DS    CL1                                                              
PFLD3    DS    CL4                                                              
         DS    CL1                                                              
PFLD4    DS    CL3                                                              
         DS    CL5                                                              
PBOX     DS    CL80                                                             
         DS    CL3                                                              
PFLD5    DS    CL7                                                              
         DS    CL1                                                              
PFLD6    DS    CL8                                                              
         DS    CL3                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTSFM39   02/15/13'                                      
         END                                                                    
