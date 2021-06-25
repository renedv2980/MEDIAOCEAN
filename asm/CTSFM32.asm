*          DATA SET CTSFM32    AT LEVEL 007 AS OF 05/01/02                      
*PHASE TA0A32A                                                                  
*INCLUDE FATABOFF                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TA0A32 - HELP RECORD REPORT                           *         
*                                                                     *         
*  COMMENTS:    GENERATES PRINTED REPORT OF HELP RECORDS              *         
*                                                                     *         
*               NEW PROGRAMS ADDED TO FATAB REQUIRE THAT THIS PHASE   *         
*               BE RELINKED.                                          *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS:      CTSFMD2 (TA0AD2) -- REPORT SCREEN                     *         
*                                                                     *         
*  OUTPUTS:     PRINTED REPORT                                        *         
*                                                                     *         
*  LOCALS:      REGISTER USAGE                                        *         
*               R0 - WORK                                             *         
*               R1 - WORK                                             *         
*               R2 - WORK                                             *         
*               R3 - GETEL REGISTER                                   *         
*               R4 - HELP RECORD DSECT                                *         
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
         TITLE 'TA0A32 HELP RECORD REPORT'                                      
TA0A32   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A32*,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         USING HV1KEY,R4                                                        
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   USEHDHK,C'Y'                                                     
         MVI   RECFOUND,C'N'                                                    
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
PR       LA    R4,KEY              BUILD HELP KEY                               
         XC    KEY,KEY                                                          
         MVI   HV1TYPE,HV1TYPEQ    HELP RECORD TYPE                             
*                                                                               
         CLI   SFMLANGH+5,0        TEST LANGUAGE IS GIVEN                       
         BNE   PR10                YES                                          
         MVI   USEHDHK,C'N'                                                     
         MVC   P(22),=C'LANGUAGE NAME REQUIRED'                                 
         GOTO1 SPOOL,DMCB,(R8)     PRINT ERROR MESSAGE                          
         B     EXIT                                                             
*                                                                               
PR10     LA    R5,LANGTAB          A(LANGUAGE TABLE)                            
         USING LANGTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R3,SFMLANGH+5       INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
PR20     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SFMLANG(0),LANGSHR  TEST MATCH ON SHORT LANGUAGE NAME            
         BE    PR30                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SFMLANG(0),LANGSHRN TEST MATCH ON NATIVE SHORT LANG NAME         
         BE    PR30                                                             
         BXLE  R5,R6,PR20          TRY NEXT TABLE ENTRY                         
         MVI   USEHDHK,C'N'        LANGUAGE NAME NOT IN TABLE                   
         MVC   P(43),=C'LANGUAGE NAME UNKNOWN -- CALL SYSTEMS DEPT.'            
         GOTO1 SPOOL,DMCB,(R8)     PRINT ERROR MESSAGE                          
         B     EXIT                                                             
*                                                                               
PR30     MVC   HV1LANG,LANGCODE    LANGUAGE CODE                                
         DROP  R5                                                               
         XI    HV1LANG,X'FF'       USE 1'S COMPLEMENT                           
         MVC   LANGNUM,HV1LANG     SAVE LANGUAGE CODE                           
*                                                                               
         CLI   SFMSYSH+5,0         TEST SYSTEM IS GIVEN                         
         BNE   PR40                YES                                          
         MVI   USEHDHK,C'N'                                                     
         MVC   P(20),=C'SYSTEM NAME REQUIRED'                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT ERROR MESSAGE                          
         B     EXIT                                                             
*                                                                               
PR40     LA    R5,SYSLST           LIST OF SUPPORTED SYSTEMS                    
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R3,SFMSYSH+5        INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
         USING SYSLSTD,R5                                                       
PR50     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SFMSYS(0),SYSLNAME  TEST MATCH ON SYSTEM NAME                    
         BE    PR60                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SFMSYS(0),SYSLSHRT  TEST MATCH ON SHORT SYSTEM NAME              
         BE    PR60                                                             
         BXLE  R5,R6,PR50          TRY NEXT TABLE ENTRY                         
         MVI   USEHDHK,C'N'        SYSTEM NAME NOT IN TABLE                     
         MVC   P(41),=C'SYSTEM NAME UNKNOWN -- CALL SYSTEMS DEPT.'              
         GOTO1 SPOOL,DMCB,(R8)     PRINT ERROR MESSAGE                          
         B     EXIT                                                             
*                                                                               
PR60     MVC   HV1SYS,SYSLNUM      SYSTEM NUMBER                                
         DROP  R5                                                               
*                                                                               
         L     R5,=V(SELIST)       A(SYSTEM EXECUTIVE LIST)                     
         A     R5,RELO                                                          
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,HV1SYS      FIND ENTRY FOR THIS SYSTEM                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                NO SELIST ENTRY                              
*                                                                               
         MVC   ASEPGMS,SEPGMS      SAVE A(PROGRAM NAME LIST)                    
         DROP  R5                                                               
*                                                                               
         MVI   HV1PROG,0           ASSUME PROGRAM 'ALL'                         
         MVI   ALLPROGS,C'Y'                                                    
         CLI   SFMPRGH+5,0         TEST PROGRAM GIVEN                           
         BE    PR90                                                             
*                                                                               
         CLI   SFMPRGH+5,3         'ALL' IS 3 LONG                              
         BNE   *+14                                                             
         CLC   =C'ALL',SFMPRG      TEST 'ALL' PROGRAMS                          
         BE    PR90                                                             
*                                                                               
         L     R5,ASEPGMS          A(PROGRAM NAME LIST)                         
         A     R5,RELO                                                          
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R3,SFMPRGH+5        INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
PR70     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SFMPRG(0),PGMNAME   TEST MATCH ON PROGRAM NAME                   
         BE    PR80                                                             
         BXLE  R5,R6,PR70          TRY NEXT TABLE ENTRY                         
         MVI   USEHDHK,C'N'        PROGRAM NAME NOT IN TABLE                    
         MVC   P(42),=C'PROGRAM NAME UNKNOWN -- CALL SYSTEMS DEPT.'             
         GOTO1 SPOOL,DMCB,(R8)     PRINT ERROR MESSAGE                          
         B     EXIT                                                             
*                                                                               
PR80     MVC   HV1PROG,PGMNUM      SAVE PROGRAM NUMBER                          
         MVI   ALLPROGS,C'N'                                                    
         DROP  R5                                                               
*                                                                               
PR90     MVI   HV1SCRN,0           ALL SCREENS                                  
         MVI   HV1FIELD,0          ALL FIELDS                                   
         MVI   HV1PAGE,1           ALL PAGES                                    
*                                                                               
         GOTO1 HIGH                GET FIRST HELP KEY                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
PR100    CLC   KEY(23),KEYSAVE     TEST SAME SYSTEM                             
         BNE   PRX                 NO                                           
*                                                                               
         CLI   ALLPROGS,C'Y'       TEST 'ALL' PROGRAMS                          
         BE    *+14                                                             
         CLC   HV1PROG,KEYSAVE+23  IF SO, TEST KEY MATCH                        
         BNE   PRX                                                              
*                                                                               
         CLC   HV1PROG,KEYSAVE+23  TEST SAME PROGRAM                            
         BE    *+12                YES                                          
         MVI   LINE,99             EJECT PAGE ON CHANGE OF PROGRAM              
         B     *+18                WE KNOW IT'S A NEW SCREEN                    
*                                                                               
         CLC   HV1SCRN,KEYSAVE+24  TEST SAME SCREEN                             
         BE    *+8                 YES                                          
         MVI   LINE,99             EJECT PAGE ON CHANGE OF SCREEN               
*                                                                               
         CLC   HV1LANG,LANGNUM     TEST MATCH ON LANGUAGE                       
         BNE   PR160               NO                                           
*                                                                               
         GOTO1 GETREC              HELP RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         LA    R3,HV1FSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,HV1TXTEQ     TEXT ELEMENT                                 
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE SOME TEXT                            
*                                                                               
         LA    R1,5                LINES NEEDED - 4 OVERHEAD PLUS FIRST         
PR110    BAS   RE,NEXTEL           KEEP LOOKING FOR TEXT ELEMENTS               
         BNE   *+12                                                             
         LA    R1,1(R1)            INCREMENT LINES NEEDED                       
         B     PR110                                                            
         STC   R1,ALLOWLIN         FORCE EJECT IF NEEDED                        
*                                                                               
         MVI   PFLD1,0             SKIP A LINE                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ALLOWLIN,0                                                       
         MVI   PFLD1,0             SKIP A LINE                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   RECFOUND,C'N'       TEST THIS IS THE FIRST RECORD                
         BE    *+14                YES                                          
         CLC   KEY(26),KEYSAVE     TEST PRINT NEW FIELD                         
         BE    PR120               NO                                           
*                                                                               
         MVI   RECFOUND,C'Y'                                                    
         MVC   PFLD1(5),=C'FIELD'                                               
         MVC   PFLD2(3),=C'ALL'                                                 
         CLI   HV1FIELD,0          TEST ALL FIELDS                              
         BE    PR120                                                            
         EDIT  HV1FIELD,(3,PFLD2),ALIGN=LEFT                                    
*                                                                               
PR120    MVC   PFLD3(4),=C'PAGE'                                                
         EDIT  HV1PAGE,(3,PFLD4),ALIGN=LEFT                                     
*                                                                               
         MVI   PBOX,UL             UPPER LEFT OF BOX                            
         MVI   PBOX+1,HO           HORIZONTAL CHARACTER                         
         MVC   PBOX+2(77),PBOX+1   TOP LINE                                     
         MVI   PBOX+79,UR          UPPER RIGHT                                  
*                                                                               
         LA    R3,HV1FSTEL         A(FIRST ELEMENT)                             
         USING HV1HEDD,R3                                                       
         MVI   ELCODE,HV1HEDEQ     HEADING ELEMENT                              
         BAS   RE,FIRSTEL                                                       
         BNE   PR140                                                            
*                                                                               
         LA    R1,L'PBOX                                                        
         ZIC   R2,HV1HEDTL         LENGTH OF HEADING                            
         SR    R1,R2                                                            
         SRL   R1,1                DISPLACEMENT TO HEADING                      
         LA    R2,PBOX                                                          
         AR    R2,R1               A(HEADING TEXT IN PRINT LINE)                
         ZIC   R1,HV1HEDTL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),HV1HEDTX    PUT HEADING IN PRINT LINE                    
         DROP  R3                                                               
*                                                                               
         LA    R3,HV1FSTEL         A(FIRST ELEMENT)                             
         USING ACTVD,R3                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,FIRSTEL                                                       
         BNE   PR140                                                            
*                                                                               
         CLI   ACTVCHNM,0          TEST RECORD HAS BEEN UPDATED                 
         BE    PR130               NO                                           
         MVC   PFLD5,=C'CHANGED'   PRINT DATE LAST CHANGED                      
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(8,PFLD6)                               
         B     PR140                                                            
*                                                                               
PR130    MVC   PFLD5,=C'ADDED  '   PRINT DATE ADDED                             
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(8,PFLD6)                               
         DROP  R3                                                               
*                                                                               
PR140    GOTO1 SPOOL,DMCB,(R8)     PRINT TOP OF BOX                             
         LA    R3,HV1FSTEL         A(FIRST ELEMENT)                             
         USING HV1TXTD,R3                                                       
         MVI   ELCODE,HV1TXTEQ     TEXT ELEMENT                                 
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE SOME TEXT                            
*                                                                               
PR150    ZIC   R2,HV1TXTLN         LENGTH OF TEXT ELEMENT                       
         LA    R0,HV1TXTOV         OVERHEAD LENGTH                              
         SR    R2,R0               LENGTH OF TEXT                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PBOX+2(0),HV1TXTTX  PUT HELP TEXT IN PRINT LINE                  
         MVI   PBOX,VE             LEFT EDGE OF BOX                             
         MVI   PBOX+79,VE          RIGHT EDGE OF BOX                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT TEXT LINE                              
         BAS   RE,NEXTEL           KEEP LOOKING FOR TEXT ELEMENTS               
         BE    PR150                                                            
         DROP  R3                                                               
*                                                                               
         MVI   PBOX,LL             LOWER LEFT OF BOX                            
         MVI   PBOX+1,HO           HORIZONTAL CHARACTER                         
         MVC   PBOX+2(77),PBOX+1   BOTTOM LINE                                  
         MVI   PBOX+79,LR          LOWER RIGHT                                  
         GOTO1 SPOOL,DMCB,(R8)     PRINT TEXT LINE                              
*                                                                               
PR160    MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY                                                           
         GOTO1 SEQ                 NEXT HELP KEY                                
         CLI   DMCB+8,0                                                         
         BE    PR100                                                            
         DC    H'0'                                                             
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHK     NTR1                                                                   
*                                                                               
         CLI   USEHDHK,C'Y'        TEST OK TO USE HEADHOOK                      
         BNE   HDHKX                                                            
*                                                                               
         MVC   H1+56(18),=C'HELP RECORD REPORT'                                 
         MVI   H2+56,HO            UNDERLINE                                    
         MVC   H2+57(17),H2+56                                                  
*                                                                               
         MVC   H1(8),=C'LANGUAGE'                                               
         MVC   H1+9(3),SFMLANG     SHORT LANGUAGE NAME                          
*                                                                               
         MVC   H2(8),=C'SYSTEM  '                                               
         MVC   H2+9(7),SFMSYS      SYSTEM NAME                                  
*                                                                               
         MVC   H3(8),=C'PROGRAM '                                               
         CLI   HV1PROG,0           TEST ALL PROGRAMS                            
         BNE   *+14                                                             
         MVC   H3+9(3),=C'ALL'                                                  
         B     HDHK10                                                           
*                                                                               
         L     R5,ASEPGMS          A(PROGRAM NAME LIST)                         
         A     R5,RELO                                                          
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   HV1PROG,PGMNUM      TEST MATCH ON PROGRAM NUMBER                 
         BE    *+18                                                             
         BXLE  R5,R6,*-10          TRY NEXT TABLE ENTRY                         
         MVC   H3+9,=C'UNKNOWN'    PROGRAM NOT IN LIST                          
         B     HDHK10                                                           
*                                                                               
         MVC   H3+9(7),PGMNAME     PUT PROGRAM NAME IN HEADLINE                 
         DROP  R5                                                               
*                                                                               
HDHK10   MVC   H4(8),=C'SCREEN  '                                               
         CLI   HV1SCRN,0           TEST ALL SCREENS                             
         BNE   *+14                                                             
         MVC   H4+9(3),=C'ALL'                                                  
         B     HDHKX                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,HV1SCRN,H4+9,1,=C'TOG'                               
         CLC   DMCB+16(4),=F'2'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HDHKX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
HEADING  SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         DC    X'00'                                                            
         SPACE 5                                                                
       ++INCLUDE FASYSLST                                                       
         SPACE 5                                                                
         GETEL R3,42,ELCODE                                                     
         EJECT                                                                  
       ++INCLUDE FALANGTAB                                                      
         SPACE 5                                                                
* BOX CHARACTER EQUATES                                                         
*                                                                               
UL       EQU   X'AC'               UPPER LEFT                                   
UR       EQU   X'BC'               UPPER RIGHT                                  
LL       EQU   X'AB'               LOWER LEFT                                   
LR       EQU   X'BB'               LOWER RIGHT                                  
HO       EQU   X'BF'               HORIZONTAL                                   
VE       EQU   X'FA'               VERTICAL                                     
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENHV1                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD2D                                                       
         SPACE 3                                                                
       ++INCLUDE CTSFMWORKD                                                     
* MY STORAGE DSECT                                                              
*                                                                               
         ORG   SYSSPARE                                                         
ASEPGMS  DS    A                   A(PROGRAM NAME LIST)                         
USEHDHK  DS    C                   'Y' IF OK TO USE HEADHOOK                    
ALLPROGS DS    C                   'Y' IF PRINT ALL PROGRAMS IN SYSTEM          
RECFOUND DS    C                   'Y' IF A RECORD WAS FOUND                    
LANGNUM  DS    X                   LANGUAGE NUMBER (INVERTED)                   
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
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
**PAN#1  DC    CL21'007CTSFM32   05/01/02'                                      
         END                                                                    
