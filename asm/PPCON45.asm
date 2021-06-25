*          DATA SET PPCON45    AT LEVEL 057 AS OF 11/05/03                      
*PHASE T40D45A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE NUMED                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE FAGETTXT                                                               
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DISPLAY/CHANGE'               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA   8/02   FIX PUBFILE RATE LOOK-UP                                        
*               CAUSED THE PROGRAM TO DUMP IF THEY TRY TO                       
*               USE IT BY LEAVING RATE FIELD BLANK.                             
*               PRBOPEN MAY CONTAIN A PRODUCT CODE - SO                         
*               SKIP "OPEN=" DISPLAY IF IT DOES                                 
*                                                                               
* BOBY  4/02    FIX DUPLICATION OF RATES PROBLEM                                
*                                                                               
* KWAN 10/15/01 BROWSE FUNCTION FOR SPACE DESCRIPTION                           
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK AND SPACE DESCRIPTION VALIDATION           
*                                                                               
* BPLA  8/00     FIX SCREEN TITLES                                              
*                                                                               
* BPLA  5/00     NO-OP DUPLICATE RATE CHECK                                     
*                                                                               
* BPLA  5/00    INCLUDE NUMED AND XSORT AND FIX OUTDOOR                         
*               SRI= DISPLAY BUG                                                
*                                                                               
* BOBY 7/99     ADD SCROLLING TO RATE DISPLAY                                   
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - INIT'               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
T40D45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKSTRLQ,T40D45,RR=R2,CLEAR=YES                                  
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
*                                                                               
         LR    R8,RC               SAVE WORKING STORAGE POINTER                 
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
*                                                                               
         L     RC,0(R1)            ESTABLISH COMMON CODE                        
         LR    R7,RC                                                            
         AHI   R7,4096                                                          
         USING GENOLD,RC,R7                                                     
*                                                                               
         ST    R2,RELO40           SAVE RELOCATION FACTOR                       
*                                                                               
         MVI   NEWKEY,0            ASSUME THERE WAS NO KEY CHANGE               
*                                                                               
         L     R9,=A(SUBROUTS)     ESTABLISH SUBROUTINES                        
         A     R9,RELO40             RELOCATE ADDRESS                           
         USING SUBROUTS,R9                                                      
*                                                                               
*        MAKE SURE WE ARE DEALING WITH CORRECT SCREEN                           
*                                                                               
         CLI   TWASTAT,X'FB'                                                    
         BE    CON50           SCREEN IN TWA                                    
*                                                                               
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFB'                                 
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWASTAT,X'FB'                                                    
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY                             
*                                                                               
CON50    DS    0H                  LOAD LINE-UP MODULE                          
*                                                                               
         ST    RD,SAVERD           SAVE INCOMING RD                             
*                                                                               
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9000A75'                                 
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VLINUP,DMCB         V(LINUP)                                     
*                                                                               
         MVI   NFLDS,6             SIX FIELDS ON A LINE                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         ST    RF,VGETTXT          SAVE ADDRESS                                 
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - KEY'                
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY FIELDS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALKEY   DS    0H                                                               
*                                                                               
         MVI   AORCFND,0           INITIALIZE AOR CONTRACT FOUND SW             
*                                                                               
*        INDICATE RATES TYPE                                                    
*                                                                               
         FOUT  RTSDESCH            FORCE TRANSMISSION OF DESCRIPTION            
*                                                                               
         LA    RF,((L'RTSDESC-46)/2)    START OF DESCRIPTION                    
         LA    RF,RTSDESC(RF)           SO THAT IT IS CENTERED                  
*                                                                               
*                                  DEFAULT TO CURRENT LEVELS                    
*                                                                               
         MVC   0(46,RF),=C'*******  PROCESSING CURRENT LEVELS *********C        
               **'                                                              
*                                                                               
         TM    TWAKIND,1           TEST FOR LOWER LEVELS                        
         BNO   *+10                                                             
         MVC   20(15,RF),=C'LOWER LEVELS  *'                                    
*                                                                               
         TM    TWAKIND,2           TEST FOR HIGHER LEVELS                       
         BNO   *+10                                                             
         MVC   20(15,RF),=C'HIGHER LEVELS  '                                    
******** MVC   RTSDESC+20(14),=C'HIGHER LEVELS '                                
*                                                                               
         TM    TWAKIND,4           TEST FOR OPEN RATES                          
         BNO   *+10                                                             
******** MVC   RTSDESC+20(14),=C'   OPEN RATES  '                               
         MVC   20(15,RF),=C'   OPEN RATES '                                     
*                                                                               
         NI    TWAKIND,X'FF'-X'80'    SET OFF CONTRACT DISPLAYED                
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - 1CON'               
***********************************************************************         
*                                                                     *         
*        VALIDATE CONTRACT NUMBER AND FIND RECORD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKCON    DS    0H                                                               
*                                                                               
*****    MVI   NEWKEY,C'N'         INIT NEW KEY SWITCH                          
*                                                                               
         LHI   RF,SVKEY-T40DFFD                                                 
         LA    RF,T40DFFD(RF)      POINT TO LAST USED KEY                       
*                                                                               
         CLC   0(PCONNUM-PCONKEY,RF),SAVKKEY IF CHANGE IN KEY                   
         BE    *+14                                                             
         MVI   NEWKEY,C'Y'              FORCE NEW KEY                           
         MVC   0(L'SVKEY,RF),SAVKKEY    SAVE CURRENT KEY                        
*                                                                               
         LHI   RF,SAVACT-T40DFFD                                                
         LA    RF,T40DFFD(RF)      POINT TO LAST ACTION                         
*                                                                               
         CLC   =C'DIS',KBAACT      IF DISPLAY FOLLOWING CHANGE                  
         BNE   *+10                                                             
         CLC   =C'CHA',0(RF)                                                    
         BNE   *+8                                                              
         MVI   NEWKEY,C'Y'         RESTART DISPLAY                              
*                                                                               
         MVC   0(3,RF),KBAACT      SAVE CURRENT ACTION                          
*                                                                               
         CLI   KBAACT+3,C' '       ASSUME OLD RATETYPE IF MISSING               
         BNH   VKACT1                                                           
*                                                                               
         CLI   KBAACT+3,C'N'       ASSUME OLD RATETYPE IF 'CHANGE'              
         BE    VKACT1                                                           
*                                                                               
         CLC   3(1,RF),KBAACT+3    CHECK FOR CHANGE IN RATE TYPE                
         BE    VKACT1                                                           
*                                                                               
         MVC   3(1,RF),KBAACT+3       SAVE NEW RATETYPE                         
         MVI   NEWKEY,C'Y'            RESTART DISPLAY                           
*                                                                               
VKACT1   DS    0H                                                               
*                                                                               
         CLC   =C'DIS',KBAACT      IF DISPLAY                                   
         BNE   *+16                                                             
         CLI   RATLI1,0            IF NO LINE TYPE                              
         BNE   *+8                                                              
         MVI   NEWKEY,C'Y'            THEN FIRST TIME                           
*                                                                               
         LA    R2,KBANUMH          POINT TO CONTRACT NUMBER                     
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         TM    FLDIIND,FINPVAL     IF FIELD NOT PREVIOUSLY VALIDATED            
         BO    *+8                                                              
         MVI   NEWKEY,C'Y'            INDICATE SO - FORCES RE-DISPLAY           
*                                                                               
         CLC   KBAACT(3),=C'DIS'   OKAY TO CHANGE CON # IF DISPLAY              
         BE    *+12                                                             
         TM    FLDIIND,FINPVAL     IF FIELD NOT PREVIOUSLY VALIDATED            
         BNO   VKACTNVE               THEN ADD/CHA NOT VALID ACTS               
*                                                                               
         L     R1,SYSPARMS             GET A(TIOB)                              
         L     R1,0(R1)                                                         
         USING TIOBD,R1                ESTABLISH TIOBD                          
*                                                                               
         CLI   TIOBAID,0           IF ENTER HIT                                 
         BNE   VKACT2                                                           
         CLI   NEWKEY,C'Y'         AND NOT RE-STARTING DISPLAY                  
         BE    VKACT2                                                           
*                                                                               
         CLI   KBAACT+4,C'1'       AND PAGE NUMBER ENTERED                      
         BNL   VKACTPFE            SUGGEST USING PFKEYS FOR SCROLL              
*                                                                               
         CLI   KBAPAGH+5,0         PAGE FIELD MUST BE EMPTY                     
         BNE   VKACTPF1                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
VKACT2   DS    0H                                                               
*                                                                               
*        VALIDATE CONTRACT NUMBER                                               
*                                                                               
         BAS   RE,PACK             VALIDATE AS A NUMBER                         
*                                                                               
         LTR   R0,R0                                                            
         BZ    VKFLDNVE            INVALID NUMBER                               
*                                                                               
         STH   R0,HALF             SAVE CONTRACT NUMBER                         
*                                                                               
         MVC   PCONKEY(13),SAVKKEY            READ CONTRACT                     
         MVC   PCONNUM,HALF                                                     
*                                                                               
         MVC   KEY,PCONKEY                                                      
*                                                                               
         FOUT  KBAACTH             FORCE RE-DIPLAY OF ACTION                    
*                                                                               
         OI    KBAACTH+6,X'01'     FORCE RE-INPUT OF ACTION                     
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    VKCON13                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    VKCON13                                                          
         TM    SADVDATA+15,X'20'                                                
         BZ    VKCON13                                                          
*                                                                               
*        MUST SWITCH TO AOR                                                     
*        TO READ CONTRACT                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         ST    RF,VSWITCH          SAVE ADDRESS                                 
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
*                                                                               
         GOTO1 VSWITCH,DMCB                                                     
*                                                                               
         CLI   4(R1),0                                                          
         BE    VKCON13                                                          
*                                                                               
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
*                                                                               
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'FF'-X'20'  INVALIDATE MEDIA                          
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
VKCON13  CLC   KBAACT(3),=C'DIS'  SEE IF DISPLAY                                
         BNE   VKCON13I           NO                                            
*                                                                               
         BAS   RE,HIGH             READ CONTRACT RECORD                         
*                                                                               
         CLC   KEY(25),KEYSAVE     MUST FIND RECORD TO CONTINUE                 
         BE    VKCON13G                                                         
*                                                                               
VKCON13A DS    0H                  HERE IF CONTRACT NOT FOUND                   
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    VKCON13E                                                         
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    VKCON13E                                                         
         TM    SADVDATA+15,X'20'                                                
         BZ    VKCON13E                                                         
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'PRINT',0                                         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VKCON13E B     VKRCDNFE            RECORD NOT FOUND                             
*                                                                               
*                                                                               
VKCON13G DS    0H                                                               
*                                                                               
         MVC   SAVKKEY,KEY         SAVE KEY FOR NEXT TIME                       
*                                                                               
VKCON13I DS    0H                                                               
*                                                                               
         MVC   KEY+27(4),SAVKKEY+27     CONTRACT DISC ADDRESS                   
*                                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   SAVPRD,0       SEE IF LOOKING FOR A PRODUCT CONTRACT             
         BE    VKCON13J                                                         
         CLC   PCONPRD,SAVPRD     MUST MATCH                                    
         BNE   VKCON13A       NOT FOUND                                         
*                                                                               
VKCON13J DS    0H                                                               
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    VKCON13L                                                         
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    VKCON13L                                                         
         TM    SADVDATA+15,X'20'                                                
         BZ    VKCON13L                                                         
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'PRINT',0                                         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SADVDATA+16,X'01'  SEE IF ACCESS TO AOR CONTRACT ALLOWED         
         BZ    VKCON13L                                                         
*                                                                               
         MVC   KBAMSG,=CL60'** ACCESS DENIED TO AOR CONTRACT **'                
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'FF'-X'20'   UNVALIDATE MEDIA                         
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
VKCON13L DS    0H                                                               
*                                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         MVC   DMWORK2,DMWORK                                                   
*                                                                               
VKCONX   DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - VKACT'              
***********************************************************************         
*                                                                     *         
*        ANALYZE ACTION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKACT    DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'21'              LOWER RATE EL  CODE       L02          
         MVI   TYPETRAN,C'L'             LOWER RATE EL  CODE       L02          
*                                                                               
         CLC   KBAACT(4),=C'DISL'        DISPLAY RATES           L02            
         BE    DISREC                                              L02          
*                                                                               
         MVI   ELCODE,X'22'             HIGHER RATE EL  CODE       L02          
         MVI   TYPETRAN,C'H'            HIGHER RATE EL  CODE       L02          
*                                                                               
         CLC   KBAACT(4),=C'DISH'          DISPLAY RATES           L02          
         BE    DISREC                                              L02          
*                                                                               
         MVI   ELCODE,X'24'             OPEN   RATE EL  CODE       L02          
         MVI   TYPETRAN,C'O'            OPEN   RATE EL  CODE       L02          
*                                                                               
         CLC   KBAACT(4),=C'DISO'          DISPLAY RATES           L02          
         BE    DISREC                                              L02          
*                                                                               
         MVI   TYPETRAN,C'R'             NORMAL ELEMENT CODE       L02          
         MVI   ELCODE,X'20'              NORMAL ELEMENT CODE       L02          
*                                                                               
         CLC   KBAACT(4),=C'DISR'          DISPLAY RATES                        
         BE    DISREC                                                           
*                                                                   L05         
*        UPDATIVE ACTIONS                                           L05         
*                                                                   L05         
         OC    ELCODE,TWAKIND     LOW ORDER BITS ARE EITHER 01 FOR  L05         
         NI    ELCODE,X'27'       LOW / 02 FOR HIGH/04 FOR OPEN     L05         
*                                                                   L05         
         CLI   ELCODE,X'20'                                         L05         
         BE    OKOKNT20                                             L05         
*                                                                   L05         
         CLC   KBAACT(3),=C'ADD'   SEE IF THIS IS AN ADD FUNCTION   L05         
         BNE   OKOKNT20            FOR HIGHER OR LOWER LEVELS       L05         
*                                                                   L05         
*NOP*    LA    R6,PCONREC          SEE IF ELEMENT EXIST           *BOBY         
         LA    R4,PCONREC          SEE IF ELEMENT EXIST           *BOBY         
         BRAS  RE,GETEL                                             L05         
         BNE   OKOKNT20            NONE FOUND OKAY TO ADD           L05         
*                                                                   L05         
         XC    KBAMSG,KBAMSG                                        L05         
         MVC   KBAMSG(L'KKMSG),KKMSG                                L05         
         FOUT  KBAMSGH             FORCE MESSAGE RE-DISPLAY                     
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,KBAACTH          CURSOR TO ACTION                             
         OI    6(R2),OI1C                                         *BOBY         
         B     EXXMOD                                             *BOBY         
*NOP*    B     ERROR                                              *BOBY         
*                                                                   L05         
KKMSG    DC    C'RATES ALREADY ESTABLISHED - DISPLAY THEN CHANGE'               
*                                                                               
OKOKNT20 DS    0H                                                               
*                                                                               
         CLC   PCONMOD,TODAY       SKIP IF ALREADY MODIFIED TODAY               
         BE    VKACT10                                                          
*                                                                               
         SR    RE,RE               BUMP NUMBER OF CHANGES TO RATES              
         IC    RE,PCONMODN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PCONMODN                                                      
*                                                                               
         MVC   PCONMOD,TODAY       INDICATE MODIFIED TODAY                      
*                                                                               
VKACT10  DS    0H                                                               
*                                                                               
         B     VALREC                                                           
*                                                                               
         TITLE 'PPCON45 - CONTRACT RATE BASIS LINE DISP - VKEYERR'              
***********************************************************************         
*                                                                     *         
*        VALKEY ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKFLDNVE DS    0H                  INVALID FIELD                                
*                                                                               
         LA    R3,2                                                             
         B     VKERR                                                            
*                                                                               
VKFLDNEE DS    0H                  REQUIRED FIELD                               
*                                                                               
         LA    R3,2                                                             
         B     VKERR                                                            
*                                                                               
VKACTNVE DS    0H                  INVALID ACTION                               
*                                                                               
         LA    R2,KBAACTH          CURSOR TO ACTION FIELD                       
         LA    R3,12                                                            
         B     VKERR                                                            
*                                                                               
VKRCDNFE DS    0H                  RECORD NOT FOUND                             
*                                                                               
         LA    R3,53                                                            
         B     VKERR                                                            
*                                                                               
VKACTPFE DS    0H                  USE PFKEYS FOR SCROLLING                     
*                                                                               
         LA    R3,ERSCROLL                                                      
         LA    R2,KBAACTH          CURSOR TO ACTION FIELD                       
         B     ERREX                                                            
*                                                                               
VKACTPF1 DS    0H                  USE PFKEYS FOR SCROLLING                     
*                                                                               
         LA    R3,ERSCROLL                                                      
         LA    R2,KBAPAGH          CURSOR TO PAGE   FIELD                       
         B     ERREX                                                            
*                                                                               
VKERR    DS    0H                                                               
         B     ERROR                                                            
*                                                                               
ERSCROLL EQU   543                 USE PFKEYS FOR SCROLLING                     
*                                                                               
         TITLE 'PPCON45 - CONTRACT RATE BASIS LINE DISP - DISREC'               
***********************************************************************         
*                                                                     *         
*        DISPLAY RATES VIA LINUP                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISREC DS      0H                                                               
*                                                                               
         BRAS  RE,LINSET                                                        
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE KEY                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',                    X        
               KEY,KEY,(TERMNAL,0)     RESTORE FILE POINTERS                    
*                                                                               
         LA    R2,KBAACTH                                                       
         MVI   NODISP,0            RESET DISPLAY SWITCH          L02            
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         OI    FLDIIND,FINPVAL     TURN ON ALL VALIDATED BITS                   
         IC    RF,0(R2)                                                         
         LA    R2,0(RF,R2)                                                      
         CLI   0(R2),0                                                          
         BNE   *-16                                                             
*                                                                               
         MVC   KBAMSG,CNSPACES                                                  
         LA    R2,KBAACTH                                                       
*                            TURN OFF X'40' BIT - RATES DISPLAYED               
*                            AND X'80' BIT - CONTRACT DISPLAYED                 
         NI    TWAKIND,X'FF'-X'80'-X'40'                                        
*                                                                               
         CLC   KBAACT(3),=C'CHA'                                                
         BNE   DISP500                                                          
*                                                                               
         MVC   KBAMSG(14),=C'RATES CHANGED '                        L02         
         LA    RF,KBAMSG+14                                         L02         
*                                                                               
         B     DISP600                                                          
*                                                                               
DISP500  DS    0H                                                               
*                                                                               
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   DISP510                                                          
*                                                                               
         MVC   KBAMSG(12),=C'RATES ADDED '                          L02         
         LA    RF,KBAMSG+12                                         L02         
*                                                                               
         B     DISP600                                                          
*                                                                               
DISP510  DS    0H                                                               
*                                                                               
         MVC   KBAMSG(16),=C'RATES DISPLAYED '                      L02         
         LA    RF,KBAMSG+16                                         L02         
         OI    TWAKIND,X'40'        SET RATES DISPLAYED                         
*                                                                               
DISP600  DS    0H                                                               
*                                                                   L02         
         TM     TWAKIND,1           LOWER LEVEL                     L02         
         BNO    *+10                                                L02         
         MVC    0(15,RF),=C'FOR LOWER LEVEL'                        L02         
*                                                                   L02         
         TM     TWAKIND,2           HIGHER LEVEL                    L02         
         BNO    *+10                                                L02         
         MVC    0(16,RF),=C'FOR HIGHER LEVEL'                       L02         
*                                                                   L02         
         TM     TWAKIND,4           OPEN RATES                                  
         BNO    *+10                                                L02         
         MVC    0(14,RF),=C'FOR OPEN RATES'                                     
*                                                                   L02         
         CLI   RTOVFL,C'Y'                                                      
         BNE   DISP610                                                          
*                                                                   L02         
         LA    R5,KBAMSG+L'KBAMSG-1                                             
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                   L02         
         MVC   2(24,R5),=C'(USE ''DISRN'' MORE RATES)'                          
*                                                                   L02         
         MVC   11(1,R5),TYPETRAN           TYPE TRANS ACTION     L02            
*                                                                   L02         
         ZIC   RF,SAVPAG                                                        
         LA    RF,1(RF)                                                         
         STC   RF,12(R5)                                                        
         OI    12(R5),X'F0'                                                     
*                                                                               
DISP610  DS    0H                                                               
*                                                                               
         CLC   KBAACT(3),=C'DIS'   SKIP IF ONLY DISPLAYING                      
         BE    DISP630                                                          
*                                  AFTER CHANGE SORT RATE ELEMS ON DATE         
         LA    R2,PCONREC+33                                                    
         SR    R4,R4                                                            
         XC    FULL,FULL                                                        
*                                                                               
DISP612  DS    0H                                                               
*                                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    DISP616                                                          
         B     DISP618                                                          
*                                                                               
DISP614  DS    0H                                                               
*                                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DISP612                                                          
*                                                                               
DISP616  DS    0H                                                               
*                                                                               
         OC    FULL,FULL                                                        
         BNZ   *+8                                                              
         ST    R2,FULL             A(FIRST RATE ELEM)                           
*                                                                               
         LA    R4,1(R4)            BUMP ELEM COUNT                              
         B     DISP614                                                          
*                                                                               
DISP618  DS    0H                                                               
*                                                                               
         CLI   0(R2),0                                                          
         BE    DISP620                                                          
*                                                                               
         LTR   R4,R4               TEST HAVE REACHED FIRST RATE ELEM            
         BZ    DISP614                                                          
*                                                                               
DISP620  DS    0H                                                               
*                                                                               
         LTR   R4,R4                                                            
         BNP   DISP625                                                          
*                                                                               
         MVC   DMCB(4),FULL                                                     
*                                                                               
         GOTO1 =V(XSORT),DMCB,,(R4),42,5,0,RR=RELO40                            
*                                                                               
DISP625  DS    0H                                                               
*                                                                               
         MVC   DMWORK(96),DMWORK2                                               
         MVC   KEY+27(4),SAVKKEY+27                                             
*                                                                               
         LA    RF,PCONREC          READ INTO CONREC                             
         ST    RF,AREC                                                          
*                                                                               
         BAS   RE,PUTREC                                                        
*                                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
DISP630  DS    0H                                                               
*                                                                               
         LA    R2,KBAACTH                                                       
******   FOUT  KBAACTH,CNSPACES,8                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATE BASIS - VALREC '               
***********************************************************************         
*                                                                     *         
*        VALIDATE RATES VIA LINUP                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALREC   DS    0H                                                               
*                                                                               
         BRAS  RE,VR                                                            
*                                                                               
VALRECX  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
NLINS    EQU   ((RTSLILH-RTSLI1H)/(RTSLI2H-RTSLI1H))+1                          
*                                                                               
       ++INCLUDE PPGENEROL         IN-LINE CODES                                
*                                                                               
         LTORG                                                                  
SPACES   DC    CL70' '                                                          
PATCH    DS    CL30                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPCONWRK                                                       
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - VALREC'             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE RECORD                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40D45   CSECT                                                                  
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE DETAIL INPUT VIA LINUP                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         L     RE,=A(LSVTAB-T40DFFD)                                            
         LA    RE,T40DFFD(RE)                                                   
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE COPY OF LINUP SAVE TABLE                
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         MVC   ORIGKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         XC    ALINCUR,ALINCUR     INIT CURSOR POSITION                         
*                                                                               
         GOTO1 =A(LINSET),RR=RELO40  LINUP INTERFACE                            
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE KEY                                  
*                                  RESTORE FILE POINTERS                        
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY,0,0                  
*                                                                               
*        SET CURSOR TO FIELD ASKED FOR                                          
*                                                                               
         OC    ALINCUR,ALINCUR     IF NO FIELD TO GET CURSOR                    
         BNZ   *+12                                                             
         LA    R2,KBAACTH             DEFAULT TO ACTION FIELD                   
         B     VR10                                                             
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,ALINCUR        ELSE GET DISPLACEMENT INTO TWA               
         AR    R2,RA                  RE-LOCATE ADDRESS IN TWA                  
*                                                                               
VR10     DS    0H                                                               
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         USING TIOBD,R1            ESTABLISH TIOBD                              
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
*                                                                               
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
*                                                                               
         XC    TIOBCURI,TIOBCURI   CLEAR DISPLACEMENT INTO FIELD                
*                                                                               
VRCURSX  DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - LINSET'             
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         ST    RA,LUATWA           PASS A(TWA)                                  
*                                                                               
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVC   LUNFLDS,NFLDS           FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,RTSLI1H          A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON LINE                 
         SR    RF,RA               GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
*                                                                               
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF A HALFPAGE IS DFLT          
*                                                                               
         CLI   RTSSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   RTSSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   RTSSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   RTSSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    RTSSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RTSSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,RTSSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
*                                     ANALYZE PFKEYS                            
         L     R3,LUATIOB          ESTABLISH                                    
         USING TIOBD,R3                                                         
*                                                                               
         CLI   TIOBAID,19          CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   TIOBAID,7           CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   TIOBAID,20          CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   TIOBAID,8           CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
         MVI   LUAPMODE,LUAPDSPQ   DEFAULT TO DISPLAY MODE                      
*                                                                               
         CLC   KBAACT(3),=C'CHA'   CHANGE FOR VALREC MODE                       
         BE    *+10                                                             
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=Y(LSVTABL) SAVED BYTES PER LINE                         
*                                                                               
         LHI   RE,LSVTAB-T40DFFD   LINUP SAVE AREA                              
         LA    RE,T40DFFD(RE)                                                   
         ST    RE,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BRAS  RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ELEMENTS TO DISPLAY                    
         BNZ   LSNORX                                                           
*                                                                               
         CLC   KBAACT(3),=C'ADD'   AND ACTION IS ADD                            
         BNE   LSNORX                                                           
*                                                                               
         CLI   RTSLI1H+5,0         THEN DATA MUST BE KEYED IN                   
         BNE   LSNORX                                                           
*                                                                               
         LA    R3,1                MISSING DATA ERROR                           
         LA    R2,RTSLI1H                                                       
         B     ERREX                                                            
*                                  NO RATE DATA GIVEN                           
LSNORX   DS    0H                                                               
*                                                                               
         CLC   KBAACT(3),=C'CHA'   IF VALIDATING RECORD                         
         BE    *+10                                                             
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   *+8                                                              
         BNE   LSPFKX                                                           
*                                     ANALYZE PFKEYS                            
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLI   TIOBAID,0           IF PFKEY ENTERED                             
         BE    LSPFKX                                                           
*                                                                               
         BRAS  RE,PFKEYS              GO ANALYZE                                
*                                                                               
         DROP  R3                                                               
*                                                                               
LSPFKX   DS    0H                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LHI   RE,LSVTAB-T40DFFD   LINUP SAVE AREA                              
         LA    RE,T40DFFD(RE)                                                   
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
****     OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         BRAS  RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
*                                                                               
         CLC   =C'CHA',KBAACT      AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
******   MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
         MVC   KBAMSG,=CL60'CONTRACT RATES DISPLAYED. ENTER CHANGES'            
*                                                                               
         B     LSMOR                                                            
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
         CLC   =C'CHA',KBAACT      IF CHANGING                                  
         BNE   *+10                                                             
         MVC   KBAMSG,=CL60'CONTRACT RATES CHANGED'                             
*                                                                               
         CLC   =C'ADD',KBAACT      IF ADDING                                    
         BNE   *+10                                                             
         MVC   KBAMSG,=CL60'CONTRACT RATES ADDED'                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,CNSPACES        INIT WORK OUTPUT AREA                        
         LA    R1,RTSMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,CNSPACES        INIT WORK OUTPUT AREA                        
         LA    R1,RTSMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MHI   RF,LSVTABL          GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
*                                  LINES IN WINDOW                              
         DROP  R3                                                               
         DROP  R4                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - LINHOOK'            
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1  LABEL=*                                                          
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - LINVAL'             
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
***      OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BRAS  RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
*                                                                               
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO40                   
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
*                                                                               
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
*                                                                               
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'PPCON45 - PRINTPAK CONTRACT RATES DIS/CHA - LINDIS'             
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
*                                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
*                                                                               
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BRAS  RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BRAS  RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - BUILD TABLE OF ELEMENTS ON FILE -LSBLDTAB'            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         XC    WORK2,WORK2         CLEAR RTLOOK OUTPUT AREA                     
         XC    TEMPCON,TEMPCON                                                  
*                                                                               
         MVC   ELSAVE,ELCODE     ELCODE MAY CHANGE IF NO RATES FOR              
*                           HIGH OR LOWER RATES --WILL REVERT TO 20             
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         LHI   R1,ELTAB-WRKSTRD    POINT TO ELEMENT TABLE                       
         AR    R1,R8                                                            
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
*                                                                               
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
*                                                                               
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
*                                                                               
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
*                                                                               
         LA    R1,MAXRTS           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
*        BUILD TABLE OF ELEMENTS                                                
*                                                                               
         SR    R0,R0               INIT ELEMENT COUNTER                         
*                                                                               
LSBTYPLP DS    0H                                                               
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         LA    R5,PCONELEM         POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         USING PRBELEM,R5          ESTABLISH AS CONTRACT RATE ELEMENT           
*                                                                               
         CLI   PRBELEM,0           DONE IF END OF RECORD REACHED                
         BE    LSBTDONE                                                         
*                                                                               
         CLC   PRBELEM,ELCODE      LOOKING FOR APPROPRIATE RATE ELEMS           
         BNE   LSBTCONT                                                         
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PRBELEM+1      ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTELEM(0),PRBELEM  SAVE RATE ELEMENT                            
*                                                                               
         AHI   R0,1                BUMP ELEMENT COUNTER                         
         STC   R0,ELTSORT          SET ELEMENT COUNTER AS SORT KEY              
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO40                
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FITS INTO TABLE              
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PRBELEM+1        ELEMENT LENGTH                               
         LA    R5,PRBELEM(RF)      POINT TO NEXT ELEMENT                        
*                                                                               
         B     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
LSBTYPCN DS    0H                                                               
*                                                                               
         OC    BSPNOR,BSPNOR       DONE IF SOME RATES FOUND                     
         BNZ   LSBTYPDN                                                         
*                                                                               
         CLI   ELCODE,X'20'        DONE IF SEARCHING FOR CURRENT RATES          
         BE    LSBTYPDN                                                         
*                                                                               
         MVI   ELCODE,X'20'        FORCE TO DISPLAY 20 DESCRIPTIONS             
         MVI   NODISP,255          INDICATE TO SHOW ONLY SPACE DESCS            
*                                                                               
         B     LSBTYPLP                                                         
*                                                                               
LSBTYPDN DS    0H                                                               
*                                                                               
         MVC   ELCODE,ELSAVE       RESTORE RATE IDENTIFIER                      
*                                                                               
         OC    BSPNOR,BSPNOR       DONE IF NO RATES FOUND                       
         BZ    LSBT200                                                          
*                                                                               
         CLI   KBAMED,C'N'         IF DOING NEWSPAPERS?                         
         BNE   LSBT200                                                          
*                                                                               
         L     R4,BSPATAB             POINT TO FIRST ENTRY IN TABLE             
         L     R6,BSPNOR              NUMBER OF ENTRIES                         
*                                                                               
LSBT100  DS    0H                                                               
         LA    R5,ELTELEM             POINT TO RATE ELEMENT IN ENTRY            
         USING PRBELEM,R5             ESTABLISH RATE ELEMENT                    
*                                                                               
         TM    PRBIND,X'80'           SKIP IF THERE IS AN OVERRIDE?             
         BO    LSBT200                                                          
         CP    PRBRATE,=P'0'           OR A RATE                                
         BNE   LSBT200                                                          
*                                                                               
*                                  ELSE LOOK UP RATE BASIS                      
*                                                                               
         L     RF,APUBIO           POINT TO PUB RECORD AREA                     
*                                                                               
         CLI   0(RF),0             TEST PUBREC IN CORE                          
         BNE   LSBT150                YES                                       
*                                                                               
         MVC   KEY+27(4),SAVPUBA   PUBREC DISK ADDR                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',         X        
               KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
LSBT150  DS    0H                                                               
*                                                                               
         CLI   NODISP,X'FF'        SKIP IF ONLY DISPLAYING SPACE DESCS          
         BE    LSBT200                FOR HIGHER AND LOWER RATES                
*                                                                               
*        GET RATE BASIS ELEMENTS FROM PUB RECORD                                
*                                                                               
*       CREATE DUMMY CONTRACT FOR PUB RATE LOOK-UP                              
*       WITH ONLY THE ELEMENT FROM THE TABLE                                    
*                                                                               
         XC    TEMPCON,TEMPCON                                                  
         MVC   TEMPCON(70),PCONREC   KEY PLUS PCONDESC                          
         MVC   TEMPCON+70(L'ELTELEM),ELTELEM   ELEMENT FROM TABLE               
*                                                                               
         GOTO1 VRTLOOK,DMCB,0,(ELCODE,APUBIO),TEMPCON,WORK2                     
*                                                                               
         LA    R2,RTSRT1H                                                       
*                                                                               
         CLI   DMCB,0              ERROR?                                       
         BE    LSBT160                                                          
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         B     ERREX                                                            
*                                                                               
LSBT160  CLI   WORK2,0      NO ELEMENTS FOUND                                   
         BNE   *+6                                                              
         DC    H'0'         SOMETHING VERY WRONG                                
*                                                                               
         MVC   ELTELEM,WORK2       SAVE IN ELEMENT TABLE                        
         LA    R4,ELTABL(R4)       POINT TO NEXT ENTRY                          
         BCT   R6,LSBT100                                                       
*                                                                               
LSBT200  DS    0H                                                               
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+6                                                              
         BCTR  R1,0                MINUS ONE                                    
         MHI   R1,ELTABL           TIMES ENTRY LENGTH                           
*                                                                               
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
TEMPCON  DS   CL200        DUMMY CONTRACT FOR RATELOOK                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHSRCH'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO SEARCH TABLE FOR ELEMENT                          *         
*        AND SET ADDRESS IN ELTENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BNZ   *+12                                                             
         OI    LUSTAT,LUEOLQ          SET OFF ON OF LIST INDICATOR              
         B     LHSRCHX                RETURN EMPTY-HANDED                       
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         BNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   HALF,HIVALS         IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
******   BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',HALF),RR=RELO40                   
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
*                                                                               
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18               DONT GO FURTHER                           
         SH    R4,=Y(ELTABL)          BACK UP AN ENTRY                          
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ          TELL LINUP THIS IS LAST                   
LHSRCH30 DS    0H                                                               
         EJECT                                                                  
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
         MVC   LSVKEYNW,ELTKEYNW   SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVALLIN'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE WINDOW LINE                              *         
*        BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PRBELEM,R3          ESTABLISH RATE ELEMENT                       
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVIND'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE LEVEL INDICATOR                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVIND   DS    0H                                                               
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE LVL IND         
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         CLI   FLDILEN,0           IF NO LEVEL IND ENTERED                      
         BNE   LHVIND10                                                         
*                                                                               
         TM    LULSTAT,LUSDATQ     AND DATA IN SOME FIELDS ON THIS LINE         
         BO    LHVIND1E               ERROR                                     
*                                                                               
*        NO DATA IN ANY FIELD ON LINE                                           
*        MARK ALL FIELDS ON LINE VALIDATED                                      
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE LVL IND         
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS LEFT ON LINE                
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BCT   R0,*-8                                                           
*                                                                               
         B     LHVLX               GO TO NEXT LINE                              
*                                                                               
LHVIND10 DS    0H                                                               
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
*        MUST RE-ESTABLISH ELCODE                                               
*                                                                               
         MVI   ELCODE,X'20'       DEFAULT                                       
         OC    ELCODE,TWAKIND     LOW ORDER 2 BITS ARE EITHER                   
         NI    ELCODE,X'27'       01 FOR LOW  02 FOR HIGH OR 0                  
*                                  04 FOR OPEN                                  
*                                                                               
*              EDIT RATE BASIS LINE                                             
*                                                                               
         MVC   PRBELEM(1),ELCODE   SET ELEMENT CODE                             
         MVI   PRBELEM+1,X'2A'     SET ELEMENT LENGTH                           
         ZAP   PRBOPEN,=P'0'       INIT OPEN RATE                               
*                                                                               
*                                                                               
*        VALIDATE LEVEL INDICATOR                                               
*                                                                               
         CLI   FLDDATA,C'L'                                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'$'                                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'P'                                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'S'        SPECIAL?                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'X'                                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'I'                                                     
         BE    LHVINDOK                                                         
         CLI   FLDDATA,C'U'                                                     
         BE    LHVINDOK                                                         
*                                                                               
         B     LHVIND2E            INVALID LEVEL INDICATOR                      
*                                                                               
LHVINDOK DS    0H                                                               
*                                                                               
         MVC   PRBLIND,FLDDATA     SAVE LVL IND                                 
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVINDX                                                          
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
LHVIND1E DS    0H                  LEVEL INDICATOR REQUIRED                     
         LHI   R3,ERINDREQ                                                      
         B     LHVINDER                                                         
*                                                                               
LHVIND2E DS    0H                  LEVEL INDICATOR INVALID                      
         LHI   R3,ERINDNV                                                       
         B     LHVINDER                                                         
*                                                                               
LHVINDER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERINDREQ EQU   509                 LEVEL INDICATOR REUIRED                      
ERINDNV  EQU   510                 LEVEL INDICATOR NOT VALID                    
*                                                                               
LHVINDX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVLVL'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE CONTRACT LEVEL                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVLVL   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO LEVEL FIELD                          
*                                                                               
         ST    R2,LEVADDR          SAVE ADDRESS OF LEVEL FIELD                  
*                                                                               
         SR    R1,R1               INIT LEVEL                                   
*                                                                               
         CLI   FLDILEN,0           OKAY IF NO LEVEL ENTERED                     
         BE    LHVLVLOK                                                         
*                                                                               
*        HANDLE SPECIAL LEVELS                                                  
*                                                                               
         CLC   =C'FLAT',FLDDATA                                                 
         BNE   *+12                                                             
         OI    PRBIND,X'01'        FLAT RATE                                    
         B     LHVLVLOK                                                         
*                                                                               
         CLC   =C'OPEN',FLDDATA    OPEN RATE OKAY                               
         BE    LHVLVLOK                                                         
*                                                                               
         CLC   =C'ADV',FLDDATA     IF ADVERTISER RATES                          
         BNE   LHVLVL70                                                         
*                                                                               
         CLC   SADVDATA(2),AGYALPHA         DISALLOW IF I AM THE AOR            
         BE    LHVLVL1E                                                         
*                                                                               
         TM    SADVDATA+15,X'04'            CHK LEVEL LOOK-UP ALLOWED           
         BZ    LHVLVL2E                                                         
*                                                                               
         ZAP   PRBLEVEL,=P'-2'              TRIGGER ADV LEVEL LOOK-UP           
*                                                                               
         B     LHVLVLO1                                                         
*                                                                               
LHVLVL70 DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,FLDILEN          INPUT LENGTH                                 
*                                                                               
         LA    R6,FLDDATA          POINT TO START OF LEVEL                      
*                                                                               
         CLI   FLDDATA,C'N'      ALLOW "N" BEFORE LEVEL IF PRBLIND IS $         
         BNE   LHVLVL72                                                         
*                                                                               
         CLI   PRBLIND,C'$'        LEVEL INDICATOR MUST BE $                    
         BNE   LHVLVL3E                                                         
*                                                                               
         MVI   PRBLIND,C'N'           SWITCH $ TO "N"                           
*                                                                               
         AHI   R0,-1                  DECREMENT LENGTH COUNTER                  
         BNP   LHVLVL4E                                                         
*                                                                               
         LA    R6,1(R6)               POINT TO ACTUAL DOLLAR LEVEL              
*                                                                               
LHVLVL72 DS    0H                                                               
*                                                                               
         GOTO1 VCASHVAL,DMCB,(C'N',0(R6)),(R0)  VALIDATE NUMBER                 
*                                                                               
         CLI   DMCB,0              VALID?                                       
         BNE   LHVLVL5E                                                         
*                                                                               
         ICM   R1,15,DMCB+4        GET LEVEL                                    
         BM    LHVLVL6E                                                         
*                                                                               
LHVLVLOK DS    0H                                                               
*                                                                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         MVC   PRBLEVEL,DUB+3      LEVEL TO RATE BASIS ELEMENT                  
*                                                                               
LHVLVLO1 DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVLVLX                                                          
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
LHVLVL1E DS    0H                                                               
         LHI   R3,ERAORAOR         AOR CAN'T LOOK UP RATES                      
         B     LHVLVLER                                                         
*                                                                               
LHVLVL2E DS    0H                                                               
         LHI   R3,ERAORADV         NOT-AUTHORIZED TO LOOKUP AOR RATES           
         B     LHVLVLER                                                         
*                                                                               
LHVLVL3E DS    0H                                                               
         LHI   R3,ERLVLNET         NET AMOUNT VALID ONLY FOR $ VOLUME           
         B     LHVLVLER                                                         
*                                                                               
LHVLVL4E DS    0H                                                               
         LHI   R3,ERNETNE          NET AMOUNT NOT ENTERED                       
         B     LHVLVLER                                                         
*                                                                               
LHVLVL5E DS    0H                                                               
         LHI   R3,ERLVLNV          AOR CAN'T LOOK UP RATES                      
         B     LHVLVLER                                                         
*                                                                               
LHVLVL6E DS    0H                                                               
         LHI   R3,ERLVLNE          LEVEL NOT ENTERED                            
         B     LHVLVLER                                                         
*                                                                               
LHVLVLER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERAORAOR EQU   511                 AOR CAN'T LOOK UP ITS OWN RATES              
ERAORADV EQU   512                 NOT AUTHORIZED TO DISPLAY RATES              
ERLVLNET EQU   513                 NET AMOUNT VALID ONLY FOR $ VOLUME           
ERNETNE  EQU   514                 NET AMOUNT NOT ENTERED                       
ERLVLNV  EQU   515                 LEVEL NOT NUMERIC                            
ERLVLNE  EQU   516                 LEVEL NOT ENTERED                            
*                                                                               
LHVLVLX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVPCT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE CONTRACT DISCOUNT PER CENT               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVPCT   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO PERCENT FIELD                        
*                                                                               
         ZAP   PRBPCT,=P'0'        INIT PERCENT                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,FLDILEN        INPUT LENGTH                                 
         BZ    LHVPCTOK            OKAY IF NO INPUT                             
*                                                                               
         GOTO1 VCASHVAL,DMCB,FLDDATA,(R0)   VALIDATE PERCENT                    
*                                                                               
         CLI   DMCB,0                                                           
         BNE   LHVPCT1E            PERCENT NOT NUMERIC                          
*                                                                               
         ICM   RF,15,DMCB+4        GET ENTERED PER CENT                         
         BM    LHVPCT2E            MUST BE NON-NEGATIVE                         
*                                                                               
         CHI   RF,9999             MAX 99.99%                                   
         BH    LHVPCT3E                                                         
*                                                                               
         CVD   RF,DUB                                                           
         ZAP   PRBPCT,DUB          SET ENTERED PER CENT                         
*                                                                               
LHVPCTOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVPCTX                                                          
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
LHVPCT1E DS    0H                                                               
         LHI   R3,ERPCTNNM         PERCENT NOT NUMERIC                          
         B     LHVPCTER                                                         
*                                                                               
LHVPCT2E DS    0H                                                               
         LHI   R3,ERPCTNEG         PER CENT MUST BE NON-NEGATIVE                
         B     LHVPCTER                                                         
*                                                                               
LHVPCT3E DS    0H                                                               
         LHI   R3,ERPCT100         PER CENT MAX IS 99.99                        
         B     LHVPCTER                                                         
*                                                                               
LHVPCTER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERPCTNNM EQU   517                 PER CENT NOT NUMERIC                         
ERPCTNEG EQU   518                 PER CENT MUST BE NON-NEGATIVE                
ERPCT100 EQU   519                 PER CENT MAX IS 100%                         
*                                                                               
LHVPCTX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVRAT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE CONTRACT RATE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVRAT   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO RATE FIELD                           
*                                                                               
         ST    R2,RATADDR          SAVE ADDRESS OF RATE FIELD                   
*                                                                               
         ZAP   PRBRATE,=P'0'       INIT RATE FIELD                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,FLDILEN        INPUT LENGTH                                 
         BZ    LHVRATOK            NO INPUT OKAY                                
*                                                                               
         SR    R1,R1               INIT WORK FIELD                              
*                                                                               
         CLC   =C'ADV',FLDDATA     IF AOR RATE LOOK UP                          
         BNE   LHVRAT10                                                         
*                                                                               
         CLC   SADVDATA(2),AGYALPHA   ERROR IF I AM THE AOR                     
         BE    LHVRAT1E                                                         
*                                                                               
         TM    SADVDATA+15,X'02'      ADV RATE LOOK-UP MUST BE ALLOWED          
         BZ    LHVRAT2E                                                         
*                                                                               
         ZAP   PRBRATE,=P'-2'         TRIGGER RATE LOOK-UP                      
*                                                                               
         B     LHVRATOK                                                         
*                                                                               
LHVRAT10 DS    0H                                                               
*                                                                               
         OI    PRBIND,X'80'     DEFAULT TO MANUAL RATE INPUT                    
*                                                                               
         CLI   FLDDATA,C'N'     NET RATE                                        
         BE    *+8                                                              
         CLI   FLDDATA+1,C'N'                                                   
         BNE   *+8                                                              
         OI    PRBIND,X'10'         NET RATE INDICATIOR                         
*                                                                               
         CLI   FLDDATA,C'S'     S RATE                                          
         BE    *+8                                                              
         CLI   FLDDATA+1,C'S'                                                   
         BNE   *+8                                                              
         OI    PRBIND,X'02'        S RATE INDICATIOR                            
*                                                                               
         CLI   FLDDATA,C'C'     COMMISSION ONLY                                 
         BE    *+8                                                              
         CLI   FLDDATA+1,C'C'                                                   
         BNE   *+8                                                              
         OI    PRBIND,X'04'        COMMISSION ONLY                              
*                                                                               
         CLI   FLDDATA,C'T'     TOTAL RATE                                      
         BE    *+8                                                              
         CLI   FLDDATA+1,C'T'                                                   
         BNE   *+8                                                              
         OI    PRBIND,X'40'        TOTAL RATE                                   
*                                                                               
         CLI   FLDDATA,C'U'     UNIT RATE                                       
         BE    *+8                                                              
         CLI   FLDDATA+1,C'U'       UNIT RATE                                   
         BNE   *+8                                                              
         OI    PRBIND,X'20'                                                     
*                                                                               
         TM    PRBIND,X'60'      CANNOT HAVE T AND U                            
         BO    LHVRAT3E                                                         
         TM    PRBIND,X'12'      CANNOT HAVE S AND N                            
         BO    LHVRAT3E                                                         
         TM    PRBIND,X'06'      CANNOT HAVE S AND C                            
         BO    LHVRAT3E                                                         
         TM    PRBIND,X'14'      CANNOT HAVE N AND C                            
         BO    LHVRAT3E                                                         
*                                                                               
         TM    PRBIND,X'40'      TOTAL RATES VALIDATED DIFFERENTLY              
         BO    LHVRAT30                                                         
*                                                                               
         TM    PRBIND,X'20'      UNIT RATES VALIDATED AS LINE RATE              
         BO    LHVRAT28                                                         
*                                                                               
         CLI   KBAMED,C'N'       IF NON-NEWSPAPER                               
         BNE   LHVRAT30               VALLIDATE AS TOTAL RATE                   
*                                                                               
*        NEWSPAPERS CAN HAVE RATE CODE IN NEXT FIELD                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         LA    RF,0(RF,R2)         POINT TO NEXT FIELD (SPACE)                  
*                                                                               
         CLC   8(2,RF),=C'R='      IF RATECODE IN SPACE FLD                     
         BE    LHVRAT28               VALIDATE AS LINE RATE                     
*                                                                               
         CLI   5(RF),0             IF NO SPACE ENTERED                          
         BE    LHVRAT28               MEANS A LINE RATE                         
*                                                                               
         B     LHVRAT30            ELSE VALIDATE AS TOTAL RATE                  
*                                                                               
*              LINE RATE - 5 DECIMALS                                           
*                                                                               
LHVRAT28 DS    0H                                                               
*                                                                               
         LA    R6,FLDDATA          START OF RATE                                
*                                                                               
         CLI   0(R6),C'.'          OKAY IF RATE STARTS WITH DECIMAL PT          
         BE    LHVRAT29                                                         
*                                                                               
         CLI   0(R6),C'0'          RATE MUST START WITH A NUMERIC               
         BNL   LHVRAT29                                                         
*                                                                               
         BCTR  R0,0                NO - ADJUST TO BYPASS 1ST POSITION           
         LA    R6,1(R6)                                                         
*                                                                               
         CLI   0(R6),C'.'          OKAY IF DECIMAL POINT OR NUMBER              
         BE    LHVRAT29                                                         
*                                                                               
         CLI   0(R6),C'0'                                                       
         BNL   LHVRAT29                                                         
*                                                                               
         BCTR  R0,0                CAN BYPASS UP TO 2 POSITIONS                 
         LA    R6,1(R6)                                                         
*                                                                               
LHVRAT29 DS    0H                                                               
*                                                                               
         OI    PRBIND,X'20'        SET AS UNIT RATE                             
*                              MOVE REMAINING INPUT TO MYTEMP                   
         XC    MYTEMP,MYTEMP       INIT WORKAREA                                
*                                                                               
         LTR   RF,R0               COPY LENGTH                                  
         BNP   LHVRAT7E                                                         
*                                                                               
         BCTR  RF,0            DECREMENT FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYTEMP(0),0(R6)    MOVE RATE TO WORKAREA                         
*                                                                               
         SR    R1,R1           USED FOR INPUT LENGTH FOR CASHVAL                
*                                                                               
         LA    RF,MYTEMP           START OF UNIT RATE                           
*                                                                               
*        LOOK FOR A TRAILING '/'                                                
*                                                                               
LHVRATLP DS    0H                                                               
*                                                                               
         CLI   0(RF),0             DONE AT END OF FIELD                         
         BE    LHVRATDN                                                         
         CLI   0(RF),C'/'          CHECK FOR '/'                                
         BE    LHVRATFD                                                         
*                                                                               
LHVRATCN DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP LENGTH COUNTER                          
         LA    RF,1(RF)            BUMP POINTER                                 
         B     LHVRATLP                                                         
*                                                                               
LHVRATFD DS    0H                                                               
*                                                                               
         CLI   1(RF),C'L'      IF LINE RATE                                     
         BNE   LHVRATF3                                                         
*                                                                               
         CLI   PRBLIND,C'I'       THE LEVEL INDICATOR CAN'T BE INCH             
         BE    LHVRAT4E                                                         
*                                                                               
         B     LHVRATF4                                                         
*                                                                               
LHVRATF3 DS    0H                                                               
*                                                                               
         CLI   1(RF),C'I'                                                       
         BNE   LHVRAT5E                                                         
*                                                                               
         CLI   PRBLIND,C'L'    DISALLOW INCH RATE FOR LINE LEVEL IND            
         BE    LHVRAT6E                                                         
*                                                                               
         OI    PRBIND,X'08'     INCH RATE INPUT                                 
*                                                                               
LHVRATF4 DS    0H                                                               
*                                                                               
LHVRATDN DS    0H                                                               
*                                                                               
         LR    R0,R1              COPY RATE LENGTH                              
         MVC   0(2,RF),=C'    '   JUST IN CASE                                  
*                                                                               
         GOTO1 VCASHVAL,DMCB,(5,MYTEMP),(R0)  VALIDATE RATE AS NUMBER           
*                                                                               
         B     LHVRAT40                                                         
*                                                                               
*              NON-LINE RATE - 2 DECIMALS                                       
*                                                                               
LHVRAT30 DS    0H                                                               
*                                                                               
         LA    R6,FLDDATA                                                       
*                                                                               
         CLI   0(R6),C'.'          DECIMAL POINT                                
         BE    LHVRAT31                                                         
         CLI   0(R6),C'0'    IS IT NUMERIC                          L01         
         BNL   LHVRAT31                                             L01         
*                                                                               
         BCTR  R0,0                NO - SHORTEN LENGTH                          
         LA    R6,1(R6)                                                         
*                                                                               
         CLI   0(R6),C'.'                                                       
         BE    LHVRAT31                                                         
         CLI   0(R6),C'0'                                           L01         
         BNL   LHVRAT31                                             L01         
*                                                                               
         BCTR  R0,0                                                             
         LA    R6,1(R6)                                                         
*                                                                               
LHVRAT31 DS    0H                                                               
*                                                                               
         OI    PRBIND,X'40'        SET TOTAL RATE IND                           
*                                                                               
LHVRAT32 DS    0H                                                               
*                                                                               
         GOTO1 VCASHVAL,DMCB,(R6),(R0)                                          
*                                                                               
LHVRAT40 DS    0H                                                               
*                                                                               
         CLI   DMCB,0              VALID?                                       
         BNE   LHVRAT7E                                                         
*                                                                               
         ICM   R6,15,DMCB+4        RATE                                         
         BNZ   LHVRAT43                                                         
*                                                                               
         LHI   R6,-1               SET ZERO TO -.01                             
         B     LHVRAT45            TO PREVENT RATELOOK                          
*                                                                               
LHVRAT43 DS    0H                                                               
*                                                                               
         CLI   FLDDATA,C'P'        IF TAKE PCT DISCOUNT OFF RATE                
         BNE   LHVRAT45                                                         
*                                                                               
         ZAP   DUB,PRBPCT             RETRIEVE PERCENTAGE RATE                  
*                                                                               
         CVB   RF,DUB                 CVB                                       
*                                                                               
         LHI   R0,10000               FIND 100%-PERCENTAGE RATE                 
         SR    R0,RF                                                            
*                                                                               
         LR    R1,R6                  PERCENTAGE OFF RATE                       
*                                                                               
         MR    R0,R0                  PRODUCT                                   
*                                                                               
         LHI   RF,10000                                                         
*                                                                               
         BAS   RE,RTDIV                                                         
*                                                                               
         LR    R6,R1                  ACTUAL PERCENTAGE                         
*                                                                               
LHVRAT45 DS    0H                                                               
*                                                                               
         CVD   R6,DUB                                                           
*                                                                               
         ZAP   PRBRATE,DUB         RATE                                         
         B     LHVRATOK                                                         
*                                                                               
*                                                                               
LHVRATOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVRATX                                                          
*                                                                               
LHVRAT1E DS    0H                                                               
         LHI   R3,ERAORAOR         AOR CAN'T LOOK UP RATES                      
         B     LHVRATER                                                         
*                                                                               
LHVRAT2E DS    0H                                                               
         LHI   R3,ERAORADV         NOT-AUTHORIZED TO LOOKUP AOR RATES           
         B     LHVRATER                                                         
*                                                                               
LHVRAT3E DS    0H                                                               
         LHI   R3,ERRATMIX         INCONSISTENT RATE TYPES                      
         B     LHVRATER                                                         
*                                                                               
LHVRAT4E DS    0H                                                               
         LHI   R3,ERLINEIN         INCH RATE WITH LINE RATE LEVEL               
         B     LHVRATER                                                         
*                                                                               
LHVRAT5E DS    0H                                                               
         LHI   R3,ERRTYPNV         INVALID RATE TYPE                            
         B     LHVRATER                                                         
*                                                                               
LHVRAT6E DS    0H                                                               
         LHI   R3,ERINCHLN         LINE RATE WITH INCH RATE LEVEL               
         B     LHVRATER                                                         
*                                                                               
LHVRAT7E DS    0H                                                               
         LHI   R3,ERRATENV         INVALID RATE                                 
         L     R2,RATADDR          CURSOR TO RATE FIELD                         
         B     LHVRATER                                                         
*                                                                               
LHVRATER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERRATMIX EQU   520                 INVALID RATE TYPES COMBINATIONS              
ERLINEIN EQU   521                 INCH RATE FOR LINE LEVEL DISCOUNT            
ERINCHLN EQU   522                 LINE RATE FOR INCH LEVEL DISCOUNT            
ERRTYPNV EQU   534                 INVALID RATE TYPE                            
ERRATENV EQU   534                 INVALID RATE                                 
*                                                                               
LHVRATX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVDSC'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE SPACE DESCRIPTION                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVDSC   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
*        MOVE INPUT TO WORKARA                                                  
*                                                                               
         MVC   WORK,CNSPACES       INIT WORKAREA                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        INPUT LENGTH                                 
         BZ    *+20                SKIP IF NO DATA                              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FLDDATA     INPUT TO WORKAREA                            
*                                                                               
         BRAS  RE,CKSPDESP         CHECKING SPACE DESP FIELD                    
         CHI   R3,SPDSPERR         SPACE DESCRIPTION NOT FOUND?                 
         BE    LHVDSCER                                                         
*                                                                               
         CLC   =C'R=',WORK         SKIP IF NOT A RATECODE                       
         BNE   LHVDSC45                                                         
*                                                                               
*        RATECODE FORMAT IS                                                     
*        R=XXX,DESCRIPTION - DESCRIPTION IS MAX 12 LONG                         
*                                                                               
         LA    R1,PRBDESC                                                       
         LA    R6,WORK                                                          
         LA    R0,6                COUNTER - MAX CODE LENGTH IS 5               
*                                                                               
*        MOVE RATECODE TO ELEMENT                                               
*                                                                               
LHVDSCLP DS    0H                                                               
*                                                                               
         CLI   0(R6),C','          DONE IF END OF FIELD REACHED                 
         BE    LHVDSCDN                                                         
         CLI   0(R6),C' '                                                       
         BNH   LHVDSCDN                                                         
*                                                                               
         MVC   0(1,R1),0(R6)       MOVE BYTE OF CODE TO ELEMENT                 
*                                                                               
LHVDSCCN DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP POINTERS                                
         LA    R6,1(R6)                                                         
         BCT   R0,LHVDSCLP                                                      
*                                                                               
         B     LHVDSC1E                RATE CODE TOO LONG                       
*                                                                               
LHVDSCDN DS    0H                                                               
*                                                                               
         MVC   PRBDESC+5(12),1(R6) SAVE DESCRIPTION                             
         OC    PRBDESC,CNSPACES    FORCE UPPERCASE                              
*                                                                               
         CLC   PRBDESC+2(3),CNSPACES                                            
         BE    LHVDSC2E               NO RATE CODE                              
*                                                                               
*        VALID DISCOUNT TYPES FOR RATE CODES                                    
*                                                                               
         CLI   PRBLIND,C'L'        RATE CODE OKAY FOR                           
         BE    LHVDSC46               LINE OR INCH LEVEL DISCOUNTS              
         CLI   PRBLIND,C'I'                                                     
         BE    LHVDSC46                                                         
*                                                                               
         CLI   PRBLIND,C'$'         $ VOLUME                                    
         BE    LHVDSC43                                                         
         CLI   PRBLIND,C'N'         NET $ VOLUME                                
         BE    LHVDSC43                                                         
         CLI   PRBLIND,C'P'         PAGES                                       
         BE    LHVDSC43                                                         
         CLI   PRBLIND,C'X'         TIMES                                       
         BE    LHVDSC43                                                         
*                                                                               
         B     LHVDSC3E           RATE CODE NOT VALID FOR DISCOUNT TYPE         
*                                                                               
LHVDSC43 DS    0H                                                               
*                                                                               
         CP    PRBRATE,=P'0'        RATE NEEDED FOR RATECODE                    
         BE    LHVDSC4E                                                         
*                                                                               
         B     LHVDSC46                                                         
*                                                                               
LHVDSC45 DS    0H                                                               
*                                                                               
         MVC   PRBDESC,WORK         SAVE DESCRIPTION                            
*                                                                               
LHVDSC46 DS    0H                                                               
*                                                                               
*        MEDIA OUTDOOR                                                          
*                                                                               
         CLI   KBAMED,C'O'         SKIP IF NOT MEDIA OUTDOOR                    
         BNE   LHVDSCOK                                                         
*                                                                               
         CLC   =C'SRI=',WORK       OKAY IF NOT SRI=                             
         BNE   LHVDSCOK                                                         
*                                                                               
         XC    PRBDESC,PRBDESC     INIT SPACE DESCRIPTION                       
*                                                                               
         MVI   PRBDESC,X'FF'       INDICATE OUTDOOR SPACE DESCRIPTION           
*                                                                               
         CLC   =C'SPC',WORK+4      IF A SPECIAL                                 
         BNE   *+18                                                             
         ZAP   PRBDESC+1(3),=P'99999'  SET SHOWINGS TO 99999                    
         LA    R6,WORK+7              NEXT POSITION IN INPUT                    
         B     LHVDSCO5                                                         
*                                                                               
         GOTO1 =V(NUMED),DMCB,WORK+4,DUB,RR=RELO40   SHOWINGS                   
*                                                                               
         CP    DUB,=P'999'          SHOWINGS CAN'T EXCEED 100                   
         BH    LHVDSC5E             ALLOW FOR GRPS UP TO 999                    
*                                                                               
         ZAP   PRBDESC+1(3),DUB    SAVE SHOWINGS                                
*                                                                               
         L     R6,DMCB             NEXT POSITION IN INPUT                       
*                                                                               
LHVDSCO5 DS    0H                                                               
*                                                                               
         CLI   0(R6),C' '          REGULAR DISPLAYS REQUIRED                    
         BE    LHVDSC6E                                                         
*                                                                               
         GOTO1 =V(NUMED),DMCB,1(R6),DUB,RR=RELO40 EDIT REGULAR DISPLAYS         
*                                                                               
         ZAP   PRBDESC+4(3),DUB    SAVE REGULAR DISPLAYS                        
*                                                                               
         L     R6,DMCB             POINT TO NEXT POSITION IN INPUT              
*                                                                               
         CLI   0(R6),C' '                                                       
         BE    LHVDSC7E            ILLUMINATED DISPLAYS REQUIRED                
*                                                                               
         GOTO1 =V(NUMED),DMCB,1(R6),DUB,RR=RELO40 EDIT ILLUM DISPLAYS           
*                                                                               
         ZAP   PRBDESC+7(3),DUB    SAVE ILLUMINATED DISPLAYS                    
*                                                                               
         L     R6,DMCB             POINT TO NEXT AVAILABLE INPUT                
*                                                                               
         CLI   0(R6),C' '                                                       
         BH    LHVDSC8E                                                         
*                                                                               
LHVDSCOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVDSCX                                                          
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
LHVDSC1E DS    0H                                                               
         LHI   R3,ERRCLONG         RATE CODE MAX 3 LONG                         
         B     LHVDSCER                                                         
*                                                                               
LHVDSC2E DS    0H                                                               
         LHI   R3,ERRCREQD         RATE CODE REQUIRED                           
         B     LHVDSCER                                                         
*                                                                               
LHVDSC3E DS    0H                                                               
         LHI   R3,ERRCLORI         RATE CODE REQUIRES INCH OR LINE LVL          
         L     R2,LUACLIN          CURSOR TO LVL IND FIELD                      
         B     LHVDSCER                                                         
*                                                                               
LHVDSC4E DS    0H                                                               
         LHI   R3,ERRCRTRQ         RATE REQUIRED                                
         L     R2,RATADDR          CURSOR TO RATE FIELD                         
         B     LHVDSCER                                                         
*                                                                               
LHVDSC5E DS    0H                                                               
         LHI   R3,ERSHWLNG         SHOWINGS HAVE 100 MAX                        
         B     LHVDSCER                                                         
*                                                                               
LHVDSC6E DS    0H                                                               
         LHI   R3,ERREGSNE         NUMBER OF REGULAR DISPLAYS REQUIRED          
         B     LHVDSCER                                                         
*                                                                               
LHVDSC7E DS    0H                                                               
         LHI   R3,ERILLMNE         NUMBER OF ILLUM   DISPLAYS REQUIRED          
         B     LHVDSCER                                                         
*                                                                               
LHVDSC8E DS    0H                                                               
         LHI   R3,ERSRINV          SRI= ENTRY NOT VALID                         
         B     LHVDSCER                                                         
*                                                                               
LHVDSCER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERRCLONG EQU   523                 RATE CODE MAX 3 LONG                         
ERRCREQD EQU   524                 RATE CODE REQUIRED                           
ERRCLORI EQU   525                 RATE CODE REQUIRES INCH OR LINE LVL          
ERRCRTRQ EQU   526                 RATE CODE REQUIRES A RATE                    
ERSHWLNG EQU   527                 SHOWINGS HAVE MAX 100                        
ERREGSNE EQU   528                 REGULAR DISPLAYS NOT ENTERED                 
ERILLMNE EQU   529                 ILLUM   DISPLAYS NOT ENTERED                 
ERSRINV  EQU   530                 SRI ENTRY INVALID                            
*                                                                               
LHVDSCX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVEFD'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE EFFECTIVE DATE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVEFD   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD ON LINE                   
*                                                                               
         CLI   FLDILEN,0           INPUT NOT REQUIRED                           
         BE    LHVEFDOK                                                         
*                                                                               
         LA    R6,FLDDATA          START OF INPUT                               
*                                                                               
         CLI   0(R6),C'-'          SKIP IF IT STARTS WITH C'-'                  
         BE    LHVEFDFD                                                         
*                                                                               
         GOTO1 VDATVAL,DMCB,FLDDATA,DUB       VALIDATE EFFECTIVE DATE           
*                                                                               
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    LHVEFD1E                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PRBDATE)   CONVERT TO BINARY             
*                                                                               
*        DASH INDICATES RATE IS FOR SPECIAL PRODUCT                             
*                                                                               
LHVEFDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),C' '          DONE AT END OF INPUT                         
         BNH   LHVEFDDN                                                         
*                                                                               
         CLI   0(R6),C'-'          CHECK FOR DASH                               
         BE    LHVEFDFD                                                         
*                                                                               
         LA    R6,1(R6)                                                         
         B     LHVEFDLP                                                         
*                                                                               
LHVEFDFD DS    0H                                                               
*                                                                               
         CLI   KBAMED,C'N'        PRODUCT OKAY FOR NON NEWS ONLY                
         BNE   LHVEFDPR                                                         
*                                                                               
         CP    PRBRATE,=P'0'       NEWSPAPER REQUIRES RATE                      
         BH    LHVEFDPR               FOR PRODUCT                               
*                                                                               
         B     LHVEFD2E                                                         
*                                                                               
LHVEFDPR DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT IS ON FILE                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY              ESTABLISH PRODUCT RECORD KEY                 
         USING PPRDREC,RF                                                       
*                                                                               
         MVC   PPRDKAGY,AGYALPHA   SET AGENCY                                   
         MVC   PPRDKMED,KBAMED     SET MEDIA                                    
         MVI   PPRDKRCD,X'06'      SET RECORD ID                                
         MVC   PPRDKCLT,SAVKKEY+4  SET CLIENT                                   
         MVC   PPRDKPRD,1(R6)      SET PRODUCT FROM INPUT                       
         OC    PPRDKPRD,=C'   '    MAKE PRODUCT IS UPPERCASE                    
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',                    X        
               KEY,KEY,(TERMNAL,0)                                              
*                                                                               
         CLI   DMCB+8,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'PPRDKEY),KEYSAVE     MUST FIND PRODUCT                     
         BE    LHVEFDP1                                                         
*                                                                               
         CLI   SAVCLTPR+5,C'1'        SEE IF MASTER CLIENT                      
         BNE   LHVEFD3E               I WON'T FIND THE PRODUCT                  
*                                                                               
LHVEFDP1 DS    0H                                                               
*                                                                               
         LA    RF,KEY              POINT TO FOUND KEY                           
*                                                                               
         MVC   PRBOPEN(3),PPRDKPRD   SAVE PRODUCT IN PRBOPEN                    
*                                                                               
         MVC   KEY,SAVKKEY         RE-READ CONTRACT KEY                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',                    X        
               KEY,KEY,(TERMNAL,0)                                              
*                                                                               
         DROP  RF                                                               
*                                                                               
LHVEFDDN DS    0H                                                               
*                                                                               
LHVEFDOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHVEFD70 DS    0H             IF NO RATE ENTERED - ATTEMPT TO LOOK-UP           
         XC    WORK2,WORK2       CLEAR RTLOOK OUTPUT AREA                       
         CP    PRBRATE,=P'0'     SEE IF RATE ENTERED                            
         BNE   LHVEFD7X                                                         
*                                                                               
         L     RF,APUBIO           POINT TO PUB RECORD AREA                     
*                                                                               
         CLI   0(RF),0             TEST PUBREC IN CORE                          
         BNE   LHVEFD75               YES                                       
*                                                                               
         MVC   KEY+27(4),SAVPUBA   PUBREC DISK ADDR                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',         X        
               KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
LHVEFD75 DS    0H                                                               
*                                                                               
*        GET RATE BASIS ELEMENTS FROM PUB RECORD                                
*                                                                               
*       CREATE DUMMY CONTRACT FOR PUB RATE LOOK-UP                              
*       WITH ONLY THE ELEMENT FROM THE TABLE                                    
*                                                                               
         XC    LTEMPCON,LTEMPCON                                                
         MVC   LTEMPCON(70),PCONREC  KEY PLUS PCONDESC                          
         MVC   LTEMPCON+70(L'ELTELEM),ELTELEM  ELEMENT FROM TABLE               
*                                                                               
         GOTO1 VRTLOOK,DMCB,0,(ELCODE,APUBIO),LTEMPCON,WORK2                    
*                                                                               
         CLI   DMCB,0              ERROR?                                       
         BE    LHVEFD78                                                         
         LA    R2,RTSRT1H                                                       
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         B     ERREX                                                            
*                                                                               
LHVEFD78 CLI   WORK2,0      NO ELEMENTS FOUND                                   
         BNE   *+6                                                              
         DC    H'0'         SOMETHING VERY WRONG                                
*                                                                               
         MVC   ELTELEM,WORK2       SAVE IN ELEMENT TABLE                        
*                                                                               
*        REISPLAY THE RATE LOOKED-UP                                            
*                                                                               
         L     R2,RATADDR        SHOULD BE ADDRESS OF RATE FIELD                
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
*        DETERMINE IF A LINE RATE                                               
*                                                                               
         TM    PRBIND,X'40'        IF TOTAL RATE                                
         BO    LHVEFDTR               NO                                        
*                                                                               
         TM    PRBIND,X'20'        IF UNIT RATE                                 
         BO    LHVEFDLR               YES                                       
*                                                                               
         CLI   KBAMED,C'N'         IF NEWSPAPER                                 
         BE    LHVEFDLR               YES                                       
*                                                                               
         CLI   PRBLIND,C'I'        IF INCH RATE                                 
         BE    LHVEFDLR               YES                                       
*                                                                               
         CLI   PRBLIND,C'L'        IF LINE RATE                                 
         BE    LHVEFDLR               YES                                       
*                                                                               
         B     LHVEFDTR            ELSE NO                                      
*                                                                               
*        LINE RATE  5 DECIMALS                                                  
*                                                                               
LHVEFDLR DS    0H                                                               
*                                                                               
         MVI   FLDDATA,C'U'        SET UNIT RATE IND                            
*                                                                               
         LA    R6,FLDDATA+1        SET DEFAULT OUTPUT POINTER                   
*                                                                               
         CLI   KBAMED,C'N'         IF NON-NEWS                                  
         BNE   LHVEFD10                                                         
*                                                                               
         CLI   PRBDESC,C' '        OR SPACE BUY                                 
         BH    LHVEFD10                                                         
*                                                                               
         LA    R6,FLDDATA          ELSE ASSUMED TO BE UNIT RATE                 
*                                                                               
LHVEFD10 DS    0H                                                               
*                                                                               
         TM    PRBIND,X'10'     NET RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'N'          NET INDICATIOR                               
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'02'       S RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'S'         S RATE INDICATIOR                             
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'04'     SEE IF C RATE                                   
         BNO   *+12                                                             
         MVI   0(R6),C'C'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         EDIT  (P5,PRBRATE),(13,(R6)),5,ALIGN=LEFT,FLOAT=-,COMMAS=YES           
*                                                                               
         CLI   PRBLIND,C'I'   INCH LEVEL MUST BE INCH RATE                      
         BE    LHVEFD20                                                         
*                                                                               
         TM    PRBIND,X'08'    SEE IF INCH RATE                                 
         BNO   LHVEFD22                                                         
*                                                                               
LHVEFD20 DS    0H                                                               
*                                                                               
         AR    R6,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R6),=C'/I'                                                   
*                                                                               
         B     LHVEFD30                                                         
*                                                                               
LHVEFD22 DS    0H                                                               
*                                                                               
         AR    R6,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R6),=C'/L'   MUST BE LINE RATE                               
*                                                                               
         B     LHVEFD30                                                         
*                                                                               
*              2 DECIMAL RATE                                                   
*                                                                               
LHVEFDTR DS    0H                                                               
*                                                                               
         LA    R6,FLDDATA          SET DEFAULT OUTPUT POINTER                   
*                                                                               
         CLI   KBAMED,C'N'         IF NEWSPAPERS                                
         BNE   LHVEFDT1                                                         
*                                                                               
         CLI   PRBDESC,C' '        AND SPACE BUY                                
         BH    LHVEFDT1                                                         
*                                                                               
         MVI   FLDDATA,C'T'        INDICATE TOTAL RATE                          
         LA    R6,1(R6)                                                         
*                                                                               
LHVEFDT1 DS    0H                                                               
*                                                                               
         TM    PRBIND,X'10'     NET RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'N'       NET INDICATIOR                                  
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'02'       S RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'S'       NET INDICATIOR                                  
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'04'     SEE IF C RATE                                   
         BNO   *+12                                                             
         MVI   0(R6),C'C'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         EDIT  (P5,PRBRATE),(13,(R6)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES           
*                                                                               
LHVEFD30 DS    0H                                                               
         BAS   RE,BUMP             BUMP BACK TO DESCRIPTION FIELD               
         BAS   RE,BUMP             BUMP BACK TO EFFECTIVE DATE                  
*                                                                               
*       REDISPLAY EFFECTIVE DATE AS IT TOO MAY HAVE BEEN LOOKED UP              
*                                                                               
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LA    R6,FLDDATA          SET OUTPUT POINTER                           
*                                                                               
         OC    PRBDATE,PRBDATE     SKIP IF NO EFFECTIVE DATE?                   
         BZ    LHVEFX10                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PRBDATE),(5,(R6))                                
         LA    R6,8(R6)        BUMP PAST DATE                                   
*                                                                               
LHVEFX10 DS    0H                                                               
*                                                                               
*        NOW FOR ALL MEDIA                                                      
*                                                                               
         CLI   PRBOPEN,C'A'           SEE IF PRODUCT IN PRBOPEN                 
         BL    *+14                                                             
         MVI   0(R6),C'-'                                                       
         MVC   1(3,R6),PRBOPEN          PRODUCT IN PRBOPEN                      
*                                                                               
LHVEFD7X DS    0H                                                               
         B     LHVEFDX                                                          
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
LHVEFD1E DS    0H                                                               
         LHI   R3,EREFDNV          INVALID EFFECTIVE DATE                       
         B     LHVEFDER                                                         
*                                                                               
LHVEFD2E DS    0H                                                               
         LHI   R3,EREFDPRN         PRODUCT NOT VALID FOR NEWSPAPERS             
         B     LHVEFDER                                                         
*                                                                               
LHVEFD3E DS    0H                                                               
         LHI   R3,ERPRDNV          PRODUCT NOT FOUND                            
         B     LHVEFDER                                                         
*                                                                               
EREFDNV  EQU   531                 EFD NOT VALID                                
EREFDPRN EQU   532                 PRODUCT NOT ALLOWED FOR NEWPAPERS            
ERPRDNV  EQU   533                 PRODUCT NOT FOUND                            
*                                                                               
LHVEFDER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
LHVEFDX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHVAOR'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE AOR RATES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVAOR   DS    0H                                                               
*                                                                               
         CP    PRBLEVEL,=P'-2'   SKIP IF NO AOR RATES INVOLVED                  
         BE    *+10                                                             
         CP    PRBRATE,=P'-2'                                                   
         BNE   LHVAORX                                                          
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE                 
*                                                                               
         BRAS  RE,CKADVC                                                        
*                                                                               
         CLI   ERRAREA,X'FF'                                                    
         BNE   LHVAORX                                                          
*                                                                               
         L     R2,LEVADDR                                                       
*                                                                               
         CP    PRBLEVEL,=P'-2'                                                  
         BE    LHVAORX                                                          
*                                                                               
         L     R2,RATADDR                                                       
         B     LHVAORX                                                          
*                                                                               
LHVAORX  DS    0H                                                               
*                                                                               
*        ALL FIELDS ON LINE ARE VALID                                           
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         L     RF,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,RF                                                       
*                                                                               
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+8                                                              
         LA    RF,LSVKEYNW            USE IT                                    
*                                                                               
         OC    LSVKEY,LSVKEY       IF THERE WAS NOTHING LAST TIME               
         BNZ   LHVTLINX                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVELTKEY+1         BUMP SUB LINE NUMBER                       
         LA    R1,1(R1)               OF LAST USED ELEMENT KEY                  
         STC   R1,SVELTKEY+1                                                    
*                                                                               
         LA    RF,SVELTKEY            POINT TO LAST USED KEY                    
*                                                                               
LHVTLINX DS    0H                                                               
*                                                                               
         MVC   ELTKEY,LSVKEY       SET SORT KEY TO LINE NUMBER                  
*                                                                               
         MVC   SVELTKEY,ELTKEY     SAVE CURRENT KEY                             
*                                                                               
         DROP  RF                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
*                                                                               
*        ADD NEW ELEMENT TO TABLE                                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO40                  
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1       SET CURRENT ELEMENT ADDRESS                  
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEME                 
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEY     ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEYNW   ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PRBELEM     UPDATE ELEM IN TABLE                         
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*********                                                                       
         B     LHVLCPDN            BYPASS DUPLICATE RATE CHECK                  
********                                                                        
*        CHECK IF THERE IS A DUPLICATE RATE                                     
*                                                                               
         ICM   R0,15,BSPNOR        NUMBER OF ELEMENTS IN TABLE                  
         ICM   R4,15,ELTLAST       END OF TABLE                                 
*                                                                               
LHVLCPLP DS    0H                                                               
*                                                                               
         CLC   ELTELEM,PRBELEM     MACTCH RATE ELEMENTS                         
         BNE   LHVLCPCN                                                         
*                                                                               
         C     R4,ELTENT           OKAY IF SAME ELEMENT                         
         BE    LHVLCPCN                                                         
*                                                                               
         TM    ELTCTL,ELTDELQ      OKAY IF DELETED                              
         BNO   LHVCMPE2                                                         
         TM    ELTCTL,ELTADDQ      BUT NOT ADDED (EQUALS CHANGED)               
         BO    LHVCMPE2                                                         
*                                                                               
LHVLCPCN DS    0H                                                               
*                                                                               
         S     R4,BSPLENR          BUMP TO NEXT TABLE ELEMENT                   
         BCT   R0,LHVLCPLP                                                      
*                                                                               
LHVLCPDN DS    0H                                                               
*                                                                               
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVLONGE DS    0H                                                               
         LHI   R3,ERTOOLNG            INPUT TOO LONG                            
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE1 DS    0H                                                               
         LHI   R3,ERRECFUL            TOO MANY DETAIL LINES                     
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE                 
         LHI   R3,ERDUPDAT         DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
ERTOOLNG EQU   32                  INPUT TOO LONG                               
ERRECFUL EQU   536                 TOO MANY DISCOUNT RATES.                     
ERDUPDAT EQU   537                 DUPLICATE RATES                              
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         J     ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CR    RB,RB               SET CC CODE                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
LTEMPCON DS    CL200     DUMMY TEMP CONTRACT                                    
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
CKSPDESP NTR1  BASE=*,LABEL=*      WORK HAS CURRENT SPACE DESP VALUE            
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
*                                                                               
         CLI   WORK,C'='           BROWSING?                                    
         BNE   CKSPD30                                                          
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(KBAMED,C' STD'),0,RR=RELO40                                   
                                                                                
         DC    H'0'                BROWSE KNOWS WHERE TO GET BACK               
*                                                                               
CKSPD30  MVC   CKSPKEY(L'KEY),KEY                                               
         MVC   CKSPKEY+L'KEY(L'KEYSAVE),KEYSAVE                                 
*                                                                               
         XC    CKSPWORK,CKSPWORK                                                
         MVC   CKSPWORK+30(4),=C'P0BY'                                          
         MVC   CKSPWORK+34(2),AGYALPHA                                          
         MVC   CKSPWORK+36(1),KBAMED                                            
         MVC   CKSPWORK+37(3),SAVCLT                                            
         CLI   SAVCLTOF,C' '       OFFICE CODE PRESENT?                         
         BNH   *+14                                                             
         MVI   CKSPWORK+40,C'*'                                                 
         MVC   CKSPWORK+41(1),SAVCLTOF                                          
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGETPROF,DMCB,(0,CKSPWORK+30),CKSPWORK,VDATAMGR                  
         DROP  R4                                                               
*                                                                               
         CLI   CKSPWORK+9,C'Y'     PROFILE ALLOWS SPACE DESP LOOK UP?           
         BNE   CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         XC    CKSPWORK,CKSPWORK                                                
         MVC   CKSPWORK(17),WORK                                                
         OC    CKSPWORK(17),=17C' '                                             
*                                                                               
* SEE IF ANY SPECIAL FORMAT NEED TO BE SKIPPED                                  
*                                                                               
         CLC   CKSPWORK(17),=17C' '                                             
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
         CLC   CKSPWORK(02),=C'R='                                              
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
         CLI   KBAMED,C'O'                                                      
         BNE   *+14                                                             
         CLC   CKSPWORK(04),=C'SRI='                                            
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
* SEE IF SPACE DESP ENTERED IS SAME AS IN RECORD NOW                            
*                                                                               
         MVI   CKSPIND,C'N'        INIT TO NOT SAME                             
*                                                                               
         MVI   CKSPECOD,X'20'      RATE BASIS ELEM (CURRENT LEVEL)              
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         MVI   CKSPECOD,X'21'      RATE BASIS ELEM (CURRENT LEVEL)              
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         MVI   CKSPECOD,X'22'      RATE BASIS ELEM (CURRENT LEVEL)              
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         MVI   CKSPECOD,X'24'      RATE BASIS ELEM (CURRENT LEVEL)              
         BAS   R8,CKSPD70                                                       
         CLI   CKSPIND,C'Y'        SAME AS ON RECORD?                           
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),KBAMED                                                
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(17),CKSPWORK                                              
*                                                                               
         MVC   KEYSAVE,KEY         FOR COMPARISON LATER                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE     INPUT MATCHED THAT OF RECORD?                
         BE    CKSPDX2             YES, VALIDATED AND DONE                      
         LA    R3,SPDSPERR                                                      
*                                                                               
CKSPDX1  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
*                                                                               
         XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
*                                                                               
CKSPDX2  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
*                                                                               
         XIT1                      ALL REGISTERS RESTORED                       
*                                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPD70  DS    0H                                                               
         LA    R5,PCONREC+33                                                    
         CLI   0(R5),X'10'         FIRST CONTRACT ELEM EXIST?                   
         BE    *+6                                                              
         DC    H'0'                BAD CONTRACT RECORD                          
CKSPD70H BAS   RE,CKSPNEL                                                       
         BNER  R8                                                               
         USING PRBELEM,R5                                                       
         CLC   PRBDESC,WORK        SAME AS ON RECORD?                           
         BNE   CKSPD70H            NO, CKECK NEXT ELEM                          
         MVI   CKSPIND,C'Y'                                                     
         BR    R8                                                               
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPNEL  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   CKSPECOD,0(R5)                                                   
         BCR   8,RE                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
CKSPNELX LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
CKSPWORK DS    CL250               WORKING STORAGE AREA                         
CKSPECOD DS    C                   BYTE FOR ELEMENT CODE                        
CKSPIND  DS    C                   INDICATOR FOR SAME SPACE DESP                
CKSPKEY  DS    CL100               FOR RESTORING KEYS                           
*                                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDISLIN'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDINITX            CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PRBELEM,R3                                                       
*                                                                               
LHDINITX DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDIND'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY RATE INDICATOR                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDIND   DS    0H                                                               
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDINDOK                                                         
*                                                                               
         CLI   NODISP,X'FF'        ONLY  DISPLAYING SPACE FOR AOR AGYS          
         BE    LHDINDOK            BYPASS                                       
*                                                                               
         MVC   FLDDATA(L'PRBLIND),PRBLIND     LBL IND                           
*                                                                               
         MVI   FLDILEN,L'PRBLIND   SET INPUT LENGTH                             
*                                                                               
         CLI   PRBLIND,C'N'        MEANS NET DOLLAR VOLUME                      
         BNE   *+8                                                              
         MVI   FLDDATA,C'$'        DISPLAY "N" AS "$"                           
*                                                                               
LHDINDOK DS    0H                                                               
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDINDX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDLVL'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY LEVEL                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDLVL   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD ON LINE                   
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDLVLOK                                                         
*                                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR AOR               
         BE    LHDLVLOK            BYPASS                                       
*                                                                               
         CLI   PRBLIND,C'N'         SEE IF NET $ VOLUME                         
         BNE   LHDLVL10                                                         
*                                                                               
         MVI   FLDDATA,C'N'         PRECEED LEVEL WITH 'N'                      
*                                   AND LEVEL WITHOUT COMMAS                    
         EDIT  (P5,PRBLEVEL),(7,FLDDATA+1),ALIGN=LEFT                           
*                                                                               
         B     LHDLVL50                                                         
*                                                                               
LHDLVL10 DS    0H                                                               
*                                                                               
         CP    PRBLEVEL,=P'999999'       SEE IF OVER 1,000,000                  
         BNH   LHDLVL20                                                         
*                                                                               
         EDIT  (P5,PRBLEVEL),(8,FLDDATA),ALIGN=LEFT                             
*                                                                               
         B     LHDLVL50                                                         
*                                                                               
LHDLVL20 DS    0H                                                               
*                                                                               
         EDIT  (P5,PRBLEVEL),(8,FLDDATA),ALIGN=LEFT,COMMAS=YES                  
*                                                                               
LHDLVL50 DS    0H                                                               
*                                                                               
         CP    PRBLEVEL,=P'0'      IF NO LEVEL                                  
         BNE   LHDLVL60                                                         
*                                                                               
         CLI   PRBLIND,C'S'        AND NOT A SPECIAL RATE                       
         BE    LHDLVL60                                                         
*                                                                               
         MVC   FLDDATA(4),=C'OPEN' THEN ITS AN OPEN RATE                        
*                                                                               
         TM    PRBIND,X'01'        OR POSSIBLY A FLAT RATE                      
         BZ    *+10                                                             
         MVC   FLDDATA(4),=C'FLAT'                                              
*                                                                               
LHDLVL60 DS    0H                                                               
*                                                                               
LHDLVLOK DS    0H                                                               
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDLVLX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDPCT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY PER CENT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDPCT   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD ON LINE                   
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDPCTOK                                                         
*                                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR AOR               
         BE    LHDPCTOK            BYPASS                                       
*                                                                               
         CLI   PRBELEM+1,42        FIELD NOT IN OLD STYLE ELEMENTS              
         BL    LHDPCTOK                                                         
*                                                                               
*        DISPLAY PER CENT TO 2 DECIMALS                                         
*                                                                               
         EDIT  (P3,PRBPCT),(5,FLDDATA),2,ALIGN=LEFT                             
*                                                                               
         LA    R0,3                COUNTER                                      
         LA    R1,FLDDATA          START OF OUTPUT                              
*                                                                               
*        PRINT WHOLE NUMBER PER CENT WITHOUT DECIMALS                           
*                                                                               
         CLC   =C'.00',0(R1)       FIND WHOLE NUMBER PER CENT                   
         BE    *+16                                                             
         AHI   R1,1                BUMP POINTER                                 
         BCT   R0,*-14                                                          
         B     *+10                   NOT A WHOLE PER CENT                      
         MVC   0(3,R1),CNSPACES    ELMINATE DECIMALS                            
*                                                                               
LHDPCTOK DS    0H                                                               
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDPCTX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDRAT'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY RATE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDRAT   DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO RTE FIELD                            
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR AOR               
         BE    LHDRATOK            BYPASS                                       
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDRATOK                                                         
*                                                                               
         CLC   =C'R=',PRBDESC      IF RATE CODE                                 
         BNE   *+12                                                             
         LA    R6,FLDDATA             SET OUTPUT POINTER                        
         B     LHDRAT10                                                         
*                                                                               
*        DETERMINE IF A LINE RATE                                               
*                                                                               
         TM    PRBIND,X'40'        IF TOTAL RATE                                
         BO    LHDRATTR               NO                                        
*                                                                               
         TM    PRBIND,X'20'        IF UNIT RATE                                 
         BO    LHDRATLR               YES                                       
*                                                                               
         CLI   KBAMED,C'N'         IF NEWSPAPER                                 
         BE    LHDRATLR               YES                                       
*                                                                               
         CLI   PRBLIND,C'I'        IF INCH RATE                                 
         BE    LHDRATLR               YES                                       
*                                                                               
         CLI   PRBLIND,C'L'        IF LINE RATE                                 
         BE    LHDRATLR               YES                                       
*                                                                               
         B     LHDRATTR            ELSE NO                                      
*                                                                               
*        LINE RATE  5 DECIMALS                                                  
*                                                                               
LHDRATLR DS    0H                                                               
*                                                                               
         MVI   FLDDATA,C'U'        SET UNIT RATE IND                            
*                                                                               
         LA    R6,FLDDATA+1        SET DEFAULT OUTPUT POINTER                   
*                                                                               
         CLI   KBAMED,C'N'         IF NON-NEWS                                  
         BNE   LHDRAT10                                                         
*                                                                               
         CLI   PRBDESC,C' '        OR SPACE BUY                                 
         BH    LHDRAT10                                                         
*                                                                               
         LA    R6,FLDDATA          ELSE ASSUMED TO BE UNIT RATE                 
*                                                                               
LHDRAT10 DS    0H                                                               
*                                                                               
         TM    PRBIND,X'10'     NET RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'N'          NET INDICATIOR                               
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'02'       S RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'S'         S RATE INDICATIOR                             
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'04'     SEE IF C RATE                                   
         BNO   *+12                                                             
         MVI   0(R6),C'C'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         EDIT  (P5,PRBRATE),(13,(R6)),5,ALIGN=LEFT,FLOAT=-,COMMAS=YES           
*                                                                               
         CLI   PRBLIND,C'I'   INCH LEVEL MUST BE INCH RATE                      
         BE    LHDRAT20                                                         
*                                                                               
         TM    PRBIND,X'08'    SEE IF INCH RATE                                 
         BNO   LHDRAT22                                                         
*                                                                               
LHDRAT20 DS    0H                                                               
*                                                                               
         AR    R6,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R6),=C'/I'                                                   
*                                                                               
         B     LHDRAT30                                                         
*                                                                               
LHDRAT22 DS    0H                                                               
*                                                                               
         AR    R6,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R6),=C'/L'   MUST BE LINE RATE                               
*                                                                               
         B     LHDRAT30                                                         
*                                                                               
*              2 DECIMAL RATE                                                   
*                                                                               
LHDRATTR DS    0H                                                               
*                                                                               
         LA    R6,FLDDATA          SET DEFAULT OUTPUT POINTER                   
*                                                                               
         CLI   KBAMED,C'N'         IF NEWSPAPERS                                
         BNE   LHDRATT1                                                         
*                                                                               
         CLI   PRBDESC,C' '        AND SPACE BUY                                
         BH    LHDRATT1                                                         
*                                                                               
         MVI   FLDDATA,C'T'        INDICATE TOTAL RATE                          
         LA    R6,1(R6)                                                         
*                                                                               
LHDRATT1 DS    0H                                                               
*                                                                               
         TM    PRBIND,X'10'     NET RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'N'       NET INDICATIOR                                  
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'02'       S RATE                                        
         BNO   *+12                                                             
         MVI   0(R6),C'S'       NET INDICATIOR                                  
         LA    R6,1(R6)                                                         
*                                                                               
         TM    PRBIND,X'04'     SEE IF C RATE                                   
         BNO   *+12                                                             
         MVI   0(R6),C'C'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         EDIT  (P5,PRBRATE),(13,(R6)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES           
*                                                                               
LHDRAT30 DS    0H                                                               
*                                                                               
LHDRATOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDRATX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDDSC'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY DESCRIPTION                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDDSC   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO DESCRIPTION FIELD                    
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDDSCOK                                                         
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         CLC   =C'R=',PRBDESC      IF RATE CODE IN DESCRIPTION FIELD            
         BNE   LHDDSCRN                                                         
*                                                                               
         LA    R1,FLDDATA             POINT TO START OF OUTPUT                  
*                                                                               
         MVC   0(5,R1),PRBDESC        DISPLAY AS R=CCC,DESCRIPTION              
*                                                                               
         LA    R1,4(R1)               FIND NEXT AVAILABLE SLOT                  
         LA    R0,3                   MAX CODE LENGTH                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         MVI   1(R1),C','                                                       
         MVC   2(12,R1),PRBDESC+5     RATE CODE DESCRIPTION                     
*                                                                               
         B     LHDDSCOK                                                         
*                                                                               
LHDDSCRN DS    0H                                                               
*                                                                               
         MVC   FLDDATA(17),PRBDESC    DESCRIPTION                               
*                                                                               
         CLI   PRBDESC,X'FF'       SKIP IF NOT OUTDOOR RATES                    
         BNE   LHDDSC70                                                         
*                                  SPECIAL OUTDOOR FIELDS                       
         MVC   FLDDATA(8),=C'SRI=SPC,'  ASSUME SPECIAL SHOWINGS                 
         LA    R6,FLDDATA+8                                                     
*                                                                               
         CP    PRBDESC+1(3),=P'99999'   DONE IF SPECIAL SHOWINGS                
         BE    LHDDSC60                                                         
*                                                                               
         LA    R6,FLDDATA+4        START OF SHOWINGS                            
         LA    R1,PRBDESC+1                                                     
*                                                                               
         MVI   0(R6),C'0'          DEFAULT SHOWINGS                             
         LA    R0,1                DEFAULT DISPLAY LENGTH                       
*                                                                               
         CP    0(3,R1),=P'0'                                                    
         BE    LHDDSC52                                                         
*                                                                               
         EDIT  (P3,0(R1)),(4,0(R6)),ALIGN=LEFT PRINT SHOWINGS                   
*                                                                               
LHDDSC52 DS    0H                                                               
*                                                                               
         AR    R6,R0               BUMP DISPLAY POINTER                         
*                                                                               
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
LHDDSC60 DS    0H                                                               
*                                                                               
         LA    R1,PRBDESC+4        POINT TO REGULAR DISPLAYS                    
*                                                                               
         MVI   0(R6),C'0'          DEFAULT IS ZERO                              
         LA    R0,1                                                             
*                                                                               
         CP    0(3,R1),=P'0'                                                    
         BE    LHDDSC62                                                         
*                                                                               
         EDIT  (P3,0(R1)),(4,0(R6)),ALIGN=LEFT DISPLAY REGULAR DISPLAYS         
*                                                                               
LHDDSC62 DS    0H                                                               
*                                                                               
         AR    R6,R0               NEXT AVAILABLE SLOT                          
*                                                                               
         MVI   0(R6),C','          SEPARATOR                                    
         LA    R6,1(R6)                                                         
*                                                                               
         LA    R1,PRBDESC+7        ILLUMS                                       
*                                                                               
         MVI   0(R6),C'0'          DEFAULT IS ZERO                              
         LA    R0,1                                                             
*                                                                               
         CP    0(3,R1),=P'0'                                                    
         BE    LHDDSC64                                                         
*                                                                               
         EDIT  (P3,0(R1)),(4,0(R6)),ALIGN=LEFT                                  
*                                                                               
LHDDSC64 DS    0H                                                               
*                                                                               
LHDDSC70 DS    0H                                                               
*                                                                               
         CLI   WORK2,0             SEE IF I DID A RATELOOK                      
         BE    LHDDSCOK            NO  (WORK2 SHOULD HAVE RATE ELEM)            
*                                                                               
         CP    PRBLEVEL,=P'0'      AND A NON-OPEN LEVEL?                        
         BE    LHDDSCOK                                                         
*                                                                               
         CLI   PRBOPEN,C'A'        MAY CONTAIN PRODUCT CODE                     
         BNL   LHDDSCOK                                                         
*                                                                               
*                                  DISPLAY OPEN RATE                            
         MVC   FLDDATA(5),=C'OPEN='                                             
*                                                                               
         EDIT  (P5,PRBOPEN),(8,FLDDATA+5),5,ALIGN=LEFT                          
*                                                                               
LHDDSCOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDDSCX  DS    0H                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHDEFD'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY EFFECTIVE DATE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDEFD   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO EFFECTIVE DATE FIELD                 
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         MVI   FLDILEN,0           KILL ANY INPUT LENGTH                        
*                                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    LHDEFDOK            BYPASS                           L02         
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDEFDOK                                                         
*                                                                               
         LA    R6,FLDDATA          SET OUTPUT POINTER                           
*                                                                               
         OC    PRBDATE,PRBDATE     SKIP IF NO EFFECTIVE DATE?                   
         BZ    LHDEFD10                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PRBDATE),(5,(R6))                                
         LA    R6,8(R6)        BUMP PAST DATE                                   
*                                                                               
LHDEFD10 DS    0H                                                               
*                                                                               
*****    CLI   KBAMED,C'N'         IF NON-NEWSPAPER                             
*****    BE    LHDEFDOK                                                         
*                                                                               
         CLI   PRBOPEN,C'A'           SEE IF PRODUCT IN PRBOPEN   L06           
         BL    *+14                                              L06            
         MVI   0(R6),C'-'                                                       
         MVC   1(3,R6),PRBOPEN          PRODUCT IN PRBOPEN                      
*                                                                               
LHDEFDOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDEFDX  DS    0H                                                               
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - LHWRTTAB'                          
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
*        READ CONTRACT RECORD FOR UPDATE                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMREAD'),=C'PRTDIR',KEY,KEY,    X        
               (TERMNAL,0)                                                      
*                                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'PRTFILE',KEY+27,    X        
               AREC,(TERMNAL,DMWORK)                                            
*                                                                               
*        REMOVE ALL RATE ELEMENTS                                               
*                                                                               
         GOTO1 DELELEM             CLEAR RATE ELEMENTS OUT OF RECORD            
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
         SR    R2,R2               INIT LINE COUNTER                            
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R6,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PRBELEM,R6          ESTABLISH AS RATE ELEMENT                    
*                                                                               
         CLC   PRBELEM,ELCODE      SKIP ANY ELTS THAT AREN'T      *BOBY         
         BNE   LSWTADDX           BEING UPDATED. OCCURS WHEN REGULAR            
*                                 ELEMENTS ARE USED TO SEED OTHER LEVLS         
*                                                                               
         LA    R2,1(R2)            BUMP TEXT LINE NUMBER                        
*                                                                               
         XC    ELTKEYNW,ELTKEYNW   SAVE THIS NEW KEY                            
         STC   R2,ELTKEYNW                                                      
*                                                                               
         CP    PRBRATE,=P'-1'                                                   
         BNE   *+10                                                             
         ZAP   PRBRATE,=P'0'                                                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PRBELEM+1      GET ELEMENT LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),PRBELEM  MOVE ELEMENT TO WORK AREA                    
*                                                                               
         BRAS  RE,ADELEM           PUT ELEMENT IN RECORD                        
*                                                                               
         BE    LSWTAD1E            ONLY ERROR CAN BE NO ROOM IN REC             
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
         LA    R0,NLINS            NUMBER OF LINES IN TABLE                     
         LHI   R5,LSVTAB-T40DFFD   LINUP SAVE AREA                              
         LA    R5,T40DFFD(R5)      POINT TO FIRST SAVED ENTRY                   
*                                                                               
LSWLSVLP DS    0H                                                               
*                                                                               
         USING LSVTABD,R5          ESTABLISH SAVED KEYS AREA                    
*                                                                               
         OC    LSVKEY,LSVKEY       SKIP IF NO ENTRY SAVED                       
         BNZ   *+10                                                             
         OC    LSVKEYNW,LSVKEYNW                                                
         BZ    LSWLSVCN                                                         
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                  FIND TABLE ENTRY                             
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',HALF),RR=RELO40                   
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,R4           ESTABLISH ELEMENT                            
*                                                                               
         MVC   LSVKEY,ELTKEYNW     COPY NEW KEY                                 
         XC    LSVKEYNW,LSVKEYNW                                                
*                                                                               
LSWLSVCN DS    0H                                                               
*                                                                               
         LA    R5,LSVTABL(R5)      BUMP TO NEXT ITEM IN TABLE                   
         BCT   R0,LSWLSVLP                                                      
*                                                                               
LSWLSVDN DS    0H                                                               
*                                                                               
*        SET TABLE KEYS TO NEW ONES                                             
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         ICM   R0,15,BSPNOR        SKIP IF NO ENTRIES                           
         BZ    LSWTRSDN                                                         
*                                                                               
LSWTRSLP DS    0H                                                               
*                                                                               
         MVC   ELTKEY,ELTKEYNW     RESET TO NEW KEY                             
         XC    ELTKEYNW,ELTKEYNW                                                
*                                                                               
LSWTRSCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         BCT   R0,LSWTRSLP         LOOP IF NOT LAST ENTRY                       
*                                                                               
LSWTRSDN DS    0H                                                               
*                                                                               
*        RE-WRITE RECORD                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'PRTFILE',KEY+27,AREC,       X        
               (TERMNAL,DMWORK)                                                 
*                                                                               
         LA    RF,IOAREA           RESTORE IOAREA POINTER                       
         ST    RF,AREC                                                          
*                                                                               
LSWRTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
LSWTAD1E DS    0H                  RECORD FULL                                  
         LA    R2,RTSLI1H          A(FIRST FIELD)                               
         LHI   R3,ERRECFUL         ERROR CODE                                   
*                                                                               
LSWTADER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - CKADVC'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO DISPLAY AOR RATES                                 *         
*                                                                     *         
*NTRY    R3==> CONTRACT RATE ELEMENT                                  *         
*        R2==> FIRST FIELD ON LINE                                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKADVC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
         USING PRBELEM,R3          ESTABLISH CONTRACT RATE ELEMENT              
*                                                                               
*        READ AOR CONTRACT FOR AOR RATES                                        
*                                                                               
         CLI   AORCFND,2           SKIP IF I ALREADY READ AOR CONTRACT          
         BE    CKADVC30                                                         
*                                                                               
         XC    IOAREA(100),IOAREA  INIT WORKAREA                                
         MVC   IOAREA+32(64),KEY   SAVE CURRENT KEY AND KEYSAVE                 
*                                                                               
         MVC   PCONKAGY-PCONKEY+IOAREA,SADVDATA    AOR AGY ID                   
         MVC   PCONKMED-PCONKEY+IOAREA,SAVKMED     MEDIA                        
         MVI   PCONKRCD-PCONKEY+IOAREA,X'10'       RECORD ID                    
         MVC   PCONKCLT-PCONKEY+IOAREA,SADVDATA+2  ADV                          
         MVC   PCONKPUB-PCONKEY+IOAREA(6),SAVKPUB  PUB                          
*                                                                               
*        READ FOR PUBLINK PASSIVE IF NEEDED                                     
*                                                                               
         TM    SADVDATA+15,X'01'   IF PUB LINK REQUIRED                         
         BNO   CKADVC10                                                         
*                                                                               
         LA    RF,KEY                 ESTABLISH PUBLINK PASSIVE KEY             
         XC    KEY,KEY                                                          
         USING PUBAID,RF                                                        
*                                                                               
         MVI   PUBAID,PUBAIDQ      SET PUBLINK PASSIVE ID                       
         MVC   PUBAMED,SAVKMED     MEDIA                                        
         MVC   PUBAAGY,AGYALPHA    AGENCY                                       
         MVC   PUBAADV,SADVDATA+2  ADV                                          
         MVC   PUBAAOR,SADVDATA+0  AOR                                          
         MVC   PUBAAPUB,SAVKPUB    AGENCY PUB NUMBER                            
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PUBDIR',KEY,KEY,(TERMNAL,0)          
*                                                                               
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(PUBAVPUB-PUBAID),KEYSAVE  MATCH ON AGY PUB NUMBER            
         BNE   CKADVC1E                   ERROR                                 
*                                                                               
         MVC   PCONKPUB-PCONKEY+IOAREA,PUBAVPUB SAVE ADV PUB NUM                
*                                                                               
CKADVC10 DS    0H                                                               
*                                                                               
*        SWITCH TO AOR'S FILES                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14  AOR SE NUMBER                               
*                                                                               
         GOTO1 (RF),DMCB           SWITCH TO AOR'S FILES                        
*                                                                               
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   CKADVC2E                                                         
*                                                                               
         MVC   KEY,IOAREA          ADV CONTRACT KEY                             
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
*                                  READ CONTRACT RECORD                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY,(TERMNAL,0)          
*                                                                               
*        FIND CONTRACT COVERING DATE RANGE                                      
*                                                                               
CKADVCLP DS    0H                                                               
*                                                                               
         CLC   KEY(PCONNUM-PCONKEY),KEYSAVE   CHK AGY/MED/CLT/PUB               
         BNE   CKADVC3E            AOR CONTRACT NOT FOUND                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'PRTFIL',KEY+27,             X        
               APUBIO,(TERMNAL,0)       READ ADV CONTRACT INTO PUBIO            
*                                                                               
         L     RF,APUBIO           POINT TO FOUND CONTRACT                      
*                                                                               
         CLC   PCONSDT,PCONSDT-PCONREC(RF)      CHK DATES                       
         BL    CKADVCCN            AOR CONTRACT MUST COVER AGY                  
*                                  CONTRACT DATES COMPLETELY                    
         CLC   PCONEDT,PCONEDT-PCONREC(RF)                                      
         BH    CKADVCCN                                                         
*                                                                               
         B     CKADVCFD                                                         
*                                                                               
CKADVCCN DS    0H                                                               
*                                                                               
*                                  READ NEXT AOR CONTRACT                       
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'PRTDIR',KEY,KEY,(TERMNAL,0)          
*                                                                               
         B     CKADVCLP                                                         
*                                                                               
CKADVCFD DS    0H                                                               
*                                                                               
         MVI   AORCFND,3                 SET AOR CONTRACT JUST READ             
*                                                                               
CKADVC30 DS    0H                                                               
*                                                                               
         L     RF,APUBIO           POINT TO ADVERTISER CONTRACT                 
         LA    R5,33(RF)           POINT TO FIRST ELEMENT                       
*                                                                               
*        FIND FIRST RATE ELEMENT IN ADV CONTRACT                                
*                                                                               
         SR    RF,RF                                                            
*                                                                               
CKAPRBLP DS    0H                                                               
*                                                                               
         CLI   0(R5),0           CHECK FOR END OF RECORD                        
         BE    CKADVC4E          NEED TO FIND A RATE ELEMENT                    
*                                                                               
         CLI   0(R5),X'20'       FIND RATE ELEM                                 
         BNE   CKAPRBCN                                                         
*                                                                               
*                                IF EFD IN AOR ELEM                             
         OC    PRBDATE-PRBELEM(L'PRBDATE,R5),PRBDATE-PRBELEM(R5)                
         BZ    CKAPRB37               NONE - NO DATE RANGE CHECK                
*                                                                               
         OC    PRBDATE,PRBDATE      IF NO EFD IN MY ELEM                        
*        BNZ   CKAPRB36                MATCH EFD AGAINST CONTRACT RANGE         
*                                                                               
*                                      IF AOR EFFECTIVE AFTER MY END            
         CLC   PCONEDT,PRBDATE-PRBELEM(R5)                                      
         BL    CKAPRBCN                   IGNORE THIS ELEM                      
*                                                                               
         B     CKAPRB37                ELSE USE ELEMENT                         
*                                                                               
CKAPRB36 DS    0H                   ELSE IF AOR EFD AFTER MY EFD                
*                                                                               
         CLC   PRBDATE,PRBDATE-PRBELEM(R5)                                      
         BL    CKAPRBCN                  SKIP                                   
*                                                                               
CKAPRB37 DS    0H                                                               
         CLC   PRBLIND,PRBLIND-PRBELEM(R5)  SEE IF LEVEL IND MATCH              
         BNE   CKAPRBCN          NO - THEN IGNORE THIS ELEM                     
*                                                                               
         CP    PRBLEVEL,=P'-2'   IF LOOKING-UP LEVEL                            
         BNE   *+10                 COPY LEVEL FROM AOR CONTRACT                
         ZAP   PRBLEVEL,PRBLEVEL-PRBELEM(L'PRBLEVEL,R5)                         
*                                                                               
         CP    PRBRATE,=P'-2'    IF LOOKING-UP RATE                             
         BNE   CKADVC50                                                         
*                                                                               
         CLC   PRBDESC,PRBDESC-PRBELEM(R5)   MATCH DESCRIPTIONS                 
         BNE   CKAPRBCN                                                         
*                                                                               
         ZAP   PRBRATE,PRBRATE-PRBELEM(L'PRBRATE,R5) USE AOR RATE               
         MVC   PRBIND,PRBIND-PRBELEM(R5)             AND INDICATOR              
*                                                                               
CKADVC50 DS    0H                                                               
*                                                                               
         B     CKAPRBDN            RATES FOUND                                  
*                                                                               
CKAPRBCN DS    0H                                                               
*                                                                               
         IC    RF,1(R5)          BUMP TO NEXT ELEMENT IN CONTRACT REC           
         AR    R5,RF                                                            
         B     CKAPRBLP                                                         
*                                                                               
CKAPRBDN DS    0H                                                               
*                                                                               
*        END OF ROUTINE                                                         
*                                                                               
         CP    PRBLEVEL,=P'-2'     ERROR IF LEVEL OR RATE UNRESOLVED            
         BE    CKADVC4E                                                         
         CP    PRBRATE,=P'-2'                                                   
         BE    CKADVC4E                                                         
*                                                                               
         B     CKADVCX                                                          
*                                                                               
CKADVCX  DS    0H                                                               
*                                                                               
         CLI   AORCFND,2              IF I ALREADY HAD AOR CONTRACT             
         BE    CKADVCXX               DONE                                      
*                                                                               
         CLI   AORCFND,3              IF AOR CONTRACT JUST FOUND                
         BNE   *+8                                                              
         MVI   AORCFND,2              SET INDICATOR                             
*                                                                               
*        SWITCH BACK TO CURRENT SYSTEM                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  TROUBLE SWITCHING BACK                     
*                                                                               
CKADVCXX DS    0H                                                               
*                                                                               
         MVC   KEY(64),IOAREA+32     RESTORE KEY AND KEYSAVE                    
*                                    RESTORE FILE POINTERS                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
*                                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
CKADVC1E DS    0H                  PUB LINK NOT FOUND                           
         LHI   R3,ERPLNKNF                                                      
         B     CKADVCER                                                         
*                                                                               
CKADVC2E DS    0H                  AOR FILES NOT AVAILABLE                      
         LHI   R3,ERAORNAV                                                      
         B     CKADVCER                                                         
*                                                                               
CKADVC3E DS    0H                  NO AOR RATES FOR PERIOD.                     
         LHI   R3,ERAORNCN                                                      
         B     CKADVCE1                                                         
*                                                                               
CKADVC4E DS    0H                  NO AOR RATES FOR LEVEL/RATE                  
         LHI   R3,ERAORNRT                                                      
         B     CKADVCE1                                                         
*                                                                               
CKADVCE1 DS    0H                                                               
*                                                                               
*        SWITCH BACK TO CURRENT SYSTEM                                          
*                                                                               
         CLI   AORCFND,2              IF I ALREADY HAD AOR CONTRACT             
         BE    CKADVCER               DONE                                      
*                                                                               
         CLI   AORCFND,3              IF AOR CONTRACT JUST FOUND                
         BNE   *+8                                                              
         MVI   AORCFND,2              SET INDICATOR                             
*                                                                               
*        SWITCH BACK TO CURRENT SYSTEM                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                  TROUBLE SWITCHING BACK                     
*                                                                               
CKADVCER DS    0H                                                               
         B     ERREX                                                            
*                                                                               
ERPLNKNF EQU   538                 NO PUB LINK                                  
ERAORNAV EQU   539                 AOR FILES NOT AVAILABLE                      
ERAORNCN EQU   540                 NO AOR CONTRACTS FOR PERIOD                  
ERAORNRT EQU   541                 NO AOR RATES FOR LEVEL/RATE                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORDS - SUBROUTS'                          
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
*                                                                               
         USING T40DFFD,RA          ESTABLISH SCREEN                             
         USING GENOLD,RC,R7                                                     
         USING SUBROUTS,R9         ESTABLISH SUBROUTINES                        
         USING WRKSTRD,R8          ESTABLISH WORKING STORAGE                    
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
***********************************************************************         
*        GETEL MACRO                                                  *         
***********************************************************************         
         SPACE 2                                                                
         GETEL R4,33,ELCODE                                                     
*                                                                               
EDT      DS    0H                                                               
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R9),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R9)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
RTDIV    DS    0H                                                               
         LTR   RF,RF                                                            
         BP    *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
*                             DELETE ELEMS DISPLAYED                            
*                                                                               
DELELEM  NTR1                                                                   
*                                                                               
         LA    R2,PCONREC+33       POINT TO FIRST ELEMENT IN CONREC             
         USING PRBELEM,R2          ESTABLISH AS RATE ELEMENT                    
*                                                                               
         XC    ELPUT,ELPUT         INIT INSERTION POINT                         
*                                                                               
         SR    R0,R0                                                            
*                                                                               
DELLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R2),0             END OF RECORD                                
         BE    DELDONE                                                          
*                                                                               
         CLC   ELCODE,0(R2)        LOOK FOR RATE ELEMENT          L02           
         BNE   DELCONT                                                          
*                                                                               
         CP    PRBRATE,=P'0'       IF RATE SET TO ZERO                          
         BNE   *+10                                                             
         ZAP   PRBRATE,=P'-1'         SET TO -1 TO PREVENT LOOK-UP              
*                                                                               
         GOTO1 VRECUP,DMCB,(C'P',PCONREC),(R2)  DELETE ELEMENT                  
*                                                                               
         ST    R2,ELPUT            START INSERTING NEW ELEMENTS HERE            
*                                                                               
         B     DELCONT1                                                         
*                                                                               
DELCONT  DS    0H                                                               
*                                                                               
         IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
*                                                                               
DELCONT1 DS    0H                                                               
*                                                                               
         B     DELLOOP                                                          
*                                                                               
DELDONE  DS    0H                                                               
*                                                                               
         OC    ELPUT,ELPUT         IF INSETION POINT NOT KNOWN                  
         BNZ   *+8                                                              
         ST    R2,ELPUT            WHERE TO START ADDING RATE ELEMENTS          
*                                                                               
DELELEMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                  ADD ELEMENT TO RECORD                        
ADELEM   NTR1                                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(C'P',PCONREC),ELEMENT,(C'R',ELPUT)                  
*                                                                               
         SR    R0,R0                                                            
         L     R2,ELPUT            POINT TO INSERTION POINT                     
         IC    R0,1(R2)                                                         
         AR    R2,R0               POINT TO NEXT AREA IN RECORD                 
         ST    R2,ELPUT            UPDATE INSERTION POINT                       
*                                                                               
         CLI   DMCB+8,0            CHECK FOR RECORD OVERFLOW                    
*                                                                               
ADELEMX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MYTEMP   DS    CL20             USED IN RATE EDIT                               
*                                                                               
LEVADDR  DS    F                                                                
RATADDR  DS    F                                                                
CNSPACES DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'ERROR EXIT -ERREX'                                              
***********************************************************************         
*                                                                     *         
*        PUT OUT ERROR MESSAGE                                        *         
*                                                                     *         
*NTRY    R3    ERROR MESSAGE NUMBER                                   *         
*        R2    A(FIELD IN ERROR)                                      *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ERREX    NTR1  BASE=BASERB,LABEL=*  PUT OUT ERROR MESSAGE VIA GETTXT            
*                                                                               
*        SET CURSOR TO FIELD IN ERROR                                           
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         USING TIOBD,R1            ESTABLISH TIOBD                              
*                                                                               
         NI    FLDIIND,X'FF'-FINPVAL     INDICATE FIELD NOT VALIDATED           
         OI    FLDOIND,FOUTTRN     TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
*                                                                               
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
*                                                                               
         XC    TIOBCURI,TIOBCURI   CLEAR DISPLACEMENT INTO FIELD                
*                                                                               
         XC    GETTXTCB,GETTXTCB   INIT GETTXT CONTROL BLOCK                    
*                                                                               
         LA    R1,GETTXTCB         ESTABLISH GETTXT CONTROL BLOCK               
         USING GETTXTD,R1                                                       
*                                                                               
         MVI   GTMTYP,GTMERR       ERROR MESSAGE                                
*                                                                               
         LA    RF,KBAMSGH          SET A(OUTPUT AREA)                           
         STCM  RF,7,GTAOUT                                                      
*                                                                               
         STCM  R3,3,GTMSGNO        SET ERROR MESSAGE NUMBER                     
*                                                                               
         OI    FLDOIND,FOUTCUR     PLACE CURSOR HERE                            
*                                                                               
         GOTO1 VGETTXT,GETTXTD     PUT OUT ERROR MESSAGE                        
*                                                                               
         L     RD,SAVERD           POINT TO INCOMING REGISTERS                  
*                                                                               
         L     RD,4(RD)            BACK UP A LEVEL                              
         CLC   =C'0D45',0(RD)      FIND ENTRY REGISTERS                         
         BNE   *-10                                                             
*                                                                               
ERREXX   DS    0H                                                               
         XIT1                      GETS US OUT OF CONTRACT PROGRAM              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1  LABEL=*             BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORD - PFKEYS'                             
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT A LINE                                   *         
*              PF6  - DELETE A LINE                                   *         
*              PF9  - CASCADE DATA                                    *         
*                                                                     *         
*NTRY    R5 ==>  LINUP CONTROL BLOCK                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1  LABEL=*                                                          
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLC   KBAACT(3),=C'ADD'   IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  KBAACTH,=C'CHAR'       CHANGE TO 'CHANGE'                        
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
         L     R4,=A(LSVTAB-T40DFFD) POINT OT START OF LINUP SAVEAREA           
         LA    R4,T40DFFD(R4)                                                   
         USING LSVTABD,R4          ESTABLISH LINUP SAVEAREA ENTRY               
*                                                                               
*        FIND LINE WITH CURSOR                                                  
*                                                                               
         CLC   TIOBCURD,0(R6)      MUST BE AFTER START OF LINE                  
         BL    PFKEYSX                                                          
         CLC   TIOBCURD,2(R6)      AND BEFORE START OF NEXT LINE                
         BL    *+16                                                             
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         B     *-28                                                             
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD A LINE                                
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNDEL                  DELETE A LINE                             
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORD - LNADD'                              
***********************************************************************         
*                                                                     *         
*        ADD A LINE                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         CLC   KBAACT(3),=C'DIS'   CANNOT BE DISPLAYING                         
         BE    PFKEYS1E                                                         
*                                                                               
         LA    R4,NLINS            NUMBER OF LINES ON SCREEN                    
         BCTR  R4,0                DECREMENT FOR INDEXING                       
         BCTR  R4,0                DECREMENT FOR NEXT TO LAST ENTRY             
         MH    R4,=Y(LSVTABL)      DISP  TO NEXT TO LAST IN LINUP SAVE          
         L     RE,=A(LSVTAB-T40DFFD) POINT OT START OF LINUP SAVEAREA           
         LA    RE,T40DFFD(RE)                                                   
         LA    R4,0(R4,RE)         POINT TO NEXT TO LAST IN LINUP SAVE          
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R5,R6                                                            
         SH    R5,=H'2'            BACK UP A TABLE ENTRY                        
         SR    R1,R1                                                            
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
         SR    R2,R2                                                            
*                                                                               
LNADDLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADDLP1                                                      
*                                                                               
LNADDDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD+LSVTABL(LSVTABL),LSVTABD  COPY LINUP SAVE                
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         SH    R4,=Y(LSVTABL)      BACK UP LINUP SAVE ENTRY                     
*                                                                               
         LR    R6,R5               BACK UP ENTRIES IN TABLE                     
         SH    R5,=H'2'                                                         
*                                                                               
         CLC   0(2,R5),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         AH    R4,=Y(LSVTABL)      POINT TO DATA FOR CURSOR LINE                
         XC    LSVTABD(LSVTABL),LSVTABD  CLEAR LINUP SAVE ENTRY                 
*                                                                               
         LH    R6,0(R6)            POINT TO FIRST OF CURSOR LINE                
         LA    R6,0(R6,RA)                                                      
*                                                                               
         ST    R6,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNADXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
*******  NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
LNADXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCON45 - PBL FILE - INSTRUCTION RECORD - LNDEL'                
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
*NTRY    R6==> TABLE ENTRY FOR LINE WITH CURSOR                       *         
*        R4==> LINUP TABLE ENTRY FOR LINE WITH CURSOR                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  DELETE LINE AT CURSOR                        
*                                                                               
         CLC   KBAACT(3),=C'DIS'   CANNOT BE DISPLAYING                         
         BE    PFKEYS1E                                                         
*                                                                               
         LH    R5,0(R6)            POINT TO FIRST FIELD OF CURSOR LINE          
         LA    R5,0(R5,RA)                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         TM    1(R5),X'20'         IF PROTECTED FIELD                           
         BNO   *+16                                                             
         IC    RF,0(R5)                                                         
         LA    R5,0(RF,R5)            BUMP TO NEXT FIELD                        
         B     *-16                                                             
*                                                                               
         ST    R5,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
*        FIND DATA FOR THIS LINE                                                
*                                                                               
         OC    LSVTABD(LSVTABL),LSVTABD    SKIP IF NO DATA ON LINE              
         BZ    LNDELD10                                                         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R4)),RR=RELO40                   
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,BSPAREC          POINT TO FOUND ELEMENT                       
         OI    ELTCTL-ELTABD(R1),ELTDELQ   FLAG FOR DELETE                      
*                                                                               
         XC    LSVTABD(LSVTABL),LSVTABD   CLEAR TABLE ENTRY                     
*                                                                               
LNDELD10 DS    0H                                                               
*                                                                               
         LR    R5,R6                                                            
         AH    R5,=H'2'            POINT TO NEXT TABLE ENTRY                    
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R5),=X'FFFF'    STOP IF PASSED END OF TABLE                  
         BNL   LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
*                                                                               
LNDELLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE UP ONE                             
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         NI    4(RF),X'FF'-X'20'   REMOVE PREVIOUSLY VALIDATED SWITCH           
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDELLP1                                                      
*                                                                               
LNDELDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD(LSVTABL),LSVTABD+LSVTABL  COPY LINUP SAVEAREA            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         LR    R6,R5               ADVANCE ENTRIES IN TABLE                     
         AH    R5,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNDLXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
******   NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
         OI    4(R6),X'20'         SET AS VALIDATED                             
*                                                                               
LNDLXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDLXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCON45 - PUB LIST POSTING - PFKEYS - EXITS'                    
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYS1E DS    0H                  MUST BE IN UPDATE MODE                       
*                                                                               
         LA    R2,KBAACTH          CURSOR TO ACTION FIELD                       
         LHI   R3,ERACTUPD         MUST BE IN UPDATE MODE                       
         B     ERREX                                                            
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
ERACTUPD EQU   542                 MUST BE IN UPDATE MODE                       
*                                                                               
         TITLE 'PPCON45 - PUB LIST POSTING - LNTBL'                             
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNTBL    DS    0D                                                               
         DC    Y(RTSLI1H-T40DFFD)   RATE 1                                      
         DC    Y(RTSLI2H-T40DFFD)   RATE 2                                      
         DC    Y(RTSLI3H-T40DFFD)   RATE 3                                      
         DC    Y(RTSLI4H-T40DFFD)   RATE 4                                      
         DC    Y(RTSLI5H-T40DFFD)   RATE 5                                      
         DC    Y(RTSLI6H-T40DFFD)   RATE 6                                      
         DC    Y(RTSLI7H-T40DFFD)   RATE 7                                      
         DC    Y(RTSLI8H-T40DFFD)   RATE 8                                      
         DC    Y(RTSLI9H-T40DFFD)   RATE 9                                      
         DC    Y(RTSLIAH-T40DFFD)   RATE 10                                     
         DC    Y(RTSLIBH-T40DFFD)   RATE 11                                     
         DC    Y(RTSLICH-T40DFFD)   RATE 12                                     
         DC    Y(RTSLIDH-T40DFFD)   RATE 13                                     
LNTBLLS  DC    Y(RTSLILH-T40DFFD)   RATE LAST                                   
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
T40DFFD  DSECT                                                                  
         ORG   T40DFFD+X'1800'                                                  
         DS    0D                                                               
SVKEY    DS    XL(L'PCONKEY)       LAST TIME KEY                                
SAVACT   DS    CL4                 ACTION SAVEAREA                              
         DS    CL4                 SPARE                                        
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
*                                                                               
         TITLE 'PPCON45 - CONTRACT RATES SCROLL - WRKSTRD'                      
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WRKSTRD  DSECT                                                                  
*                                                                               
FLD      DS    CL80                WORK AREA                                    
ACURFORC DS    A                   A(FIELD WITH CURSOR)                         
VGETTXT  DS    V                   V(GETTXT)                                    
ELEMENT  DS    XL256               WORKAREA                                     
SAVERD   DS    F                   REGISTER RD SAVEAREA                         
*                                                                               
SVDIR    DS    X                   LINUP DIRECTION SAVEAREA                     
SVELTKEY DS    XL2                 KEY SAVEAREA                                 
*                                                                               
GETTXTCB DS    XL(L'GTBLOCK)       GETTEXT CONTROL BLOCK                        
*                                                                               
*                                                                               
HELPSAVE DS    XL512               HELP CONTROL BLOCK                           
ELTMAX   EQU   100                                                              
TYPETRAN DC    X'00'         MAY HAVE TO BE IN TWA                              
ELSAVE   DC    X'00'         MAY HAVE TO BE IN TWA                              
ELCODE   DC    X'00'         ELEMENT CODE                          L02          
NODISP   DC    X'00'  IF ON NO X'21' OR 22'  DISPLAY SPACE OF 20   L02          
         DS    0D                                                               
*                                                                               
         EJECT                                                                  
NFLDS    DS    X                   NUMBER OF FIELDS ON LINE OF SCREEN           
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
WRKELTAB DS    XL256               WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
*                                                                               
LASTLIN  DS    XL1                 NUMBER OF LAST USED LINE                     
CURLIN   DS    XL1                 NUMBER OF CURRENT   LINE                     
LINCT    DS    PL2                                                              
ALINCUR  DS    XL2                 DISP OF LINE WITH CURSOR IN TWA              
         DS    XL2                                                              
VLINUP   DS    V                   V(LINUP)                                     
VSWITCH  DS    V                   V(SWITCH)                                    
*                                                                               
         DS    0F                                                               
       ++INCLUDE PRVALPARMS                                                     
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
HELPCBLK DS    XL256               HELP CONTROL BLOCK                           
         DS    0D                                                               
LUBLK    DS    XL256               LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL(25*2)                                                         
SVLSVTAB DS    XL(25*10)           HOLD COPY OF LINUP SAVE TABLE                
         DS    0D                                                               
*                                                                               
ELTAB    DS    XL(ELTABL*ELTMAX)      TABLE FOR SPACE/RATE ELEMS                
*                                                                               
WRKSTRLQ EQU   *-WRKSTRD           LENGTH OF WORKING STORGAE                    
*                                                                               
         EJECT                                                                  
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL2                 SORT VALUE - LINE # AND LINE SUB #           
LSVKEYL  EQU   *-LSVTABD                                                        
LSVKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL2                 SORT VALUE - LINE # AND LINE SUB #           
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(42)              CONTRACT RATE ELEMENT                        
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         TITLE 'PPCON45 - CONTRACT RATES SCROLL - SCREEN'                       
***********************************************************************         
*                                                                     *         
*        RATES SCREEN                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T40DFFD  DSECT                                                                  
         ORG   KBALAST                                                          
       ++INCLUDE PPCONFBD                                                       
*                                                                               
         TITLE 'PPCON45 - CONTRACT RATES SCROLL - INCLUDE'                      
***********************************************************************         
*                                                                     *         
*        INCLUDED DSECTS                                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE PRGLOBEQUS                                                     
*NOP*****INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDLINUPD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PRHELPCB                                                       
       ++INCLUDE PRVALTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
PPRDRECD DSECT                     PRODUCT RECORD                               
         PRINT OFF                                                              
       ++INCLUDE PPRDREC                                                        
         PRINT ON                                                               
*                                                                               
PUBAKEYD DSECT                                                                  
PUBAIDQ  EQU   X'FE'               RECORD ID                                    
       ++INCLUDE PPPUBADVPP                                                     
*                                                                               
         PRINT OFF                                                              
*NOP*****INCLUDE FATIOB                                                         
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PPCON45   11/05/03'                                      
         END                                                                    
