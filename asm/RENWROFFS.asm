*          DATA SET RENWROFFS  AT LEVEL 142 AS OF 10/07/02                      
*PHASE T00AA9B,*                                                                
*INCLUDE KHDUMMY                                                                
*          TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES'                     
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES'                     
***********************************************************************         
*                                                                     *         
*          TITLE  T000AA9 - REPPAK WRITER OFFLINE ROUTINES            *         
*  COMMENTS: REPPAK WRITER OFFLINE ROUTINES                           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG, KEY DSECT POINTER, GETEL REG                *         
*          R5 - POINTER TO VLPARMS                                    *         
*          R6 - SPARE                                                 *         
*          R7 - POINTER TO RENWRIOD                                   *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN OFFLINE ROUTINES (RENWR00-T23000)   *         
*                  - IN AIO FROM HYPER OFFLINE ROUTINES (T00A30)      *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*   *** NOTE *** PROGRAM USES NENETDATES TO CALCULATE DATELIST        *         
*                                                                     *         
*                                                                     *         
*  HISTORY OF CHANGES                                                 *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - INIT'                
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RENWROFF CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NWROFF**,CLEAR=YES                                             
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL WORKAREA                     
*                                                                               
         LA    RC,SPOOLEND         ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         ST    RD,COMMRD                                                        
*                                                                               
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          ESTABLISH WRITER WORKING STORAGE             
*                                                                               
         LA    R7,RENWRIOD                                                      
         USING RENWRIOD,R7                                                      
*                                                                               
         L     RA,ATWA                                                          
         USING T830FFD,RA                                                       
*                                                                               
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES '                      
***********************************************************************         
*                                                                     *         
*        SYSTEM ROUTINES ENTERABLE FORM BASE OR OVERLAY               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         SRL   RF,24                                                            
         CLM   RF,1,=AL1((BRMAX-BRANCH)/4)                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         LA    RF,BRANCH(RF)       POINT TO ENTRY IN BRANCH TABLE               
*                                                                               
         CLI   0(RF),0             IF IT IS NOT AN ADDRESS                      
         BNE   0(RF)                  GO TO BRANCH STATEMENT                    
*                                                                               
         L     RF,0(RF)            ELSE GO TO THAT ADDRESS                      
         BASR  RE,RF                                                            
         B     XIT                                                              
*                                                                               
BRANCH   DS    0A                                                               
         DC    A(VTRACDRN)         TRACE DRONE                                  
         DC    A(VDOFLOW)          FLOW CHARTING                                
         DC    A(VINIDRV)          INIT DRIVER                                  
         DC    A(VGENHEAD)         GENERATE HEADLINES                           
         DC    A(0)                RESERVED FOR GENFOOT                         
         DC    A(VDLSTBLD)         BUILD DATE LIST                              
BRMAX    EQU   *                                                                
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - VTRACDRN'            
***********************************************************************         
*                                                                     *         
*        TRACE DRONE                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VTRACDRN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   VTRACDRX                                                         
*                                                                               
         L     R3,ADPGPROG                                                      
*                                                                               
TRACD2   CLI   0(R3),0                                                          
         BE    VTRACDRX                                                         
*                                                                               
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
*                                                                               
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
*                                                                               
         B     TRACD2                                                           
*                                                                               
VTRACDRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - VINIDRV'             
***********************************************************************         
*                                                                     *         
*              INITIALIZE TO RUN DRIVER                               *         
*                                  LOADS PHASES                       *         
*                                  SETS GLOBAL ADDRESSES              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VINIDRV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,X'9A000000',0,0  LOAD T8309A(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AA8'   LOAD T00AA8 (RENWRIO)                
         L     RE,DMCB                                                          
         ST    RE,VRWIO                                                         
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AAA'   LOAD T00AAA (REPPAK DRIVER)          
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
*                                                                               
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLDOWNLD,SPECOPT                                                 
         MVC   GLDWNLD2,SPECOPT2                                                
         MVC   GLDLCHAR,SPECCHAR                                                
         MVI   SPECOPT,0           CLEAR                                        
         MVI   SPECOPT2,0          CLEAR                                        
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
         MVC   GLRNKMAX,MAXRANK                                                 
         MVI   GLNORBOX,X'40'       TURN OFF ROW BOXES FOR TOTALS               
*                                                                               
         ZIC   RE,GLFHEADL                                                      
         LA    RE,3(RE)                                                         
         LA    RF,14                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         STC   RE,GLLHEADL                                                      
         MVI   GLDETHED,C'Y'        I GET HEADS AT DETAIL TIME                  
*                                                                               
         CLI   DOWNOPT,C'Y'         OPTION TO DOWNLOAD                          
         BNE   *+8                                                              
         OI    GLDOWNLD,X'80'                                                   
*                                                                               
         MVC   GLINDS,DRINDS        PASS THROUGH DRIVER INDICATORS              
         MVC   GLINDS2,REQGLOPS     PASS THROUGH DRIVER INDICATORS              
         MVI   REQGLOPS,0                                                       
*                                                                               
         CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
*                                                                               
*              INITIALIZATION OF PRINT RELATED FIELDS                           
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BE    IDRWIDE                                                          
*                                                                               
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         L     R1,=A(REGSPECS)                                                  
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   *+8                                                              
         L     R1,=A(NARSPECS)                                                  
*                                                                               
         ST    R1,SPECS                                                         
         B     VINIDRVX                                                         
*                                                                               
IDRWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         L     R1,=A(WIDSPECS)                                                  
         ST    R1,SPECS                                                         
*                                                                               
VINIDRVX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - REGSPECS'          
***********************************************************************         
*                                                                     *         
*              PRINTING SPECS                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REGSPECS DS    0C                                                               
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,112,PAGE                                                      
         SSPEC H2,100,RUN           SPECS FOR REGULAR PRINTING                  
         DC    X'00'                                                            
         SPACE                                                                  
NARSPECS DS    0C                                                               
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,60,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,60,RUN           SPECS FOR NARROW PRINTING                    
         DC    X'00'                                                            
         SPACE                                                                  
WIDSPECS DS    0C                                                               
         WSPEC H1,2,AGYNAME                                                     
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,REPORT                                                    
         WSPEC H1,142,PAGE                                                      
         WSPEC H2,129,RUN            SPECS FOR WIDE PRINTING                    
         DC    X'00'                                                            
*                                                                               
         TITLE 'RENWRGEN - VALIDATE COLUMN FILTERS - VDLSTBLD'                  
***********************************************************************         
*                                                                     *         
*        BUILD DATELISTS                                              *         
*                                                                     *         
*        - GENERATE MONTH/QUARTER/YEAR DATE LISTS                     *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
*EXIT                                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VDLSTBLD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         L     R4,=A(DATEAREA)     ESTABLISH DATELIST AREA                      
         USING DATELSTD,R4                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,REQPSTRB),(2,DLSTPSTC) COMPRESSED START           
*                                                                               
         LR    R0,R4               CLEAR DATELIST AREA                          
         LHI   R1,DATELSTL                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    R4,ADATLIST         RETURN ADDRESS                               
         ST    R4,REDLSTA          RETURN ADDRESS                               
*                                                                               
*        BUILD LIST OF 50 MONTHS                                                
*                                                                               
         XC    PERTYPE,PERTYPE     INIT PERIOD TYPE                             
*                                                                               
         MVI   PERTYPE,C'W'                                                     
         MVI   PERTYPE+2,1                                                      
*                                                                               
         LA    R1,104              START WITH 104 WEEKS LIST                    
         ST    R1,NMONTHS                                                       
*                                                                               
         GOTO1 =A(WKLST),DMCB,NMONTHS,WEEKLIST,PERTYPE                          
*                                                                               
*        BUILD LIST OF 104 WEEKS                                                
*                                                                               
         XC    PERTYPE,PERTYPE     INIT PERIOD TYPE                             
*                                                                               
         MVI   PERTYPE,C'M'                                                     
         MVI   PERTYPE+2,1                                                      
*                                                                               
         LA    R1,50               START WITH 50 MONTHS LIST                    
         ST    R1,NMONTHS                                                       
*                                                                               
         GOTO1 =A(WKLST),DMCB,NMONTHS,MNTHLIST,PERTYPE                          
*                                                                               
*        BUILD LIST OF QUARTERS                                                 
*                                                                               
*        QUARTERS START IN JAN,APR,JUL,OCT                                      
*              EXCEPTION IF REQUEST STARTS IN SEPTEMBER                         
*              THEN FALL QUARTER STARTS WITH REQUEST                            
*                                                                               
         LA    R2,MNTHLIST         A(MONTH LIST)                                
         LA    R0,50               MAX 50 MONTHS                                
         LA    R3,QURTLIST         A(QUARTER LIST)                              
         LA    R6,16               MAX 16 QUARTERS                              
*                                                                               
*                                                                               
         MVI   YTDADJQ,0           INIT YTD ADJUST TO DATELIST START            
         SR    R5,R5               INIT ADJUSMENT COUNTER                       
*                                                                               
         MVC   0(2,R3),0(R2)       1ST QTR STARTS WITH START OF 1ST             
*                                  MONTH IN REQUEST MONTH LIST                  
DLBQTRLP DS    0H                                                               
*                                                                               
*        SET CURRENT QUARTER END TO THAT OF CURRENT MONTH                       
*              IN MONTH LIST                                                    
*                                                                               
         MVC   2(2,R3),2(R2)       MOVE IN END OF CURRENT MONTH                 
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT MONTH                           
         BCT   R0,*+8              DONE IF LAST MONTH IN LIST                   
         B     DLBQTRDN                                                         
*                                                                               
         CLI   0(R2),0                                                          
         BE    DLBQTRDN                                                         
*                                                                               
         GOTO1 =A(GETBM)           GET BROACAST MONTH MUMBER                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORK+7           (RETURNED BY GETBM IN WORK+7)                
*                                                                               
*        CHECK IF START MONTH OF A QUARTER                                      
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'3'            IF MONTH IS 1 4 7 10                         
*                                                                               
         CHI   RE,1                   REMAINDER WILL BE 1                       
         BNE   DLBQTRCN                                                         
*                                                                               
         CHI   R6,16               IF NOT FIRST PASS IGNORE SEP. CHECK          
         BNE   DLBQTR4                                                          
*                                                                               
         CLI   WORK+7,10           IF THIS IS OCTOBER                           
         BNE   DLBQTR4                                                          
*                                                                               
         CLC   REQPSTR+2(2),=C'09'    AND REQ. STARTED IN SEPTEMBER             
         BE    DLBQTRCN            DON'T BREAK                                  
*                                                                               
DLBQTR4  DS    0H                                                               
*                                                                               
*                                  IF YTD OPTION IN EFFECT                      
*                                  LIST STARTS AT YTD START                     
*                                  AND ADJUSTMNT COUNTS ENTRIES BETWEEN         
*                                  YTD START AND REQUEST START                  
*                                                                               
         CLC   0(2,R2),DLSTPSTC    IF PERIOD START NOT REACHED                  
         BH    *+12                                                             
         LA    R5,1(R5)               BUMP ADJUSTMENT COUNTER                   
         STC   R5,YTDADJQ             RESET YTD ADJUSTMENT FACTOR               
*                                                                               
         LA    R3,4(R3)               AND WE HAVE A NEW QUARTER                 
         BCT   R6,*+8                                                           
         B     DLBQTRDN                                                         
*                                                                               
         MVC   0(2,R3),0(R2)          MOVE IN START                             
*                                                                               
DLBQTRCN DS    0H                                                               
*                                                                               
         B     DLBQTRLP                                                         
*                                                                               
DLBQTRDN DS    0H                                                               
*                                                                               
*                                                                               
*        BUILD LIST OF SEMI-YEARS                                               
*                                                                               
*        SEMI-YEARS START IN JAN,JUL                                            
*                                                                               
         LA    R2,MNTHLIST         A(MONTH LIST)                                
         LA    R0,50               MAX 25 MONTHS                                
         LA    R3,SEMILIST         A(SEMI-ANNUAL LIST)                          
         LA    R6,8                MAX 4 SEMI-YEARS                             
*                                                                               
         MVI   YTDADJS,0           INIT YTD ADJUST TO DATELIST START            
         SR    R5,R5               INIT ADJUSMENT COUNTER                       
*                                                                               
         MVC   0(2,R3),0(R2)       1ST SEMI STARTS WITH START OF 1ST            
*                                  MONTH IN REQUEST MONTH LIST                  
*                                                                               
DLBSMILP DS    0H                                                               
*                                                                               
*        SET CURRENT SEMI END TO THAT OF CURRENT MONTH                          
*              IN MONTH LIST                                                    
*                                                                               
         MVC   2(2,R3),2(R2)       MOVE IN END OF CURRENT MONTH                 
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT MONTH                           
         BCT   R0,*+8              DONE IF LAST MONTH IN LIST                   
         B     DLBSMIDN                                                         
*                                                                               
         CLI   0(R2),0                                                          
         BE    DLBSMIDN                                                         
*                                                                               
         GOTO1 =A(GETBM)           GET BROACAST MONTH MUMBER                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORK+7           (RETURNED BY GETBM IN WORK+7)                
*                                                                               
*        CHECK IF START MONTH OF A SEMI                                         
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'6'            IF MONTH IS 1 7                              
*                                                                               
         CH    RE,=H'1'               REMAINDER WILL BE 1                       
         BNE   DLBSMICN                                                         
*                                                                               
*                                  IF YTD OPTION IN EFFECT                      
*                                  LIST STARTS AT YTD START                     
*                                  AND ADJUSTMNT COUNTS ENTRIES BETWEEN         
*                                  YTD START AND REQUEST START                  
*                                                                               
         CLC   0(2,R2),DLSTPSTC    IF PERIOD START NOT REACHED                  
         BH    *+12                                                             
         LA    R5,1(R5)               BUMP ADJUSTMENT COUNTER                   
         STC   R5,YTDADJS             RESET YTD ADJUSTMENT FACTOR               
*                                                                               
         LA    R3,4(R3)               AND WE HAVE A NEW QUARTER                 
         BCT   R6,*+8                                                           
         B     DLBSMIDN                                                         
*                                                                               
         MVC   0(2,R3),0(R2)          MOVE IN START                             
*                                                                               
DLBSMICN DS    0H                                                               
*                                                                               
         B     DLBSMILP                                                         
*                                                                               
DLBSMIDN DS    0H                                                               
*                                                                               
         MVC   YEARLIST(2),MNTHLIST    1ST YEAR STARTS WITH 1ST MONTH           
*                                                                               
         LA    R3,MNTHLIST+44      POINT TO END MONTH OF YEAR                   
*                                                                               
         MVC   YEARLIST+2(2),2(R3) LAST MONTH OF THE YEAR                       
*                                                                               
         LA    R3,4(R3)            START OF NEXT YEAR                           
*                                                                               
         MVC   YEARLIST+4(2),0(R3)                                              
*                                                                               
         LA    R3,44(R3)           LAST MON IN 2ND YEAR                         
         MVC   YEARLIST+6(2),2(R3)                                              
*                                                                               
         LA    R3,4(R3)            START OF NEXT YEAR                           
*                                                                               
         MVC   YEARLIST+8(2),0(R3)                                              
*                                                                               
         LA    R3,44(R3)           LAST MON IN 3RD YEAR                         
         MVC   YEARLIST+10(2),2(R3)                                             
*                                                                               
         LA    R3,4(R3)            START OF NEXT YEAR                           
*                                                                               
         MVC   YEARLIST+12(2),0(R3)                                             
*                                                                               
         LA    R3,44(R3)           LAST MON IN 4TH YEAR                         
         MVC   YEARLIST+14(2),2(R3)                                             
*                                                                               
BDEX     DS    0H                                                               
*                                                                               
* - CONVERT 2 BYTE COMPRESSED Y/M/D/ QUARTERS TO Y/M QUARTERS                   
*                                                                               
         LA    R2,QURTLIST                                                      
         LA    R0,16                                                            
*                                                                               
YMQLOOP  GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)   CONVERT TO YYMMDD               
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'10'     ADD 10 TO GET B MONTH           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,WORK)  YMD OF B MONTH                  
*                                                                               
         MVC   0(2,R2),WORK        SET Y/M OF QUARTER START                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,WORK)   CONVERT TO YMD                  
*                                                                               
         MVC   2(2,R2),WORK        SET Y/M OF QUARTER END                       
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT QUARTER                         
*                                                                               
         OC    0(2,R2),0(R2)       ARE WE FINISHED                              
         BZ    *+8                                                              
         BCT   R0,YMQLOOP                                                       
*                                                                               
* - CONVERT 2 BYTE COMPRESSED Y/M/D/ SEMIS TO Y/M SEMIS                         
*                                                                               
         LA    R2,SEMILIST                                                      
         LA    R0,8                                                             
*                                                                               
YMSLOOP  GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)   CONVERT TO YYMMDD               
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'10'     ADD 10 TO GET B MONTH           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,WORK)  YMD OF B MONTH                  
*                                                                               
         MVC   0(2,R2),WORK        SET Y/M OF SEMIS START                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,WORK)   CONVERT TO YMD                  
*                                                                               
         MVC   2(2,R2),WORK        SET Y/M OF SEMIS END                         
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT SEMI                            
*                                                                               
         OC    0(2,R2),0(R2)       ARE WE FINISHED                              
         BZ    *+8                                                              
         BCT   R0,YMSLOOP                                                       
*                                                                               
* - CONVERT 2 BYTE COMPRESSED Y/M/D/ YEARS    TO Y/M YEARS                      
*                                                                               
         LA    R2,YEARLIST                                                      
         LA    R0,4                                                             
*                                                                               
YMYLOOP  GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)   CONVERT TO YYMMDD               
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'10'     ADD 10 TO GET B MONTH           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,WORK)  YMD OF B MONTH                  
*                                                                               
         MVC   0(2,R2),WORK        SET Y/M OF YEAR START                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,WORK)   CONVERT TO YMD END DATE         
*                                                                               
         MVC   2(2,R2),WORK        SET Y/M OF YEAR END                          
*                                                                               
YMYCONT  DS    0H                                                               
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT YEAR                            
*                                                                               
         OC    0(2,R2),0(R2)       ARE WE FINISHED                              
         BZ    *+8                                                              
         BCT   R0,YMYLOOP                                                       
*                                                                               
*        BUILD LIST OF BROADCAST MONTHS IN Y/M BINARY FORMAT                    
*                                                                               
         LA    R2,MNTHLIST                                                      
         LA    R3,MNTHLSTB                                                      
         LA    R0,50                                                            
         MVI   YTDADJM,0           INIT YTD ADJUST TO DATELIST START            
         SR    R5,R5               INIT ADJUSMENT COUNTER                       
*                                                                               
YMMBLOOP DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,WORK)   CONVERT TO YMD                  
*                                  END OF MONTH ALWAYS IN BRDCAST MONTH         
*                                                                               
         MVC   0(2,R3),WORK        SAVE YM                                      
         MVC   2(2,R3),WORK        SAVE YM                                      
*                                                                               
         CLC   0(2,R3),REYMSTRP    IF PERIOD START REACHED                      
         BNE   *+8                                                              
         STC   R5,YTDADJM             SAVE YTD ADJUTMENT FACTOR                 
*                                  IF YTD OPTION IN EFFECT                      
*                                  MONTH LIST STARTS AT YTD START               
*                                  AND ADJUSTMNT COUNTS ENTRIES BETWEEN         
*                                  YTD START AND REQUEST START                  
*                                                                               
YMMBCONT DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP YTD ADJUSTMENT FACTOR                   
         LA    R2,4(R2)            BUMP TO NEXT MONTH                           
         LA    R3,4(R3)            BUMP TO NEXT MONTH                           
*                                                                               
         OC    0(2,R2),0(R2)       ARE WE FINISHED                              
         BZ    YMMBDONE                                                         
*                                                                               
         BCT   R0,YMMBLOOP                                                      
*                                                                               
YMMBDONE DS    0H                                                               
*                                                                               
*        BUILD LIST OF BROADCAST WEEKS IN Y/M/D BINARY FORMAT                   
*                                                                               
         LA    R2,WEEKLIST                                                      
         LA    R3,WEEKLSTB                                                      
         LA    R0,208                                                           
*                                                                               
YMDBLOOP DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(3,0(R3))  CONVERT TO YMD                  
*                                                                               
YMDBCONT DS    0H                                                               
*                                                                               
         LA    R2,2(R2)            BUMP TO NEXT WEEK                            
         LA    R3,3(R3)            BUMP TO NEXT WEEK                            
*                                                                               
         OC    0(2,R2),0(R2)       ARE WE FINISHED                              
         BZ    YMDBDONE                                                         
*                                                                               
         BCT   R0,YMDBLOOP                                                      
*                                                                               
YMDBDONE DS    0H                                                               
*                                                                               
DLSTBLDX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
PERTYPE  DS    CL3                                                              
NMONTHS  DS    F                                                                
NQUARTS  DS    F                                                                
DLSTPSTC DS    XL2                 COMPRESSED REQUEST START DATE                
*                                                                               
         DS    0D                  ALIGNMENT                                    
DATEAREA DS    XL(DATELSTL)        UP TO 25 MONTHS                              
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'RENWRGEN - VALIDATE COLUMN FILTERS - GETBM'                     
***********************************************************************         
*                                                                     *         
*        FIND BROADCAST MONTH FOR A DATE                              *         
*                                                                     *         
*NTRY          INPUT               R2=A(COMPRESSED DATE)              *         
*                                                                     *         
*EXIT          OUTPUT              RETURN YYMMDD EBCDIC IN WORK       *         
*                                         YMD    BINARY IN WORK+6     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETBM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                  TRANSLATE DATE TO YYMMDD                     
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
*                                                                               
*        FIND DATE OF SUNDAY OF DATE'S WEEK                                     
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+6   FIND DAY OF WEEK                       
*                                                                               
         SR    R4,R4                                                            
         IC    R4,DMCB             DAY OF WEEK MON=1, SUN=7                     
*                                                                               
         LA    R5,7                (DEFAULT IS MONDAY)                          
*                                                                               
         SR    R5,R4               # OF DAYS TO ADD TO DATE                     
         BZ    GETBM6                 SUNDAY DATE                               
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R5)  GET NEXT SUNDAY'S DATE              
*                                  IT WILL BE IN DAY'S BROADCAST MONTH          
         MVC   WORK(6),WORK+6      USE SUNDAY DATE                              
*                                                                               
GETBM6   DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)  BINARY FORMAT                   
*                                                                               
GETBMX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - WKLST'               
***********************************************************************         
*                                                                     *         
*              ROUTINE TO CREATE A WEEKLIST                           *         
*                                                                     *         
*        - GENERATE MONTH/QUARTER/YEAR DATE LISTS                     *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
*              PARAMETERS          1 (R3) A(FULL WORD N'PERIODS)      *         
*                                  2 (R4) A(OUTPUT LIST AREA)         *         
*                                  3 (R5) BYTE 1 W/M/Q                *         
*                                         BYTE 2 NON ZERO = USE MONTHS*         
*                                         BYTE 3 NON ZERO = USE QUARTER         
*              INPUT               NREPTYP ACCOUNTING/MEDIA           *         
*                                          USED TO DETERMINE MONTH TYPE         
*                                  USES NBUSER FOR BROAD/CAL/SPECIAL  *         
*                                  USES 00 AND B3 PROFILES FOR SPECIAL*         
*                                  REQCMPST REQCMPND FOR PERIOD       *         
*EXIT                                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
WKLST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         LM    R3,R5,0(R1)         SAVE ARGS                                    
*                                                                               
         GOTO1 =A(WKLUTIL)           ROUTINE WHICH GETS LIST                    
*                                                                               
WKLSTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - WKLUTIL'             
***********************************************************************         
*                                                                     *         
*              WORK ROUTINE CALLED ABOVE                              *         
*                                                                     *         
*              INPUT               R3=N'PERIODS                       *         
*                                  R4=A(OUTPUT LIST)                  *         
*                                  R5=A(TYPE - SEE ABOVE)             *         
*                                                                     *         
*              LOCAL               DUB=DOW/TYPE/FISC MD               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
WKLUTIL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         XC    DUB,DUB             INIT WORKAREA                                
         LA    R2,1                DEFAULT TO MONDAY                            
*                                                                               
         STC   R2,DUB                                                           
*                                                                               
WKLULOOP DS    0H                                                               
*                                                                               
         MVC   HALF(1),0(R5)       EITHER W OR M                                
         MVI   HALF+1,C'B'         BROADCAST                                    
*                                                                               
         MVC   WORK(2),REQCMPST    DEFAULT TO REQUEST START                     
*                                                                               
         TM    REQCFOPT,REQCFYTQ   IF DOING YTD                                 
         BNO   WKLUYTDX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,REQYTDSB),(2,WORK)   USE YTD START DATE           
*                                                                               
WKLUYTDX DS    0H                                                               
*                                                                               
         L     RF,VNETDATE                                                      
         MVI   FULL,1              1=GETLIST                                    
         ICM   RF,8,FULL                                                        
*                                                                               
         GOTO1 (RF),DMCB,HALF,WORK,REQCMPND,0(R3),0(R4),DUB                     
*                                                                               
WKLUCONT DS    0H                                                               
*                                                                               
         CLC   0(2,R4),WORK        STARTDATE=REQCMPST IF LIST NOT FULL          
         BE    WKLUTILX                                                         
*                                                                               
         CLI   0(R5),C'M'           IF MONTHS SELECTED                          
         BNE   WKLUTILX                                                         
*                                                                               
         CLI   2(R5),1                AND SHOULD NOW TRY QUARTERS               
         BNE   WKLUTILX                                                         
*                                                                               
         MVI   0(R5),C'Q'                                                       
*                                                                               
         B     WKLULOOP                                                         
*                                                                               
WKLUTILX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* COVERED BY REDATLSTD (DATE LIST DSECT)                                        
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIOFF - FLOWCHARTING - DOFLOW'                               
***********************************************************************         
*                                                                     *         
*                 FLOWCHARTING                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VDOFLOW  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING SUBSYSD,R9                                                       
         USING GEND,RC                                                          
*                                                                               
*        ADD TITLE ELEMENTS IF STACKING                                         
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLTTLX                                                          
*                                                                               
         L     RF,ASTACKBF         POINT TO STACK BUFFER                        
         USING STACKBFD,RF         ESTABLISH BUFFER                             
*                                                                               
         LM    R3,R5,STBFBXLE      LOAD BXLE REGISTERS FOR BUFFER               
*                                                                               
         DROP  RF                                                               
*                                                                               
DFLTTLLP DS    0H                                                               
*                                                                               
*        ADD ONE COLUMN FOR EACH STACK KEYWORD                                  
*        THEY WILL PRINT VERTICALLY WITH COLUMN HEADINGS                        
*                                                                               
         LA    R0,DRLENIN          CLEAR DRONEBLOCK AREA                        
         LA    R1,DRINFEND-DRLENIN                                              
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0500'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(5),=C'TITLE'                                            
*                                                                               
         LR    R0,R4               SAVE BUFFER ENTRY LENGTH                     
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VCOLDRON            VALIDATE TITLE AS KEYWORD                    
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
         LR    R4,R0               RESTORE BUFFER ENTRY LENGTH                  
*                                                                               
*        USE WIDTH ENTERED ON STACK KEYWORD                                     
*                                                                               
         MVC   DRLENO,STACKWID     SET OUTPUT WIDTH                             
         MVC   DRPOSO(2),=C'P+'    PRINT ON NEXT LINE                           
         NI    DRHEAD1,X'FF'-X'80'   NO HEADLINES                               
*                                                                               
         CLC   DRLABELI-DRLENIN(L'DRLABELI,R3),STACK1ST IF FIRST COL            
         BNE   DFLTTL10                                                         
*                                                                               
         MVI   DRPOSO+1,1          PRINT ON FIRST LINE                          
*                                                                               
         B     DFLTTL10            NO HEADLINES                                 
*                                                                               
         OI    DRHEAD1,X'80'       PRINT HEADLINE                               
*                                                                               
*        TRANSFER SAVED HEADLINES TO HEADLINES FOR THIS FIELD                   
*                                                                               
         LA    R0,2                ONLY INTERESTED IN 1ST TWO HEADLINES         
         LA    R1,STACKH1L         HEAD 1 SAVEAREA                              
         LA    R2,DRHEAD1          POINT TO FIRST HEADLINE                      
*                                                                               
DFLWHDLP DS    0H                                                               
*                                                                               
         USING DRHEADD,R2                                                       
*                                                                               
         CLI   0(R1),X'FF'         IF THERE IS NO HEADING                       
         BNE   *+12                                                             
         NI    DRHEAD,X'FF'-X'80'     SKIP HEADLINE PRINTING                    
         B     DFLWHDCN                                                         
*                                                                               
         CLI   0(R1),0             IF NO OVERRIDE, USE DEFAULT HEADING          
         BE    DFLWHDCN                                                         
*                                                                               
         OI    DRHEAD,X'80'        PRINT HEADLINE                               
*                                                                               
         MVC   DRHLITL,0(R1)       SAVE LITERAL LENGTH                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          GET LENGTH OF HEADING                        
         BZ    *+10                NO LENGTH                                    
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     *+10                                                             
         MVC   DRHLIT,1(R1)        SET HEADLINE                                 
*                                                                               
DFLWHDCN DS    0H                                                               
*                                                                               
         LA    R2,L'DRH1ALL(R2)    POINT TO NEXT HEADLINE                       
         LA    R1,STACKH2L         POINT TO SECOND SAVEAREA                     
         BCT   R0,DFLWHDLP                                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
DFLTTL10 DS    0H                                                               
*                                                                               
         MVI   DRLITLNI,1          INPUT IS LITERAL TO FORCE                    
         MVI   DRLITI,C' '           DRIVER TO HANDLE IT                        
         MVC   DROPTSO,DROPTSO-DRLENIN(R3)  COPY OPTIONS                        
*                                                                               
*        COMBINE HEADLINES INTO AN OUTPUT LITERAL                               
*                                                                               
         LA    R7,DRH1ALL-DRLENIN(R3)  POINT TO FIRST HEADLINE                  
         USING DRH1ALL,R7          ESTABLISH HEADLINE AREA                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,STACKWID          MAX LENGTH TO OUTPUT LITERAL                
         LA    R1,4                 MAX 4 HEADLINES                             
         LA    R2,DRLITO            START OF OUTPUT LITERAL                     
         XC    DRLITO,DRLITO        INIT OUTPUT LITERAL                         
*                                                                               
DFLLITLP DS    0H                                                               
*                                                                               
         TM    DRHEAD1,X'80'       SKIP IF NOT PRINTING HEADLINE                
         BNO   DFLLIT10                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH1LITL       LENGTH OF FIRST HEADLINE                     
         BZ    DFLLIT10            BLANK HEADLINE                               
*                                                                               
         LA    RE,DRH1LIT(RF)      POINT TO LAST BYTE OF LITERAL                
         BCTR  RE,0                                                             
*                                                                               
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+14                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         B     DFLLIT10            BLANK HEADLINE                               
*                                                                               
         CR    RF,R0               CHECK IF IT WILL FIT                         
         BH    DFLLITDN                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),DRH1LIT     MOVE HEADLINE TO OUTPUT                      
*                                                                               
         LA    RF,2(RF)            LENGTH INCLUDING TRAILING SPACE              
         LA    R2,0(RF,R2)         NEXT AVAILABLE SPOT                          
         SR    R0,RF               LENGTH LEFT IN OUTPUT LITERAL                
         BNP   DFLLITDN            NO ROOM LEFT                                 
*                                                                               
DFLLIT10 DS    0H                                                               
*                                                                               
DFLLITCN DS    0H                                                               
*                                                                               
         LA    R7,L'DRH1ALL(R7)    BUMP TO NEXT HEADLINE                        
         BCT   R1,DFLLITLP                                                      
*                                                                               
DFLLITDN DS    0H                                                               
*                                                                               
         BCTR  R2,0                ELIMINATE TRAILING SPACE                     
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)            LAST WASN'T A SPACE                          
*                                                                               
         LA    RF,DRLITO           START OF LITERAL                             
         SR    R2,RF               LITERAL LENGTH                               
         BP    *+12                IF ALL HEADS BLANK                           
         LA    R2,1                                                             
         MVI   DRLITO,X'41'           FORCE TO PRINT                            
*                                                                               
         STC   R2,DRLITLNO         PASS ON LENGTH                               
*                                                                               
         DROP  R7                                                               
*                                                                               
         GOTO1 GCOLDRON            GENERATE TITLE KEYWORD                       
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
DFLTTLCN DS    0H                                                               
*                                                                               
         BXLE  R3,R4,DFLTTLLP                                                   
*                                                                               
DFLTTLX  DS    0H                                                               
*                                                                               
*        IF FLOWCHARTING NON-NUMERIC                                            
*        THEN MUST ADD 'STACK' KEYWORD                                          
*                                                                               
DFLSTCK  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF ALREADY STACKING                     
         BO    DFLSTCKX                                                         
*                                                                               
         CLI   DRTYPEI,C'C'        IF CHARACTER                                 
         BE    *+8                                                              
         CLI   DRTYPEI,C'X'           HEX                                       
         BE    *+8                                                              
         CLI   DRTYPEI,C'D'           DATE                                      
         BE    *+8                                                              
         B     DFLSTCKX                                                         
*                                  ADD STACK KEYWORD                            
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         LA    R2,DRLENIN                                                       
         L     R1,AIO1                                                          
         BAS   RE,MOVERE           SAVE MASTER DRONEBLK                         
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0502'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'STACK'                                            
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VROWDRON            VALIDATE STACK AS KEYWORD                    
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' MAKE SURE THERE IS NO PRINTING               
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
* MAY NEED TO ALTER WIDTH                                                       
*                                                                               
         GOTO1 GROWDRON            GENERATE STACK KEYWORD                       
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
*        RESTORE MASTER DRONEBLK                                                
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         L     R2,AIO1                                                          
         LA    R1,DRLENIN                                                       
         BAS   RE,MOVERE                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
DFLSTCKX DS    0H                                                               
*                                                                               
         L     R5,ADATLIST         ESTABLISH DATEAREA                           
         USING DATELSTD,R5                                                      
*                                                                               
         LA    R7,EXPLIST          POINT TO FIRST IN EXPLIST                    
         ZAP   DFLCTR,=P'1'        INIT ITERATION COUNTER                       
*                                                                               
         CLI   STFLWSTR,0          IF STATION FLOWCHARTING                      
         BE    *+10                                                             
         SR    R7,R7                                                            
         IC    R7,STFLWSTR            GET FIRST COMPETETIVE STATION             
*                                                                               
DFLLOOP  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLP10                                                          
*                                                                               
         L     RF,ASTACKBF         POINT TO STACK BUFFER                        
         USING STACKBFD,RF         ESTABLISH BUFFER                             
*                                                                               
         LM    R3,R5,STBFBXLE      LOAD BXLE REGISTERS FOR BUFFER               
*                                                                               
         DROP  RF                                                               
*                                                                               
DFLSTKLP DS    0H                                                               
*                                                                               
         STM   R3,R5,DFLBXLE       SAVE IN LOCAL STORAGE                        
*                                                                               
         USING STBFENTD,R3         ESTABLISH BUFFER ENTRY                       
*                                                                               
         LA    R1,DRINFEND-DRLENIN DRONE BLOCK LENGTH                           
         LR    RF,R1               DRONE BLOCK LENGTH                           
         LA    R0,STBFDBLK         FROM                                         
         LA    RE,DRLENIN          TO                                           
*                                                                               
         MVCL  RE,R0               COPY BUFFER                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVC   DRLENO,STACKLNO     SET COLUMN WIDTH                             
*                                                                               
         MVC   DRPOSO(2),=C'P+'    PRINT ON NEXT LINE                           
*                                                                               
         NI    DRHEAD1,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD2,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD3,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD4,X'FF'-X'80' DON'T PRINT HEADLINES                        
*                                                                               
         CLC   DRLABELI,STACK1ST   IF FIRST COLUMN                              
         BNE   DFLLP05                                                          
*                                                                               
         MVI   DRPOSO+1,1             PRINT ON FIRST LINE (P1)                  
*                                                                               
*        PRINT FIRST 2 COLUMN HEADINGS                                          
*                                                                               
         CLI   STACKH1L,0          IF STACK HEADLINE EXISTS                     
         BE    DFLLP03                                                          
         CLI   STACKH1L,X'FF'      IF STACK HEADLINE EXISTS                     
         BE    DFLLP03                                                          
*                                                                               
         OI    DRHEAD1,X'80'          PRINT HEADLINE                            
         MVC   DRH1LITL,STACKH1L      PASS LITERAL LENGTH TO DRONE              
         XC    DRH1LIT,DRH1LIT        INIT HEADLINE AREA                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH1LITL                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DRH1LIT(0),STACKH1  PASS LITERAL TO DRONE                        
*                                                                               
DFLLP03  DS    0H                                                               
*                                                                               
         CLI   STACKH2L,0          IF STACK HEADLINE EXISTS                     
         BE    DFLLP04                                                          
         CLI   STACKH2L,X'FF'      IF STACK HEADLINE EXISTS                     
         BE    DFLLP04                                                          
*                                                                               
         OI    DRHEAD2,X'80'          PRINT HEADLINE                            
         MVC   DRH2LITL,STACKH2L      PASS LITERAL LENGTH TO DRONE              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH2LITL                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DRH2LIT(0),STACKH2  PASS LITERAL TO DRONE                        
*                                                                               
DFLLP04  DS    0H                                                               
*                                                                               
DFLLP05  DS    0H                                                               
*                                                                               
DFLLP10  DS    0H                                                               
*                                                                               
         CLI   DRTYPEI+1,C' '      IF THIS INFO NOT SPECIFIED                   
         BH    *+8                                                              
         MVI   DRTYPEI+1,C'+'         FORCE ADDITIVE NATURE                     
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLP15                                                          
*                                                                               
         CLI   FLOWSTRT,0          IF NO DATES SPECIFIED                        
         BNE   DFLLP15                                                          
*                                                                               
         MVC   DRARGSI+9(6),REQPSTRB    USE PERIOD DATES                        
*                                                                               
         TM    DRARGSI+8,X'80'          IF YTD COLUMN                           
         BNO   *+10                                                             
         MVC   DRARGSI+9(3),REQYTDSB       RESET START OF PERIOD                
*                                                                               
         B     DFLCHDN                                                          
*                                                                               
DFLLP15  DS    0H                                                               
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         LA    R2,DRLENIN                                                       
         L     R1,AIO1                                                          
         BAS   RE,MOVERE           SAVE MASTER DRONEBLK                         
*                                                                               
         CLI   STFLWSTR,0          IF FLOWCHARTING BY STATIONS                  
         BE    DFLLP20                                                          
*                                                                               
         LA    R6,DRH3ALL             POINT TO 3RD HEADLINE                     
*                                                                               
         CLI   DRH3LITL-DRH3ALL(R6),0   IF HEADLINE ALREADY USED                
         BE    *+8                                                              
         LA    R6,L'DRH3ALL(R6)            BUMP TO NEXT HEADLINE                
*                                                                               
         OI    DRHEAD3-DRH3ALL(R6),X'80'   MAKE SURE HEADLINE PRINTS            
*                                                                               
         MVC   DRH3OPTS-DRH3ALL(L'DRH3OPTS,R6),DRH1OPTS CPY HDLN 1 OPTS         
*                                                                               
         STC   R7,DRARGSI+11               SAVE STATION NUMBER                  
         STC   R7,DRARGSO+11               SAVE STATION NUMBER                  
         STC   R7,DRH3ARGS+11-DRH3ALL(R6)                                       
         MVI   DRH3NARG-DRH3ALL(R6),16     MAXIMUM NUMBER OF ARGS               
*                                                                               
         MVC   DRH3RTN-DRH3ALL(L'DRH3RTN,R6),=CL8'HSTA' SET HEAD RTN            
*                                                                               
         B     DFLCHDN                                                          
*                                                                               
DFLLP20  DS    0H                  FLOWCHARTING BY DATE                         
*                                                                               
         CLI   0(R7),0             PROBLEMS IF AT END OF LIST                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,ADATLIST         ESTABLISH DATE LIST                          
         USING DATELSTD,R5                                                      
*                                                                               
         SR    RE,RE               DETERMINE LIST ADJUSTMENT FACTOR             
*                                                                               
         CLI   REQDTYPE,C'M'       MONTHS                                       
         BNE   *+8                                                              
         IC    RE,YTDADJM                                                       
*                                                                               
         CLI   REQDTYPE,C'Q'       QUARTERS                                     
         BNE   *+8                                                              
         IC    RE,YTDADJQ                                                       
*                                                                               
         CLI   REQDTYPE,C'S'       SEMI-YEARS                                   
         BNE   *+8                                                              
         IC    RE,YTDADJS                                                       
*                                                                               
         CLI   REQDTYPE,C'Y'       YEARS                                        
         BNE   *+8                                                              
         IC    RE,YTDADJY                                                       
*                                                                               
         ZIC   RF,0(R7)            INDEX TO CORRECT DATES                       
         AR    RF,RE               ADD IN LIST ADJUSTMENT FACTOR                
         BCTR  RF,0                                                             
         MH    RF,=H'4'            2 BYTE COMPRESSED DATES                      
*                                                                               
         LA    RF,DATELSTD(RF)     POINT TO DATE IN LIST                        
*                                                                               
         LR    R6,RF               SAVE POINTER                                 
*                                                                               
         LA    R2,0(R6)            ASSUME DATES IN BINARY                       
*                                                                               
         CLI   REQDTYPE,C'W'       WEEKS  IN COMPRESSED B'CAST WEEK             
         BNE   DFLTYPWX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(3,DUB)   MONDAY TO YMD BINARY             
*                                                                               
         MVC   DRARGSI+9(3),DUB    START OF WEEK                                
*                                                                               
         MVC   DFLSTRTB,0(R2)      SAVE START DATE                              
*                                                                               
         GOTO1 =A(GETBM)           FIND SUNDAY OF WEEK                          
*                                                                               
         MVC   DRARGSI+12(3),WORK+6  END OF WEEK                                
*                                                                               
         LA    R2,DRARGSI+9        POINT TO START AND END DATES                 
*                                                                               
         B     DFLTYPF3                                                         
*                                                                               
DFLTYPWX DS    0H                                                               
*                                                                               
         CLI   REQDTYPE,C'M'       MONTHS IN COMPRESSED B'CAST                  
         BNE   DFLTYPF1                                                         
*                                                                               
         CLI   DRARGSI,RPWCKTYQ     SKIP IF FLOWCHARTING PAPERWORK KYWS         
         BE    DFLPWC                                                           
*                                                                               
         GOTO1 =A(GETBM)           GET START BROADCAST MONTH                    
*                                                                               
         LA    R2,WORK+6           POINT TO YM OF BROADCAST MONTH               
*                                                                               
DFLTYPF1 DS    0H                                                               
*                                                                               
         CLI   DRARGSI,RPWCKTYQ     SKIP IF FLOWCHARTING PAPERWORK KYWS         
         BE    DFLPWC2                                                          
*                                                                               
         MVC   DRARGSI+9(2),0(R2)   YM BINARY BROADCAST MONTH                   
*                                                                               
         MVC   DFLSTRTB(2),0(R2)      SAVE START DATE                           
         MVI   DFLSTRTB+2,0        YM ONLY                                      
*                                                                               
         TM    DRARGSI+8,X'80'     IF YTD COLUMN                                
         BNO   *+10                                                             
         MVC   DRARGSI+9(2),REQYTDSB    USE YTD START MONTH                     
*                                                                               
         CLC   =C'ICST ',DRRTNI    IF STATION SHARE KEYWORD                     
         BNE   DFLSTR10                                                         
         CLI   DRARGSI+2,C'P'         AND PRIOR TYPE                            
         BNE   DFLSTR10                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DRARGSI+9              DECREMENT YEAR BY 1                    
         AHI   RF,-1                                                            
         STC   RF,DRARGSI+9                                                     
*                                                                               
DFLSTR10 DS    0H                                                               
*                                                                               
         LA    R2,2(R6)            ASSUME DATES IN BINARY                       
*                                                                               
         CLI   REQDTYPE,C'M'       MONTHS IN COMPRESSED B'CAST                  
         BNE   DFLTYPF2                                                         
*                                                                               
         GOTO1 =A(GETBM)            GET END   BROADCAST MONTH                   
*                                                                               
         LA    R2,WORK+6           POINT TO YM OF BROADCAST MONTH               
*                                                                               
DFLTYPF2 DS    0H                                                               
*                                                                               
         MVC   DRARGSI+12(2),0(R2)   YM BINARY BROADCAST MONTH                  
*                                                                               
         CLC   =C'ICST ',DRRTNI    IF STATION SHARE KEYWORD                     
         BNE   DFLEND10                                                         
         CLI   DRARGSI+2,C'P'         AND PRIOR TYPE                            
         BNE   DFLEND10                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DRARGSI+12             DECREMENT YEAR BY 1                    
         AHI   RF,-1                                                            
         STC   RF,DRARGSI+12                                                    
*                                                                               
DFLEND10 DS    0H                                                               
*                                                                               
         B     DFLTYPF3                                                         
*                                                                               
*        FLOW CHART PAPERWORK KEYWORDS                                          
*                                                                               
DFLPWC   DS    0H                  DATES ARE FULL BDCAST MONTH                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(3,DUB)   START TO YMD BINARY              
*                                                                               
         MVC   DRARGSI+9(3),DUB    START OF MONTH                               
*                                                                               
         MVC   DFLSTRTB,0(R2)      SAVE START DATE                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,DUB)   END   TO YMD BINARY              
*                                                                               
         MVC   DRARGSI+12(3),DUB   END   OF MONTH                               
*                                                                               
         B     DFLTYPF3                                                         
*                                                                               
DFLPWC2  DS    0H                  DATES ARE BINARY YM BDCAST MONTHS            
*                                                                               
         MVC   DFLSTRTB(2),0(R2)      SAVE START DATE                           
         MVI   DFLSTRTB+2,0        YM ONLY                                      
*                                                                               
         MVC   DUB(2),0(R2)        YM BINARY OF START MONTH                     
         MVI   DUB+2,1             ASSUME FIRST OF MONTH                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),WORK CONVERT TO YYMMDD                       
*                                                                               
*                                  GET BROADCAST MONTH                          
         GOTO1 QGTBROAD,DMCB,(1,WORK),(C'0',WORK+6),GETDAY,ADDAY                
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,DRARGSI+9)   START DATE                
*                                                                               
         MVC   DUB(2),2(R2)        YM BINARY OF END   MONTH                     
         MVI   DUB+2,1             ASSUME FIRST OF MONTH                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),WORK CONVERT TO YYMMDD                       
*                                                                               
*                                  GET BROADCAST MONTH                          
         GOTO1 QGTBROAD,DMCB,(1,WORK),(C'0',WORK+6),GETDAY,ADDAY                
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,DRARGSI+12)  END DATE                 
*                                                                               
         B     DFLTYPF3                                                         
*                                                                               
DFLTYPF3 DS    0H                                                               
*                                                                               
         MVC   DRARGSI+15(1),REQDTYPE   DATE TYPE                               
*                                                                               
*        PUT DATES IN HEADLINES                                                 
*                                                                               
         CLC   DRPOSO(2),=C'P+'    SKIP IF NOT FIRST FOR COLUMN                 
         BE    DFLCHDX1                                                         
*                                                                               
         LA    R6,DRH3ALL          POINT TO 3RD HEADLINE                        
         USING DRH3ALL,R6          ESTABLISH HEADLINE AREA                      
*                                                                               
         CLI   DRH3LITL,0          IF HEADLINE ALREADY USED                     
         BE    *+8                                                              
         LA    R6,L'DRH3ALL(R6)    BUMP TO NEXT HEADLINE                        
*                                                                               
         MVC   DRH3OPTS-DRH3ALL(L'DRH3OPTS,R6),DRH1OPTS CPY HDLN 1 OPTS         
*****    OI    DRH3OPTS-DRH3ALL(R6),DRHALGNR   ALIGN RIGHT                      
*                                                                               
         CLI   DRARGSI+15,C'W'     IF WEEK  DATES                               
         BNE   DFLTYPWN                                                         
*                                     COLUMN HEADING IS MMM/DD/YY               
         GOTO1 DATCON,DMCB,(3,DFLSTRTB),(17,DRH3LIT)                            
*                                                                               
         MVI   DRH3LITL,8          SET TITLE LENGTH TO 8                        
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLTYPWN DS    0H                                                               
*                                                                               
         MVC   FULL(3),DRARGSI+12  COPY DATE                                    
*                                                                               
         CLC   =C'IDOL ',DRRTNI    IF DOLLAR KEYWORD                            
         BNE   *+12                                                             
         LA    R1,DRARGSI+4           POINT TO CURRENT/PRIOR INDICATOR          
         B     DFLTYP03                                                         
******                                                                          
******   CLC   =C'ICST ',DRRTNI    IF STATION SHARE KEYWORD                     
******   BNE   *+12                                                             
******   LA    R1,DRARGSI+2           POINT TO CURRENT/PRIOR INDICATOR          
******   B     DFLTYP03                                                         
*                                                                               
         B     DFLTYP10                                                         
*                                                                               
DFLTYP03 DS    0H                                                               
*                                                                               
         CLI   0(R1),C'C'          OKAY IF CURRENT DOLLARS                      
         BE    DFLTYP10                                                         
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF STACKING                             
         BO    DFLTYP10                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FULL           GET YEAR                                     
*                                                                               
         CLI   0(R1),C'P'          IF PRIOR KEYWORD                             
         BNE   *+12                                                             
         AHI   RF,-1                  DECREMENT YEAR                            
         B     DFLTYP05                                                         
*                                                                               
         CLI   0(R1),C'2'          IF PRIOR 2 YEAR KEYWORD                      
         BNE   *+12                                                             
         AHI   RF,-2                  DECREMENT YEAR BY 2                       
         B     DFLTYP05                                                         
*                                                                               
         CLI   0(R1),C'N'          IF NEXT YEAR KEYWORD                         
         BNE   DFLTYP10                                                         
         AHI   RF,1                   INCREMENT YEAR                            
         B     DFLTYP05                                                         
*                                     COLUMN HEADING IS MMM/YY                  
DFLTYP05 DS    0H                                                               
*                                                                               
         STC   RF,FULL             RESET YEAR                                   
*                                                                               
DFLTYP10 DS    0H                                                               
*                                                                               
         CLI   DRARGSI+15,C'M'     IF MONTH DATES                               
         BNE   DFLTYPCN                                                         
*                                     COLUMN HEADING IS MMM/YY                  
         GOTO1 DATCON,DMCB,(3,FULL),(9,DRH3LIT)                                 
*                                                                               
         MVI   DRH3LITL,6          SET TITLE LENGTH TO 6                        
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLTYPCN DS    0H                                                               
*                                                                               
*        FORMAT DATES IN COLUMN HEADING                                         
*                                                                               
         CLI   DRARGSI+15,C'Y'     IF YEARLY    DATES                           
         BE    DFLCHDQ1                                                         
*                                                                               
         CLI   DRARGSI+15,C'Q'     IF QUARTERLY DATES                           
         BNE   DFLCHDQN                                                         
*                                                                               
         CLI   DRLENO,9            AND COLUMN WIDTH AT MOST 9                   
         BH    DFLCHDQ1                                                         
*                                                                               
         SR    RF,RF                                                            
         SR    RE,RE               FORMAT QUARTER AS #QYY                       
*                                                                               
         IC    RF,FULL+1           MONTH OF END OF QUARTER                      
*                                                                               
         D     RE,=F'4'            NUMERATOR GIVES RELATIVE QUARTER             
*                                                                               
         LA    RF,1(RF)            TRUE QUARTER                                 
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  DRH3LIT(1),DUB      PRINT QUARTER                                
*                                                                               
         MVI   DRH3LIT+1,C'Q'      QUARTER SYMBOL                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FULL             QUARTER'S YEAR                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  DRH3LIT+2(2),DUB     PRINT YEAR                                  
*                                                                               
         MVI   DRH3LITL,4          LITERAL LENGTH                               
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDQ1 DS    0H                  FORMAT COLUMN HEADING AS MMM-MMM/YY          
*                                                                               
         MVI   DRH3LITL,10                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,DFLSTRTB),(4,DRH3LIT)                             
*                                                                               
         MVI   DRH3LIT+3,C'-'                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(9,DRH3LIT+4)                               
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDQN DS    0H                                                               
*                                                                               
         CLI   DRARGSI+15,C'S'     IF SEMI-ANNUAL DATES                         
         BNE   DFLCHDSN                                                         
*                                                                               
         CLI   DRLENO,9            AND COLUMN WIDTH AT MOST 9                   
         BH    DFLCHDS1                                                         
*                                                                               
         SR    RF,RF                                                            
         SR    RE,RE               FORMAT AS #SYY                               
*                                                                               
         IC    RF,DRARGSI+13       MONTH OF END OF HALF-YEAR                    
*                                                                               
         D     RE,=F'6'            NUMERATOR GIVES RELATIVE HALF                
*                                                                               
         LA    RF,1(RF)            TRUE HALF                                    
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  DRH3LIT(1),DUB      PRINT HALF-YEAR                              
*                                                                               
         MVI   DRH3LIT+1,C'S'      SEMI-YEAR SYMBOL                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DUB              YEAR                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  DRH3LIT+2(2),DUB     PRINT YEAR                                  
*                                                                               
         MVI   DRH3LITL,4          LITERAL LENGTH                               
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDS1 DS    0H                  FORMAT COLUMN HEADING AS MMM-MMM/YY          
*                                                                               
         MVI   DRH3LITL,10                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,DFLSTRTB),(4,DRH3LIT)                             
*                                                                               
         MVI   DRH3LIT+3,C'-'                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(9,DRH3LIT+4)                               
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDSN DS    0H                  ALL ELSE USE MMM/YY                          
*                                  ENSURES BROADCAST MONTH                      
         GOTO1 DATCON,DMCB,(3,FULL),(18,DRH3LIT)                                
*                                                                               
         MVI   DRH3LITL,6                                                       
*                                                                               
DFLCHDX  DS    0H                                                               
*                                                                               
         OI    DRHEAD3,X'80'       MAKE SURE HEADLINE PRINTS                    
*                                                                               
         CLC   DRH3LITL,DRLENO     DEFAULT TO SMALLER                           
         BNH   *+10                                                             
         MVC   DRH3LITL,DRLENO                                                  
*                                                                               
DFLCHDX1 DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
DFLCHDN  DS    0H                                                               
*                                                                               
         MVI   DRARGSI+7,C'$'      INDICATE FLOWCHART KEYWORD                   
         MVC   DRARGSO+7(9),DRARGSI+7 COPY DATE RANGE TO OUTPUT                 
*                                  DON'T INCLUDE RANGE TYPE                     
         MVI   DRNARGSO,16         FORCE MAXIMUM ARGUMENTS                      
         MVI   DRNARGSI,16                                                      
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF STACKING                             
         BO    *+10                   LABEL ALREADY IN DRONEBLOCK               
         MVC   DRLABELI,MYLABEL                                                 
*                                                                               
*        COPY FILTERAREA                                                        
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF STACKING                             
         BO    DFLCFLTX               KEYWORD #'S NOT RESET                     
*                                                                               
         L     R3,AFLTTAB          POINT TO FILTER SAVEAREAS                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DRARGSI+6      ORIGINAL KEYWORD#                            
         BZ    *+6                                                              
         BCTR  RE,0                DECREMENT FOR INDEXING                       
         LA    RF,FTBENTL          FILTER AREA LENGTH                           
         MR    RE,RE               INDEX INTO FILTER AREA                       
         LA    R3,0(RF,R3)         AREA FOR THIS KEYWORD                        
*                                                                               
         L     R1,AFLTTAB          POINT TO FILTER SAVEAREAS                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,KEYWORD#       CURRENT KEYWORD#                             
         BZ    *+6                                                              
         BCTR  RE,0                DECREMENT FOR INDEXING                       
         LA    RF,FTBENTL          FILTER AREA LENGTH                           
         MR    RE,RE               INDEX INTO FILTER AREA                       
         LA    R1,0(RF,R1)         AREA FOR THIS KEYWORD                        
*                                                                               
         MVC   0(FTBENTL,R1),0(R3) COPY FILTERAREA                              
*                                                                               
         MVC   DRARGSI+6(1),KEYWORD# PASS ALONG KEYWORD NUMBER                  
         MVC   DRARGSO+6(1),KEYWORD# PASS ALONG KEYWORD NUMBER                  
*                                                                               
DFLCFLTX DS    0H                                                               
*                                                                               
         GOTO1 GCOLDRON            GENERATE COLUMN                              
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLBLX                                                          
*                                                                               
         SR    RF,RF                                                            
         L     R6,DROLDBUF         POINT TO START OF BUFFER ENTRY               
         USING DEISD,6             ESTABLISH AS INPUT LABEL ELEMENT             
*                                                                               
         CLI   DEISEL,0            SKIP IF END OF BUFFER                        
         BE    DFLLBLX                                                          
         CLI   DEISEL,X'20'        FIND INPUT LABEL ELEMENT                     
         BE    *+16                                                             
         IC    RF,DEISLEN          BUMP TO NEXT ELEMENT                         
         LA    R6,DEISD(RF)                                                     
         B     *-24                                                             
*                                                                               
         UNPK  DEISLAB+1(2),DFLCTR ADD ITERATION COUNTER TO LABEL               
         OI    DEISLAB+2,X'F0'     FORCE ZONE                                   
*                                                                               
DFLLBLX  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLCONT                                                          
*                                                                               
         L     R3,DFLBXLE          RE-POINT TO CURRENT BUFFER ENTRY             
         USING STBFENTD,R3         ESTABLISH ENTRY                              
*                                                                               
         CLC   =CL8'ICOMPUTE',DRRTNI-DRLENIN+STBFDBLK IF COMPUTE DONE           
         BNE   DFLCOMX             DDRONE CLEARS DRBLOCK ON RETURN              
*                                                                               
         MVC   BLOCK+42(42),STBFSBLK RESTORE SCANNER BLOCK FOR COMPUTE          
         LA    R4,BLOCK+42         SET SCAN BLOCK POINTER                       
*                                                                               
         GOTO1 GCMPDRON            GENERATE COMPUTE DATA                        
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                                                               
         SR    RF,RF                                                            
         L     R6,DROLDBUF         POINT TO START OF BUFFER ENTRY               
         USING DEFRMD,R6           ESTABLISH AS COMP FORMULA ELEMENT            
*                                                                               
DFLCMPLP DS    0H                                                               
*                                                                               
         CLI   DEFRMEL,0           STOP AT END OF BUFFER                        
         BE    DFLCMPDN                                                         
*                                                                               
         CLI   DEFRMEL,X'52'       FIND COMP FORMULA ELEMENT                    
         BNE   DFLCMPCN                                                         
*                                                                               
         CLI   DEFRMON,0           SKIP IF NOT A LABEL                          
         BNE   DFLCMPCN                                                         
*                                                                               
         UNPK  DEFRMLAB+1(2),DFLCTR ADD ITERATION COUNTER TO LABEL              
         OI    DEFRMLAB+2,X'F0'    FORCE ZONE                                   
*                                                                               
DFLCMPCN DS    0H                                                               
*                                                                               
         IC    RF,DEFRMLEN         BUMP TO NEXT ELEMENT                         
         LA    R6,DEFRMD(RF)                                                    
         B     DFLCMPLP                                                         
*                                                                               
DFLCMPDN DS    0H                                                               
*                                                                               
DFLCOMX  DS    0H                                                               
*                                                                               
         B     DFLSCKCN                                                         
*                                                                               
DFLSCKCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
         LM    R3,R5,DFLBXLE       RELOAD BXLE REGISTERS                        
         BXLE  R3,R4,DFLSTKLP      HANDLE NEXT IN STACK                         
*                                                                               
         B     DFLCONT1                                                         
*                                                                               
DFLCONT  DS    0H                                                               
*                                                                               
         LA    R0,DRINFEND-DRLENIN   RE-COPY MASTER DRONEBLK                    
         L     R2,AIO1             BECAUSE GENERATION DESTROYS BLOCK            
         LA    R1,DRLENIN                                                       
         BAS   RE,MOVERE                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
DFLCONT1 DS    0H                                                               
*                                                                               
         CLI   STFLWSTR,0          IF STATION FLOWCHARTING                      
         BE    DFLCONT2                                                         
*                                                                               
         LA    R7,1(R7)               BUMP COUNTER                              
*                                                                               
         CLM   R7,1,STFLWEND       CHECK FOR END OF LIST                        
         BH    DOFLOWX                                                          
*                                                                               
         B     DFLCONT3                                                         
*                                                                               
DFLCONT2 DS    0H                                                               
*                                                                               
         CLI   0(R7),0             DONE IF NO DATES GIVEN                       
         BE    DOFLOWX                                                          
*                                                                               
         CLI   FLOWEND,0           DONE IF NO DATE RANGE                        
         BE    DOFLOWX                                                          
*                                                                               
         LA    R7,1(R7)            BUMP DATE POINTER                            
*                                                                               
         CLI   0(R7),0             DONE IF END OF DATES REACHED                 
         BE    DOFLOWX                                                          
*                                                                               
         AP    DFLCTR,=P'1'        BUMP ITERATION COUNTER                       
*                                                                               
DFLCONT3 DS    0H                                                               
*                                                                               
         ZIC   RF,MYPOSO           BUMP POSITION COUNTER                        
         LA    RF,1(RF)                                                         
         STC   RF,MYPOSO                                                        
*                                                                               
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
         B     DFLLOOP                                                          
*                                                                               
DOFLOWX  DS    0H                                                               
*                                                                               
         LA    R0,DRLENIN          CLEAR DRONEBLOCK AREA                        
         LA    R1,DRINFEND-DRLENIN                                              
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
*===================================================*                           
* SUBROUTINE MOVES REC AT R2 TO R1 FOR LENGTH IN R0 *                           
* RETURN ON RE (R0-R2 AND RF ARE DESTROYED)         *                           
*===================================================*                           
         SPACE 1                                                                
MOVERE   LA    RF,256                                                           
         CR    R0,RF                                                            
         BNH   MOVERE2                                                          
         MVC   0(256,R1),0(R2)                                                  
         AR    R1,RF                                                            
         AR    R2,RF                                                            
         SR    R0,RF                                                            
         B     MOVERE                                                           
*                                                                               
MOVERE2  LTR   RF,R0                                                            
         BZR   RE                                                               
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2) *EXECUTED*                                         
*                                                                               
DFLSTRTB DS    XL3                 COLUMN PERIOD START - BINARY                 
DFLBXLE  DS    3A                  BXLE REGISTERS SAVEAREA                      
DFLCTR   DS    PL2                 ITERATION COUNTER                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIGEN - VALIDATE ROWS - VGENHEAD'                            
***********************************************************************         
*                                                                     *         
*        GENERAL HEADLINE HOOK ROUTINES                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VGENHEAD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,34(R2)           FIND START OF TITLE                          
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   VGENH10                                                          
*                                                                               
         SH    R2,=H'14'                                                        
*                                                                               
         L     R1,AH4                                                           
         MVC   BLOCK(24),59(R1)    SAVE H4 RIGHT FOR NARROW                     
         MVC   59(24,R1),SPACES                                                 
*                                                                               
         MVC   0(39,R2),TITLE      (TITLE IS ALREADY CENTERED)                  
         A     R2,PWIDTH           BUMP TO NEXT LINE                            
*                                                                               
         GOTO1 UNDERLIN,DMCB,(39,TITLE),(X'BF',(R2))                            
*                                                                               
         MVC   39(26,R2),43(R2)    FIX 'RUN ON' TITLE                           
*                                                                               
         B     VGENH15                                                          
*                                                                               
VGENH10  MVC   0(64,R2),TITLE      (TITLE IS ALREADY CENTERED)                  
*                                                                               
         A     R2,PWIDTH           BUMP TO NEXT LINE                            
*                                                                               
         GOTO1 UNDERLIN,DMCB,(64,TITLE),(X'BF',(R2))                            
*                                                                               
VGENH15  DS    0H                                                               
*                                                                               
*        LINE UP TITLES, CODES * NAMES FOR HEADLINES                            
*                                                                               
         L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
*                                                                               
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
*                                                                               
         SR    R4,R4                                                            
         IC    R4,GLFHEADL         NUMBER OF HEADLINES                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         SH    R4,=H'6'                                                         
*                                                                               
         BAS   RE,GETLONG                                                       
*                                                                               
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
*                                                                               
         LA    R3,15                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
*                                                                               
         L     R2,AH4              RESTORE HEAD4                                
         LA    R2,96(R2)                                                        
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,42(R2)                                                        
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   *+14                                                             
         SH    R2,=H'37'                                                        
         MVC   0(24,R2),BLOCK      (REPLACE SAVED H4 RIGHT)                     
*                                                                               
         L     R2,AH1              PERIOD TO HEAD3 CENTER                       
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         ST    R2,FULL                                                          
*                                                                               
         LA    R2,66-19(R2)                                                     
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,15(,R2)                                                       
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   *+8                                                              
         SH    R2,=H'26'                                                        
*                                                                               
         OC    APERFLD,APERFLD     WAS PERIOD ENTERED                           
         BZ    VGENH20                                                          
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BE    *+10                                                             
         MVC   0(7,R2),=C'FOR THE'                                              
*                                                                               
         MVC   8(11,R2),=C'PERIOD FROM'                                         
         MVC   20(6,R2),REQPSTRM                                                
         MVC   27(2,R2),=C'TO'                                                  
         MVC   30(6,R2),REQPENDM                                                
*                                                                               
         A     R2,PWIDTH                                                        
         L     RF,FULL                                                          
         A     RF,PWIDTH                                                        
         ST    RF,FULL                                                          
*                                                                               
VGENH20  DS    0H                                                               
*                                                                               
         ICM   RE,15,AACTFLD       WERE ACTIVITY DATES ENTERED                  
         BZ    VGENH30                                                          
*                                                                               
         LA    R2,5(,R2)                                                        
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,3(,R2)                                                        
*                                                                               
         MVC   0(8,R2),=C'ACTIVITY'                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,REQAASTR),(5,9(R2))                               
*                                                                               
         MVC   18(2,R2),=C'TO'                                                  
*                                                                               
         GOTO1 (RF),(R1),(0,REQAAEND),(5,21(R2))                                
*                                                                               
         L     RF,FULL                                                          
         A     RF,PWIDTH                                                        
         ST    RF,FULL                                                          
*                                                                               
VGENH30  MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(197),BLOCK                                               
*                                                                               
         ICM   RF,15,FULL          A(PRINT AREA)                                
         BZ    VGENHDX                                                          
*                                                                               
         GOTO1 =A(DSPOPT),DMCB,(RF)     DISPLAY OPTIONS                         
*                                                                               
VGENHDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
GETLONG  NTR1  LABEL=*                                                          
*              INPUTS              R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=NUMBER OF LINES                           
*              OUTPUT              FULL=WIDEST FOUND                            
         SPACE                                                                  
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    GETLONGX                                                         
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
         SPACE                                                                  
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    GETLONGX                                                         
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
*                                                                               
GETLONGX DS    0F                                                               
         XIT1                                                                   
*                                                                               
         DS    0D                                                               
SHUFFLE  NTR1  LABEL=*                                                          
*              INPUTS              R2=A(START DATA ON FIRST LINE)               
*                                  R3=A(FROM DATA)                              
*                                  R4=NUMBER OF LINES                           
         SPACE                                                                  
SHUFFLE2 MVC   WORK,0(R3)                                                       
         MVC   0(60,R3),SPACES                                                  
         MVC   0(60,R2),WORK                                                    
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - DSPOPT'            
***********************************************************************         
*                                                                     *         
*        DISPLAY OPTIONS IN HEADLINES                                 *         
*                                                                     *         
*NTRY    P0    A(NEXT AVAILABLE HEADLINE)                             *         
*        OPTPRTBF   CONTAINS OPTIONS TO PRINT                         *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DSPOPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SUBSYSD,R9                                                       
         USING T830FFD,RA                                                       
*                                                                               
         MVC   DOPSTRTA,0(R1)      SAVE START OF AREA FOR PRINTING              
*                                                                               
         ICM   R5,15,OPTPRTBA      POINT TO OPTIONS PRINT BUFFER                
         BZ    DSPOPTX             SKIP IF NO BUFFER AVAILABLE                  
         USING OPTPRTD,R5          ESTABLISH OPTIONS BUFFER                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,OPPHENTN       GET NUMBER OF OPTIONS TO PRINT               
         BZ    DSPOPTX             SKIP IF NONE                                 
*                                                                               
*        DETERMINE WIDTH OF PRINT AREA AND DISPLACEMENT                         
*                                                                               
         LA    R2,38               FOR 132 LINE - WIDTH                         
         LA    R3,47                            - DISPLACEMEMNT                 
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+12                                                             
         LA    R2,54               FOR 165 LINE - WIDTH                         
         LA    R3,56                            - DISPLACEMENT                  
*                                                                               
         CLI   NARROPT,C'Y'                                                     
         BNE   *+12                                                             
         LA    R2,25               FOR 80  LINE - WIDTH                         
         LA    R3,30                            - DISPLACEMENT                  
*                                                                               
         A     R3,DOPSTRTA         A(OF FIRST OPTION)                           
*                                                                               
         LA    R6,OPPTAB           ESTABLISH FIRST ENTRY IN TABLE               
         USING OPPENTD,R6                                                       
*                                                                               
DOPLOOP  DS    0H                                                               
*                                                                               
*        BUILD OPTION DESCRIPTION IN BLOCK                                      
*                                                                               
         MVC   BLOCK(80),SPACES    INIT BLOCK                                   
*                                                                               
         LA    R1,BLOCK            PRINT POINTER                                
*                                                                               
         CLC   OPPTTL,SPACES       SKIP IF NO TITLE                             
         BNH   DOPLP10                                                          
*                                                                               
         MVC   0(L'OPPTTL,R1),OPPTTL   TRY USING LARGE TITLE                    
         MVI   L'OPPTTL(R1),C':'                                                
         LA    R1,L'OPPTTL+2(R1)                                                
*                                                                               
DOPLP10  DS    0H                                                               
*                                                                               
         MVC   0(L'OPPCODE,R1),OPPCODE  CODE                                    
         LA    R1,L'OPPCODE+1(R1)                                               
*                                                                               
         MVC   0(L'OPPNAME,R1),OPPNAME  NAME                                    
*                                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,80   GET RID OF EXTRA BLANKS                 
*                                                                               
         ICM   RF,15,4(R1)         GET RETURNED LENGTH                          
         BZ    DOPCONT             SKIP IF ALL BLANKS                           
*                                                                               
         LA    R1,BLOCK-1(RF)      POINT TO LAST POSITION                       
*                                                                               
         CLI   0(R1),C':'          IF LAST IS COLON THEN NO CDE OR NAME         
         BNE   *+16                                                             
         MVI   0(R1),C' '             KILL COLON                                
         SH    RF,=H'2'               DECREMENT LENGTH                          
         BNP   DOPCONT                   MUST HAVE SOMETHING LEFT               
*                                                                               
         CR    RF,R2               IF SQUASHED LENGTH TOO LONG                  
         BNH   DOPSQUX                REDO WITH SHORT TITLE                     
*                                                                               
         MVC   BLOCK(80),SPACES    INIT BLOCK                                   
*                                                                               
         LA    R1,BLOCK            PRINT POINTER                                
*                                                                               
         CLC   OPPTTLSH,SPACES     SKIP IF NO TITLE                             
         BNH   DOPLP30                                                          
*                                                                               
         MVC   0(L'OPPTTLSH,R1),OPPTTLSH   TRY USING SHORT TITLE                
         MVI   L'OPPTTLSH(R1),C':'                                              
         LA    R1,L'OPPTTLSH+2(R1)                                              
*                                                                               
DOPLP30  DS    0H                                                               
*                                                                               
         MVC   0(L'OPPCODE,R1),OPPCODE  CODE                                    
         LA    R1,L'OPPCODE+1(R1)                                               
*                                                                               
         MVC   0(L'OPPNAME,R1),OPPNAME  NAME                                    
*                                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,80   GET RID OF EXTRA BLANKS                 
*                                                                               
         ICM   RF,15,4(R1)         GET RETURNED LENGTH                          
         BZ    DOPCONT             SKIP IF ALL BLANKS                           
*                                                                               
         LA    R1,BLOCK-1(RF)      POINT TO LAST POSITION                       
*                                                                               
         CLI   0(R1),C':'          IF LAST IS COLON THEN NO CDE OR NAME         
         BNE   *+16                                                             
         MVI   0(R1),C' '             KILL COLON                                
         SH    RF,=H'2'               DECREMENT LENGTH                          
         BNP   DOPCONT                   MUST HAVE SOMETHING LEFT               
*                                                                               
DOPSQUX  DS    0H                                                               
*                                                                               
         CR    RF,R2               DEFAULT TO MAX LENGTH FOR PRINTAREA          
         BNH   *+6                                                              
         LR    RF,R2                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BLOCK       MOVE TO PRINT AREA                           
*                                                                               
         GOTO1 CENTER,DMCB,(R3),(R2) CENTER OPTION                              
*                                                                               
         A     R3,PWIDTH           BUMP TO NEXT LINE                            
*                                                                               
DOPCONT  DS    0H                                                               
*                                                                               
         LA    R6,OPPENTLQ(R6)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,DOPLOOP          PRINT NEXT OPTION                            
*                                                                               
DOPDONE  DS    0H                                                               
*                                                                               
DSPOPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
DOPSTRTA DS    A                   A(START OF PRINT AREA)                       
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - DATELSTD'          
***********************************************************************         
*                                                                     *         
*        DSECT FOR DATELISTS FOUND IN DATEAREA                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RENWRDLSTD                                                     
*                                                                               
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES -RENWRWORKD'         
***********************************************************************         
*                                                                     *         
*              WRITER WORKING STORAGE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RENWRWORKD                                                     
*                                                                               
           TITLE 'T000AA9 - REPPAK WRITER OFFLINE ROUTINES - DSECTS'            
***********************************************************************         
*                                                                     *         
*              OTHER DSECTS ARE HIDDEN IN HERE                        *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*CTGENDIC                                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENDIC                                                       
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*RENWRFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE RENWRFFD                                                       
         PRINT ON                                                               
*DDGENTWA                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*RENWRF1D                                                                       
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RENWRF1D                                                       
         PRINT ON                                                               
         EJECT                                                                  
T830FFD  DSECT                                                                  
         ORG   CONHEADH-64+X'3000'                                              
HELPSAVE DS    XL512               HELP SAVEAREA                                
*                                                                               
*RENWREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE RENWREQUS                                                      
         PRINT ON                                                               
*PRVALTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRVALTABD                                                      
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*DRGLOBAL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*REGENALL1                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
         PRINT ON                                                               
*REGENPWC                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENPWC                                                       
         PRINT ON                                                               
*REGENSET                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
*REGENDCT                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
*REGENDSP                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENDSP                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142RENWROFFS 10/07/02'                                      
         END                                                                    
