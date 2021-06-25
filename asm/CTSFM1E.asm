*          DATA SET CTSFM1E    AT LEVEL 039 AS OF 05/12/16                      
*PHASE TA0A1EA                                                                  
         TITLE 'TA0A1E - CTSFM1E - ALPHA MARKET LIST/MAINT'                     
***********************************************************************         
*                                                                     *         
*      CTSFM1E (TA0A1E) - MAINTENANCE/LIST OF ALPHA MARKET RECORDS    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* MOD LOG:                                                            *         
* --------                                                            *         
*                                                                     *         
* 12MAR92  (EFJ) --- INITIAL ENTRY                                    *         
*                                                                     *         
* 18MAR92  (EFJ) --- ADD BOOK TYPE FIELD TO KEY (PER ZEN)             *         
*                                                                     *         
* 24JUL92  (GH ) --- LIST SHOWS MARKET NAMES                          *         
*                                                                     *         
* 27OCT92  (GH ) --- MAKE MARKET NUMBER 4 CHARS                       *         
*                                                                     *         
* 17JUN93  (GL ) --- ADD SRC TO VKSRC CODE                            *         
*                                                                     *         
* 24OCT95  (GL ) --- ALLOW MEDIA=W FOR WEEKLY TIME PERIOD             *         
*                                                                     *         
* 27JUN96  (ZEN) --- ALLOW MEDIA=C FOR CANADIAN SUPPORT               *         
*                     (FORCE BBM TO ARB)                              *         
*                                                                     *         
***********************************************************************         
TA0A1E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A1E**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    TMPBLOCK,TMPBLOCK   INITIALIZE DBLOCK                            
         XC    TMPBLOCK+256(256),TMPBLOCK+256                                   
*                                                                               
         LA    R5,TMPBLOCK         SET UP DBLOCK                                
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   DEMAND,CDEMAND                                                   
         DROP  RE                                                               
         MVC   DBFILE,=C'TP'                                                    
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBGETMK                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE REC?                                
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,LISTRECS       DISPLAY RECORD?                              
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    LR                                                               
*                                                                               
NEQXIT   LA    R1,1                                                             
         B     *+6                                                              
EQXIT    SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VK: VALIDATE KEY                                                    *         
***********************************************************************         
VK       DS    0H                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
* VALIDATE MEDIA                                                                
VKMED    DS    0H                                                               
         LA    R2,SFMMEDH                                                       
         TM    4(R2),X'20'         PRE-VALID                                    
         BNZ   VKSRC                                                            
         XC    MEDIA,MEDIA                                                      
         CLI   ACTNUM,ACTLIST      MEDIA NOT REQ'D FOR LIST/PRINT               
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VKSRC                                                            
         GOTO1 ANY                                                              
         CLI   WORK,C'R'           RADIO                                        
         BE    *+8                                                              
         CLI   WORK,C'T'           T.V.                                         
         BE    *+8                                                              
         CLI   WORK,C'W'           WEEKLY TIME PERIOD                           
         BE    *+8                                                              
         CLI   WORK,C'C'           CANADIAN TV                                  
         BE    *+8                                                              
         BNE   ERREXIT                                                          
         MVC   MEDIA,WORK                                                       
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* VALIDATE SOURCE                                                               
VKSRC    DS    0H                                                               
         LA    R2,SFMSRCH                                                       
         TM    4(R2),X'20'         PRE-VALID?                                   
         BNZ   VKMKT                                                            
         XC    SOURCE,SOURCE                                                    
         CLI   ACTNUM,ACTLIST      SOURCE NOT REQ'D FOR LIST/PRINT              
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VKMKT                                                            
         GOTO1 ANY                                                              
*                                                                               
         CLI   MEDIA,C'C'          CANADIAN TV                                  
         BNE   *+8                                                              
         CLI   WORK,C'B'           BBM?                                         
         BNE   *+8                                                              
         MVI   WORK,C'A'           SWITCH TO ARB TO SYNC FILE                   
*                                                                               
         CLI   WORK,C'C'           COMSCORE?                                    
         BE    VKSRCX                                                           
         CLI   WORK,C'S'           SRC?                                         
         BE    VKSRCX                                                           
         CLI   WORK,C'M'           BBM RADIO OR MEDIAFAX TV                     
         BE    VKSRCX                                                           
         CLI   WORK,C'A'           ARBITRON?                                    
         BE    *+12                                                             
         CLI   WORK,C'N'           NIELSON?                                     
         BNE   ERREXIT                                                          
VKSRCX   MVC   SOURCE,WORK                                                      
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* VALIDATE MARKET                                                               
VKMKT    DS    0H                                                               
         LA    R2,SFMMKTH                                                       
         TM    4(R2),X'20'         PRE-VALID?                                   
         BNZ   VKBKT                                                            
         XC    MKT,MKT                                                          
         CLI   ACTNUM,ACTLIST      MARKET NOT REQ'D FOR LIST/PRINT              
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VKBKT                                                            
         GOTO1 ANY                                                              
         CLI   5(R2),2             MKT MUST BE 2 OR 3 CHARS                     
         BL    ERREXIT                                                          
         TM    4(R2),X'04'         ALPHA ONLY                                   
         BZ    ERREXIT                                                          
         MVC   MKT,WORK                                                         
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* VALIDATE BOOKTYPE                                                             
VKBKT    DS    0H                                                               
         LA    R2,SFMBKTH                                                       
         TM    4(R2),X'20'         PRE-VALID                                    
         BNZ   VKNUM                                                            
         MVI   BKTYPE,0                                                         
         CLI   5(R2),0             BOOKTYPE NOT REQ'D - DEFAULT X'FF'           
         BNE   *+12                                                             
         MVI   BKTYPE,X'FF'                                                     
         B     VKNUM                                                            
         GOTO1 ANY                                                              
*                                                                               
         L     RF,ACOMFACS         VALIDATE BOOKTYPE                            
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         CLI   5(R2),1             IF 1-CHARACTER BOOKTYPE ENTERED...           
         BNE   VKBKT10                                                          
         MVI   WORK+1,C' '         ...PAD IT WITH A BLANK                       
*                                                                               
VKBKT10  DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BE    ERREXIT                                                          
*                                                                               
         CLC   SPBKTYPA,WORK       IS BOOKTYPE IN TABLE?                        
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     VKBKT10                                                          
*                                                                               
         MVC   BKTYPE,SPBKTYPN     SAVE INTERNAL BOOKTYPE                       
         DROP  RF                                                               
*                                                                               
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* VALIDATE MARKET NUM                                                           
VKNUM    DS    0H                                                               
         LA    R2,SFMNUMH                                                       
         TM    4(R2),X'20'         PRE-VALID?                                   
         BNZ   VKX                                                              
         XC    BINMKT,BINMKT                                                    
         ZICM  R1,5(R2),1          MUST BE SOME INPUT                           
         BNZ   *+24                                                             
         CLI   ACTNUM,ACTLIST      MK NUM NOT REQ'D FOR LIST/PRINT              
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                                                              
         B     ERREXIT                                                          
         TM    4(R2),X'08'         NUMERIC ONLY                                 
         BZ    ERREXIT                                                          
         BCTR  R1,0                R1 HAS INPUT LENGTH                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMNUM(0)                                                    
         CVB   R1,DUB                                                           
         STH   R1,BINMKT                                                        
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* BUILD KEY FOR GENCON AND EXIT                                                 
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTDMREC,R4                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,MEDIA                                                   
         MVC   CTDMKSRC,SOURCE                                                  
         MVC   CTDMKMKT,MKT                                                     
         MVC   CTDMKBKT,BKTYPE                                                  
         MVC   CTDMKNUM,BINMKT                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VR: VALIDATE RECORD                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         L     R4,AIO                                                           
         USING CTDMREC,R4                                                       
         XC    0(29,R4),0(R4)                                                   
*                                                                               
         MVC   0(25,R4),KEY                                                     
         MVC   CTDMLEN,=H'29'                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DR: DISPLAY RECORD                                                  *         
***********************************************************************         
DR       DS    0H                                                               
         L     R4,AIO                                                           
         USING CTDMREC,R4                                                       
*                                  GET MARKET NAME FROM DEMAND                  
         XC    SFMMKTN,SFMMKTN                                                  
         OI    SFMMKTNH+6,X'80'                                                 
*                                  GET MARKET NAME FROM DEMAND                  
         MVC   DBSELMED,CTDMKMED                                                
         MVC   DBSELSRC,CTDMKSRC                                                
         MVC   DBSELRMK,CTDMKNUM                                                
         CLI   CTDMKSRC,C'C'       COMSCORE?                                    
         JNE   DR05                                                             
         MVI   DBSELSRC,C'N'       DEFAULT TO NIELSON                           
         SR    RF,RF                                                            
         ICM   RF,3,CTDMKNUM       SUBTRACT OFF 400 FOR COMSCORE                
         S     RF,=F'400'                                                       
         STCM  RF,3,DBSELRMK                                                    
*                                                                               
DR05     BAS   RE,SWTCHSPT                                                      
         BNE   DR10                                                             
         MVC   DATADISP,=H'23'     FOR ARA RECORDS                              
         GOTO1 DEMAND,DMCB,DBLOCK,NAMEHOOK                                      
         MVC   SFMMKTN,WORK        RETURNED MARKET NAME                         
         BAS   RE,SWTCHCTL                                                      
         MVC   DATADISP,=H'28'                                                  
*                                                                               
DR10     DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DK: DISPLAY KEY                                                     *         
***********************************************************************         
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING CTDMREC,R4                                                       
         FOUT  SFMMEDH,CTDMKMED                                                 
         FOUT  SFMSRCH,CTDMKSRC                                                 
         FOUT  SFMMKTH,CTDMKMKT                                                 
         EDIT  CTDMKNUM,(4,SFMNUM),ALIGN=L                                      
         FOUT  SFMNUMH                                                          
*                                                                               
         CLI   CTDMKBKT,X'FF'      ANY BOOKTYPE?                                
         BE    DKX                                                              
*                                                                               
         L     RF,ACOMFACS         LOOK UP BOOKTYPE                             
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
DK10     DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID BOOKTYPE IN RECORD                   
*                                                                               
         CLC   SPBKTYPN,CTDMKBKT   MATCH ON BOOKTYPE?                           
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     DK10                                                             
*                                                                               
         MVC   SFMBKT,SPBKTYPA     DISPLAY ALPHA BOOKTYPE                       
         OI    SFMBKTH+6,X'80'                                                  
         DROP  RF                                                               
*                                                                               
DKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LR: LIST RECS & PRINT REPORT                                        *         
***********************************************************************         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING CTDMREC,R4                                                       
         OC    KEY(25),KEY         TEST FIRST TIME THRU                         
         BNZ   LR10                                                             
         MVI   CTDMKTYP,CTDMKTEQ   BUILD KEY                                    
         MVI   CTDMKTY2,CTDMKT2E                                                
*                                                                               
* SAVE MKT NUM INPUT IN BIN                                                     
         XC    BINMKT,BINMKT                                                    
         CLI   SFLNUMH+5,0         PICK UP NUMBER IF PRESENT                    
         BE    LR5                                                              
         LLC   R1,SFLNUMH+5        GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMNUM(0)                                                    
         CVB   R1,DUB                                                           
         STH   R1,BINMKT                                                        
*                                                                               
* PICK UP FIRST FIELDS INPUT FOR START AT                                       
LR5      DS    0H                                                               
         CLI   SFLMEDH+5,0         PICK UP MEDIA IF PRESENT                     
         BE    LR10                                                             
         MVC   CTDMKMED,SFLMED                                                  
         CLI   SFLSRCH+5,0         PICK UP SOURCE IF PRESENT                    
         BE    LR10                                                             
         MVC   CTDMKSRC,SFLSRC                                                  
         CLI   SFLMKTH+5,0         PICK UP MARKET NAME IF PRESENT               
         BE    LR10                                                             
         MVC   CTDMKMKT,SFLMKT                                                  
         MVC   CTDMKNUM,BINMKT                                                  
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     LR30                                                             
         DROP  R4                                                               
*                                                                               
LR20     GOTO1 SEQ                                                              
*                                                                               
* FILTER ON FIELDS INPUT                                                        
LR30     DS    0H                                                               
         L     R4,AIO                                                           
         USING CTDMREC,R4                                                       
         CLC   =C'DM',0(R4)        STILL ON RIGHT KEY TYPE?                     
         BNE   LRX                                                              
*                                                                               
         CLI   SFLMEDH+5,0         FILTER ON MEDIA                              
         BE    *+14                                                             
         CLC   CTDMKMED,SFLMED                                                  
         BNE   LR20                                                             
*                                                                               
         CLI   SFLSRCH+5,0         FILTER ON SOURCE                             
         BE    *+14                                                             
         CLC   CTDMKSRC,SFLSRC                                                  
         BNE   LR20                                                             
*                                                                               
         ZICM  R1,SFLMKTH+5,1      FILTER ON MARKET NAME                        
         BZ    LR35                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTDMKMKT(0),SFLMKT                                               
         BNE   LR20                                                             
*                                                                               
LR35     DS    0H                                                               
         CLI   BKTYPE,X'FF'        ANY BOOKTYPE FILTER?                         
         BE    *+14                                                             
         CLC   CTDMKBKT,BKTYPE     YES: HONOR IT                                
         BNE   LR20                                                             
*                                                                               
         OC    BINMKT,BINMKT       FILTER ON MKT NUM                            
         BZ    *+14                                                             
         CLC   CTDMKNUM,BINMKT                                                  
         BNE   LR20                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
*                                                                               
         MVC   LMED,CTDMKMED                                                    
         MVC   LSRC,CTDMKSRC                                                    
         MVC   LMKT,CTDMKMKT                                                    
         EDIT  CTDMKNUM,(4,LNUM),ALIGN=L                                        
*                                                                               
         CLI   CTDMKBKT,X'FF'      NO BOOKTYPE?                                 
         BE    LR38                                                             
*                                                                               
         L     RF,ACOMFACS         LOOK UP BOOKTYPE                             
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
LR36     DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID BOOKTYPE IN RECORD                   
*                                                                               
         CLC   SPBKTYPN,CTDMKBKT   MATCH ON BOOKTYPE?                           
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     LR36                                                             
*                                                                               
         MVC   LBKT,SPBKTYPA       ALPHA BOOKTYPE                               
         DROP  RF                                                               
*                                                                               
LR38     DS    0H                                                               
*---------------------------------------------------------------                
* GET MARKET NAME FROM DEMAND                                                   
*---------------------------------------------------------------                
         MVC   DBSELMED,CTDMKMED                                                
         MVC   DBSELSRC,CTDMKSRC                                                
         MVC   DBSELRMK,CTDMKNUM                                                
         CLI   CTDMKSRC,C'C'       COMSCORE?                                    
         JNE   LR38A                                                            
         MVI   DBSELSRC,C'N'       DEFAULT TO NIELSON                           
         SR    RF,RF                                                            
         ICM   RF,3,CTDMKNUM       SUBTRACT OFF 400 FOR COMSCORE                
         S     RF,=F'400'                                                       
         STCM  RF,3,DBSELRMK                                                    
*                                                                               
LR38A    MVC   LMKTNAME,=CL30' '                                                
*                                                                               
         BAS   RE,SWTCHSPT                                                      
         BNE   LR39                                                             
         MVC   DATADISP,=H'23'     FOR ARA RECORDS                              
         GOTO1 DEMAND,DMCB,DBLOCK,NAMEHOOK                                      
         MVC   LMKTNAME,WORK       RETURNED MARKET NAME                         
         BAS   RE,SWTCHCTL                                                      
         MVC   DATADISP,=H'28'                                                  
*                                                                               
LR39     CLI   ACTNUM,ACTLIST                                                   
         BNE   LR40                                                             
*                                                                               
         GOTO1 LISTMON             FOR LIST                                     
         B     LR50                                                             
*                                                                               
LR40     DS    0H                  FOR PRINTREP                                 
         MVC   P(LISTQ),LISTAR                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR50     DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,KEY+23         GET MARKET #                                 
         LA    R1,1(R1)            GET NEXT ONE                                 
         STCM  R1,3,KEY+23         SINCE DEMAND DESTROYS SEQ READ               
         B     LR10                CHANGE KEY, THEN DO READ HIGH                
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*------------------------------------------------------------------             
NAMEHOOK NTR1                                                                   
* RETURN MARKET NAME IN FIELD 'WORK'                                            
         XC    WORK,WORK                                                        
         L     R6,DBAREC                                                        
         MVI   ELCODE,1            GET MARKET ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   EXIT                CAN'T FIND IT, LEAVE AS SPACES               
         LLC   R1,1(R6)            GET LENGTH OF ELEMENT                        
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R6)       COPY NAME TO OUTPUT                          
         B     EXIT                                                             
*------------------------------------------------------------------             
SWTCHSPT NTR1                                                                   
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CSWITCH-COMFACSD(RF)                                       
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   4(R1),0                                                          
         BE    EQXIT                                                            
         CLI   4(R1),2             WAS SYSTEM CHANGED BUT NOT OPEN?             
         BE    SWTCHCT1              SWITCH BACK                                
         B     NEQXIT                                                           
*------------------------------------------------------------------             
SWTCHCTL NTR1                                                                   
SWTCHCT1 L     RF,ACOMFACS                                                      
         ICM   RF,15,CSWITCH-COMFACSD(RF)                                       
         GOTO1 (RF),DMCB,=C'CTL',0                                              
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DUMP IF CAN'T GET BACK TO CONTROL            
         B     NEQXIT                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE CTSFMFFD          (BASE SCREEN FOR SYSTEM)                     
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMDED          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMEED          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD        (SYSTEM AREAS)                               
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
BINMKT   DS    H                   SAVE BINARY MKT NUM                          
MEDIA    DS    C                                                                
SOURCE   DS    C                                                                
MKT      DS    CL3                                                              
BKTYPE   DS    C                                                                
DEMAND   DS    A                                                                
TMPBLOCK DS    2CL256              NEED THIS FOR DBLOCK                         
*                                                                               
* ONLINE LIST LINE/OFFLINE REPORT                                               
*                                                                               
LISTD    DSECT                                                                  
         DS    CL2                                                              
LMED     DS    CL1                                                              
         DS    CL6                                                              
LSRC     DS    CL1                                                              
         DS    CL7                                                              
LMKT     DS    CL3                                                              
         DS    CL7                                                              
LBKT     DS    CL2                                                              
         DS    CL7                                                              
LNUM     DS    CL4                                                              
         DS    CL2                                                              
LMKTNAME DS    CL30                                                             
LISTQ    EQU   *-LISTD                                                          
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CTSFM1E   05/12/16'                                      
         END                                                                    
