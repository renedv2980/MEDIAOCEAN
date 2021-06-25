*          DATA SET SPAUTH     AT LEVEL 011 AS OF 02/07/03                      
*PHASE T00AB7A                                                                  
***********************************************************************         
* PROGRAMS THAT CALL SPAUTH:                                                    
* CTMAD26   - TO ADD STATION LEVEL AUTHS WHEN BUYS ADDED                        
* SPBUY00   - TO ADD STATION LEVEL AUTHS WHEN BUYS ADDED                        
* SPNWS04   - TO ADD MKT LEVEL AUTHS WHEN WORK RECORDS ADDED                    
* SPNWS11   - TO ADD MKT LEVEL AUTHS WHEN CAMPAIGNS COPIED                      
* SPNWS07   - TO ADD STATION LEVEL AUTHS WHEN BUYS ADDED (XFER)                 
* SPNWS37   - TO ADD STATION LEVEL AUTHS WHEN BUYS ADDED (XFER)                 
* SPGOL03   - TO ADD MARKET LEVEL AUTHS WHEN GOALS ADDED                        
* SPGOL04   - TO ADD MARKET LEVEL AUTHS WHEN GOALS ADDED                        
* SPDAR09   - TO UPDATE DARE ORDERED DATE IN STATION LEVEL AUTHS                
* SPOMS09   - TO UPDATE DARE ORDERED DATE IN STATION LEVEL AUTHS                
* SPOMS0B   - TO UPDATE DARE CONFIRMED DATE IN STATION LEVEL AUTHS              
* SPREPSU02 - TO UPDATE DARE CONFIRMED DATE IN STATION LEVEL AUTHS &            
*             WHEN AUTHORIZATION IS ADDED OR FLIGHT DATES EXPANDED,             
*             CHECKS FOR BUYS TO ADD STATION LEVEL AUTHORIZATIONS               
* SPREPML02 - TO UPDATE LAST DATE ML REPORT RAN                                 
* SPWRI27   - TO UPDATE LAST DATE BG REPORT RAN                                 
* SPREPD202 - TO UPDATE ORDER SENT DATE WHEN DX REPORT RAN                      
***********************************************************************         
         TITLE 'SPAUTH - SPOTPAK EDIT AUTH RECORDS FOR SUPERDESK'               
SPAUTH   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*SPAUTH*,RA,RR=RE,CLEAR=YES                                
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         ST    R1,SAVER1                                                        
         MVC   AUTHBLK,0(R1)                                                    
         USING SPAUTHD,AUTHBLK                                                  
         MVI   SPAERR,0                                                         
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
         BRAS  RE,VALPARM          VALIDATE INPUT PARAMETERS                    
*                                                                               
         CLI   WHATIDO,IDOGOAL     DOING GOAL TYPE AUTHS?                       
         BNE   *+12                NO                                           
         BRAS  RE,GLAUTH                                                        
         B     EXITBACK                                                         
*                                                                               
         CLI   WHATIDO,IDONWS      DOING NWS TYPE AUTHS?                        
         BNE   *+12                NO                                           
         BRAS  RE,GLAUTH                                                        
         B     EXITBACK                                                         
*                                                                               
         CLI   WHATIDO,IDODARE     DOING DARE TYPE AUTHS?                       
         BNE   *+12                NO                                           
         BRAS  RE,DAAUTH                                                        
         B     EXITBACK                                                         
*                                                                               
         CLI   WHATIDO,IDOTBL      DOING TABLE BUILD?                           
         BNE   *+12                NO                                           
         BRAS  RE,TBBLD                                                         
         B     EXITBACK                                                         
*                                                                               
         DC    H'0'                THIS IS WHAT I THINK OF DARE...              
         EJECT                                                                  
********************************************************************            
* SUBROUTINE TO ADD OR UPDATE AUTHORIZATION MARKET RECORDS FOR     *            
* SUPERDESK                                                        *            
********************************************************************            
         SPACE 1                                                                
GLAUTH   NTR1                                                                   
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                                                          
K        USING AUTRECD,KEY                                                      
         MVI   K.AUTKTYP,AUTKTYQQ      '0D39'                                   
         MVI   K.AUTKSUB,AUTKSUBQ                                               
         MVC   K.AUTKAM,SPAKAM                                                  
         MVC   K.AUTKCLT,SPAKCLT                                                
         MVC   K.AUTKPRD,SPAKPRD                                                
         MVC   K.AUTKPRD2,SPAKPRD2                                              
         MVC   K.AUTKEST,SPAKEST                                                
         B     GLA04                                                            
*                                                                               
GLA02    MVC   KEY(AUTKMKT-AUTKEY),SVKEY    GET NEXT VERSION                    
         MVC   K.AUTKMKT(5),EFFS                                                
*                                                                               
GLA04    GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                 
         BE    GLA06                                                            
*                                                                               
         MVC   KEY,KEYSAVE         IF NOT FOUND, READ FOR PRD POL               
         MVI   K.AUTKPRD,X'FF'                                                  
         MVI   K.AUTKPRD2,0                                                     
         GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                 
         BNE   EXITOK              FINISHED                                     
*                                                                               
GLA06    MVC   SVKEY(AUTKMKT-AUTKEY),KEY                                        
         MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
* COMPARE GOAL DATE WITH BUY FLIGHT DATES FROM LATEST REVISION                  
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   AUDEL,AUDELQ        MAKE SURE RECORD LOOKS OK                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   AUDFLST,EDTYMD      START DATE > END DATE?                       
         BH    GLA02               YES - NOT IN PERIOD                          
         CLC   AUDFLEN,SDTYMD      END DATE < START DATE                        
         BL    GLA02               YES - NOT IN PERIOD                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,AUDFLST),(2,FULL)  START DATE                     
         GOTO1 (RF),(R1),(3,AUDFLEN),(2,FULL+2)  END DATE                       
*                                                                               
         MVC   SVDUDT,AUDDUEDT     SAVE DUE DATE                                
*                                                                               
         LA    R6,AUTEL            A(FIRST ELEMENT)                             
         USING AINFELD,R6                                                       
         XR    RF,RF                                                            
GLA08    CLI   AINFEL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AINFEL,AINFELQ                                                   
         BE    *+12                                                             
         IC    RF,1(R6)                                                         
         BXH   R6,RF,GLA08                                                      
*                                                                               
         TM    AINFFLAG,AINFRVNA   A REV CHNGED DUE DATE FOR SPEC MKT?          
         BNO   GLA10               NO, USE CURRENT DUE DATE                     
*                                                                               
         XC    KEY,KEY             YES, GET ORIGINAL DUE DATE                   
         MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY                                   
         MVI   K.AUTKREV,X'99'                                                  
         GOTO1 AIO,IOREAD+IOLOCK                                                
         MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         MVC   SVDUDT,AUDDUEDT                                                  
         DROP  R6                                                               
*                                                                               
* GOAL DATES OVERLAP WITH BUY FLIGHT DATES, SO ADD AUTH/MKT RECORDS             
*                                                                               
GLA10    XC    KEY,KEY                                                          
         MVC   KEY(AUTKMKT-AUTKEY),SVKEY                                        
         MVC   K.AUTKMKT,SPAKMKT                                                
         GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY,KEYSAVE    MKT LEVEL EXISTS?                            
         BE    GLA20               YES - UPDATE GOAL INPUT DATE                 
*                                                                               
         CLI   SPAUPDT,SPAUPDBG    UPDATE BG DATE?                              
         BE    GLA02               YES, SO DON'T ADD MARKET RECORD              
         CLI   SPAUPDT,SPAUPDML    UPDATE ML DATE?                              
         BE    GLA02               YES, SO DON'T ADD MARKET RECORD              
*                                                                               
         L     R0,SPAIO            CLEAR OUT I/O AREA                           
         LHI   R1,1000                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,SPAIO                                                         
REC      USING AUTRECD,R6                                                       
         MVC   REC.AUTKEY(AUTKSTA-AUTKEY),KEYSAVE                               
         MVC   K.AUTKEY(AUTKSTA-AUTKEY),KEYSAVE                                 
         LA    R3,REC.AUTEL                                                     
         USING MINFEL,R3                                                        
         MVI   MINFEL,MINFELQ            X'01' ELEMENT                          
         MVI   MINFLEN,MINFLNQ           ELEMENT LENGTH                         
         MVI   MINFRVNM,X'99'            REV. 99 (NO REV APPLIED YET)           
         MVC   MINFRVDT,TDYYMD           REVISION DATE = TODAY'S DATE           
         MVC   MINFDUDT,SVDUDT           DUE DATE                               
*                                                                               
         CLI   SPAUPDT,SPAUPGOL          UPDATE GOAL DATE                       
         BNE   *+10                      NO                                     
         MVC   MINFGIDT,TDYYMD           GOAL INPUT DATE = TODAY'S DATE         
         CLI   SPAUPDT,SPAUPXFR          UPDATE NWS TRANSFER DATE?              
         BNE   *+10                                                             
         MVC   MINFNXDT,XDTYMD           LAST XFR DATE                          
         B     *+12                      XFRS UPDATE WORK DATE ALSO             
         CLI   SPAUPDT,SPAUPDWK          UPDATE WORK ADD DATE?                  
         BNE   *+10                                                             
         MVC   MINFWKDT,WDTYMD           FIRST WORK REC ADDED                   
*                                                                               
         AHI   R3,MINFLNQ                                                       
         MVI   0(R3),0                                                          
         S     R3,SPAIO                                                         
         STCM  R3,3,REC.AUTRLEN                                                 
         DROP  R3,REC                                                           
*                                                                               
         XC    IODA,IODA                                                        
         TM    SPAFLAG,SPAFNOWR    WRITE=NO?                                    
         BO    GLA12                                                            
         GOTO1 AIO,IOADD+IOLOCK                                                 
GLA12    B     GLA30                                                            
*                                                                               
GLA20    MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   MINFEL,MINFELQ                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   SPAUPDT,SPAUPGOL    UPDATE GOAL DATE                             
         BNE   GLA21               NO                                           
         MVC   MINFGCDT,TDYYMD     GOAL CHNGE DATE = TODAY'S DATE               
         OC    MINFGIDT,MINFGIDT   GOAL INPUT DATE THERE YET?                   
         BNZ   GLA21               YES                                          
         MVC   MINFGIDT,TDYYMD     NO, GOAL INPUT DATE = TODAY'S DATE           
*                                                                               
GLA21    CLI   SPAUPDT,SPAUPDBG    UPDATE BG DATE?                              
         BNE   *+10                NO                                           
         MVC   MINFBGDT,TDYYMD     BG RUN DATE = TODAY'S DATE                   
*                                                                               
         CLI   SPAUPDT,SPAUPDML    UPDATE ML DATE?                              
         BNE   *+10                NO                                           
         MVC   MINFMLDT,TDYYMD     ML RUN DATE = TODAY'S DATE                   
*                                                                               
         CLI   SPAUPDT,SPAUPXFR    UPDATE NWS TRANSFER DATE?                    
         BNE   GLA22                                                            
         CLC   MINFNXDT,XDTYMD     UPDATE IF NEW DATE LATER                     
         BNL   GLA24               XFRS UPDATE WORK DATE ALSO                   
         MVC   MINFNXDT,XDTYMD     LAST XFR DATE                                
         B     GLA24               XFRS UPDATE WORK DATE ALSO                   
*                                                                               
GLA22    CLI   SPAUPDT,SPAUPDWK    UPDATE WORK ADD DATE?                        
         BNE   GLA28                                                            
GLA24    OC    WDTYMD,WDTYMD       WORK ADD DATE SENT?                          
         BZ    GLA28               NO                                           
         OC    MINFWKDT,MINFWKDT   UPDATE IF CURRENTLY NO DATE                  
         BZ    GLA26                                                            
         CLC   MINFWKDT,WDTYMD     OR IF NEW DATE EARLIER                       
         BNH   GLA28                                                            
GLA26    MVC   MINFWKDT,WDTYMD     FIRST WORK REC ADDED DATE                    
*                                                                               
GLA28    TM    SPAFLAG,SPAFNOWR    WRITE=NO?                                    
         BO    GLA30                                                            
         GOTO1 AIO,IOPUT+IOLOCK                                                 
         DROP  R6                                                               
*                                                                               
GLA30    CLI   WHATIDO,IDOGOAL     DOING GOAL TYPE AUTHS?                       
         BE    GLA02               YES - CHECK NEXT VERSION                     
*                                                                               
         TM    SPAFLAG,SPAFBUY     CALLED BY SPOT/BUY PROG?                     
         BZ    GLA60               NO                                           
         TM    SPAFLAG,SPAFUPT     AND UPDATE PASSED AUTH TABLE?                
         BZ    GLA60               NO                                           
         ICM   RF,15,SPAFATBL      GET A(TABLE)                                 
         BNZ   *+6                                                              
         DC    H'0'                PASS THE TABLE PLEASE                        
*                                                                               
         LHI   R0,256              MAX ENTRIES IN TABLE (+1)                    
GLA40    CLC   0(4,RF),EFFS                                                     
         BNE   *+6                 FOUND EOT                                    
         DC    H'0'                                                             
         CLC   FULL,0(RF)                                                       
         BE    GLA50                                                            
         AHI   RF,4                                                             
         BCT   R0,GLA40                                                         
         DC    H'0'                WHERE EOT?                                   
*                                                                               
GLA50    MVC   0(4,RF),=XL4'00010001'                                           
*                                                                               
GLA60    XC    KEY,KEY             READ STATION RECORD                          
         MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY                                   
         MVC   K.AUTKMKT,SPAKMKT                                                
         MVC   K.AUTKSTA,SPAKSTA                                                
         GOTO1 AIO,IOHIGH                                                       
*                                                                               
         CLC   K.AUTKEY,KEYSAVE                                                 
         BNE   GLA70               IF STATION RECORD EXISTS, AND                
         TM    SPAFLAG,SPAFUPL     CREATED THROUGH SPOT BUY UPLOAD,             
         BNO   GLA02               THEN UPDATE THE SENT & CNFRMD DATES          
*                                                                               
* ADD STATION ELEMENT WITH SENT AND CONFIRMED STATUS                            
         MVC   IODA,K.AUTKDA       GET STATION RECORD                           
         GOTO1 AIO,IOGET+IOLOCK                                                 
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   SDTLEL,X'01'        CHECK IF ELEMENT THERE ALREADY               
         BNE   GLA65                                                            
*                                                                               
         NI    SDTLFLG,X'FF'-SDTLDXOR-SDTLDROR-SDTLDRCN  TURN OFF FLAGS         
         OI    SDTLFLG,SDTLBUOR+SDTLBUCN  SET SBU CHANGED DATA FLAGS            
*                                                                               
* SET ORDER SENT AND CONFIRMED DATES                                            
         TM    SPAFLAG,SPAFOVR     IF CALLED BY OVERNIGHT PROGRAM               
         BNO   GLA62                                                            
         MVC   SDTLSCNF,XDTYMD     USE UPLOADED DATE FOR CONFIRMED &            
         MVC   SDTLORSN,XDTYMD     ORDER SENT DATE                              
         B     GLA67                                                            
GLA62    MVC   SDTLSCNF,TDYYMD     OTHERWISE USE TODAY'S DATE                   
         MVC   SDTLORSN,TDYYMD                                                  
         B     GLA67                                                            
*                                                                               
GLA65    XC    WORK,WORK                                                        
T        USING SDTLEL,WORK                                                      
         MVI   T.SDTLEL,SDTLELQ                                                 
         MVI   T.SDTLLEN,SDTLLENQ  ELEMENT LENGTH                               
         TM    SPAFLAG,SPAFOVR     IF CALLED BY OVERNIGHT PROGRAM               
         BNO   GLA66                                                            
         MVC   SDTLSCNF,XDTYMD     USE UPLOADED DATE FOR CONFIRMED &            
         MVC   SDTLORSN,XDTYMD     ORDER SENT DATE                              
         B     GLA66A                                                           
GLA66    MVC   SDTLSCNF,TDYYMD     OTHERWISE USE TODAY'S DATE                   
         MVC   SDTLORSN,TDYYMD                                                  
GLA66A   OI    SDTLFLG,SDTLBUOR+SDTLBUCN  SET SBU CHANGED DATA FLAGS            
*                                                                               
         GOTO1 HELLO,DMCB,(=C'P',XSPFIL),(R6),WORK,0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GLA67    TM    SPAFLAG,SPAFNOWR    WRITE=NO?                                    
         BO    GLA68                                                            
         GOTO1 AIO,IOPUT+IOLOCK                                                 
GLA68    B     GLA02                                                            
*                                                                               
GLA70    L     R0,SPAIO            CLEAR OUT I/O AREA                           
         LHI   R1,100                                                           
         XR    RF,RF                                                            
         MVCL  R0,RE               FOR LENGTH OF EMPTY RECORD + SPARE           
*                                                                               
         L     R6,SPAIO            ADD STATION LEVEL AUTH RECORD                
         USING AUTRECD,R6                                                       
         MVC   AUTKEY(AUTKMKT-AUTKEY),SVKEY                                     
         MVC   AUTKMKT,SPAKMKT                                                  
         MVC   AUTKSTA,SPAKSTA                                                  
*                                                                               
         MVC   AUTRLEN,=X'002B'    LENGTH OF RECORD, NO ELEMENTS                
*                                                                               
         TM    SPAFLAG,SPAFUPL     IF CREATED THROUGH SPOT BUY UPLOAD           
         BNO   GLA80                 THEN ADD STATION DETAILS ELEMENT           
         MVI   SDTLEL,SDTLELQ        AND SET ORDER AND CONFIRMED DATES          
         MVI   SDTLLEN,SDTLLENQ                                                 
         AHI   R3,MINFLNQ                                                       
         TM    SPAFLAG,SPAFOVR     IF CALLED BY OVERNIGHT PROGRAM               
         BNO   GLA75                                                            
         MVC   SDTLSCNF,XDTYMD     USE UPLOADED DATE (CONFRMD DATE)             
         MVC   SDTLORSN,XDTYMD     ORDER SENT DATE                              
         B     GLA77                                                            
GLA75    MVC   SDTLSCNF,TDYYMD     OTHERWISE USE TODAY'S DATE                   
         MVC   SDTLORSN,TDYYMD                                                  
*                                                                               
GLA77    OI    SDTLFLG,SDTLBUOR+SDTLBUCN  SET SBU CHANGED DATA FLAGS            
         SR    R3,R3                                                            
         ICM   R3,3,AUTRLEN                                                     
         AH    R3,=Y(SDTLLENQ)                                                  
         STCM  R3,3,AUTRLEN                                                     
*                                                                               
GLA80    TM    SPAFLAG,SPAFNOWR    WRITE=NO?                                    
         BO    GLA90                                                            
         GOTO1 AIO,IOADD+IOLOCK                                                 
GLA90    B     GLA02                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF AN AUTHORIZATION STATION RECORD EXISTS                     *         
***********************************************************************         
         SPACE 1                                                                
DAAUTH   NTR1  ,                                                                
         XC    KEY,KEY             READ AUTHORIZATION RECORD                    
K        USING AUTRECD,KEY                                                      
         MVI   K.AUTKTYP,AUTKTYQQ      '0D39'                                   
         MVI   K.AUTKSUB,AUTKSUBQ                                               
         MVC   K.AUTKAM,SPAKAM                                                  
         MVC   K.AUTKCLT,SPAKCLT                                                
         MVC   K.AUTKPRD,SPAKPRD                                                
         MVC   K.AUTKPRD2,SPAKPRD2                                              
         MVC   K.AUTKEST,SPAKEST                                                
         B     DAA20                                                            
*                                                                               
DAA10    MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY    GET NEXT VERSION               
         MVC   K.AUTKMKT(5),EFFS                                                
*                                                                               
DAA20    GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                 
         BE    DAA30                                                            
*                                                                               
         MVC   KEY,KEYSAVE         IF NOT FOUND, READ FOR PRD POL               
         MVI   K.AUTKPRD,X'FF'                                                  
         MVI   K.AUTKPRD2,0                                                     
         GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                 
         BNE   EXITOK              FINISHED                                     
*                                                                               
DAA30    MVC   SVKEY(L'AUTKEY),KEY                                              
         MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
* COMPARE DATE WITH BUY FLIGHT DATES                                            
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   AUDEL,AUDELQ        MAKE SURE RECORD LOOKS OK                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   AUDFLST,EDTYMD      START DATE > END DATE?                       
         BH    DAA10               YES - NOT IN PERIOD                          
         CLC   AUDFLEN,SDTYMD      END DATE < START DATE                        
         BL    DAA10               YES - NOT IN PERIOD                          
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             READ STATION RECORD                          
         MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY                                   
         MVC   K.AUTKMKT,SPAKMKT                                                
         MVC   K.AUTKSTA,SPAKSTA                                                
         GOTO1 AIO,IOHIGH                                                       
         CLC   K.AUTKEY,KEYSAVE                                                 
         BNE   DAA10               IF NOT READ NEXT VERSION                     
*                                                                               
         MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   SDTLEL,X'01'        IS THERE ALREADY AN ELEMENT?                 
         BNE   DAA60                                                            
*                                                                               
         CLI   SPAUPDT,SPAUPSDX    CALLED BY DX, SET SENT DATE                  
         BNE   DAA40                                                            
         MVC   SDTLORSN,TDYYMD     ORDER SENT DATE                              
         NI    SDTLFLG,X'FF'-SDTLBUOR-SDTLDROR   TURN OFF OLD FLAGS             
         OI    SDTLFLG,SDTLDXOR    SET DX CHANGED DATA FLAG                     
         B     DAA100                                                           
*                                                                               
DAA40    CLI   SPAUPDT,SPAUPDCN    UPDATE DARE CONFRM DATE? (OVERNIGHT)         
         BNE   DAA45                                                            
         NI    SPUPSTAT,X'FF'-SPUPSTCG     NOT CHANGED YET                      
         CLC   SDTLSCNF,WDTYMD     CHECK IF CNF IS LATER                        
         BNL   DAA42                                                            
         MVC   SDTLSCNF,WDTYMD     UPDATE CONFIRMED DATE                        
         OI    SPUPSTAT,SPUPSTCG   COMFIRMED DATE CHANGED                       
         NI    SDTLFLG,X'FF'-SDTLBUCN-SDTLBYCN   TURN OFF OLD FLAGS             
         OI    SDTLFLG,SDTLDRCN    SET DARE CHANGED DATA FLAG                   
DAA42    CLC   SDTLORSN,XDTYMD     CHECK IF SENT DATE IS LATER                  
         BNL   DAA100                                                           
         MVC   SDTLORSN,XDTYMD      IF UPDATE SENT DATE                         
         NI    SDTLFLG,X'FF'-SDTLDXOR-SDTLBUOR  TURN OFF OLD FLAGS              
         OI    SDTLFLG,SDTLDROR    SET DARE CHANGED DATA FLAG                   
         B     DAA100                                                           
*                                                                               
DAA45    CLI   SPAUPDT,SPAUPBCN    UPDATE BYR CONFRM DATE? (OM)                 
         BNE   DAA50                                                            
         NI    SDTLFLG,X'FF'-SDTLBUCN-SDTLDRCN   TURN OFF OLD FLAGS             
         OI    SDTLFLG,SDTLBYCN    SET BUYER CONF CHANGED DATA FLAG             
*                                                                               
DAA47    MVC   SDTLSCNF,TDYYMD     UPDATE CONFIRMED DATE                        
         CLC   SDTLORSN,XDTYMD     CHECK IF DARE SENT DATE IS LATER             
         BNL   DAA100                                                           
         MVC   SDTLORSN,XDTYMD     UPDATE SENT DATE                             
         B     DAA55                                                            
*                                                                               
DAA50    MVC   SDTLORSN,TDYYMD     ORDER DATE                                   
DAA55    NI    SDTLFLG,X'FF'-SDTLDXOR-SDTLBUOR  TURN OFF OLD FLAGS              
         OI    SDTLFLG,SDTLDROR    SET DARE CHANGED DATA FLAG                   
         B     DAA100                                                           
*                                                                               
DAA60    XC    WORK,WORK                                                        
T        USING SDTLEL,WORK                                                      
         MVI   T.SDTLEL,SDTLELQ                                                 
         MVI   T.SDTLLEN,SDTLLENQ  ELEMENT LENGTH                               
*                                                                               
         CLI   SPAUPDT,SPAUPSDX    CALLED BY DX, SET SENT DATE                  
         BNE   DAA70                                                            
         MVC   T.SDTLORSN,TDYYMD   ORDER SENT DATE                              
         OI    T.SDTLFLG,SDTLDXOR  SET DX CHANGED DATA FLAG                     
         B     DAA90                                                            
*                                                                               
DAA70    CLI   SPAUPDT,SPAUPDCN    UPDATE CONFIRMATION DATE - OV                
         BNE   DAA75                                                            
         MVC   T.SDTLSCNF,WDTYMD   SET CONF DATE                                
         OI    T.SDTLFLG,SDTLDRCN  SET DARE CHANGED DATA FLAG                   
         MVC   T.SDTLORSN,XDTYMD   UPDATE SENT DATE ALSO                        
         OI    T.SDTLFLG,SDTLDROR  SET DARE CHANGED DATA FLAG                   
         OI    SPUPSTAT,SPUPSTCG   COMFIRMED DATE CHANGED                       
         B     DAA90                                                            
*                                                                               
DAA75    CLI   SPAUPDT,SPAUPBCN    UPDATE BYR CONFRM DATE? (OM)                 
         BNE   DAA80                                                            
         OI    T.SDTLFLG,SDTLBYCN  SET BUYER CONF CHANGED DATA FLAG             
DAA77    MVC   T.SDTLSCNF,TDYYMD                                                
         MVC   T.SDTLORSN,XDTYMD   UPDATE SENT DATE ALSO                        
         B     DAA85                                                            
*                                                                               
DAA80    MVC   T.SDTLORSN,TDYYMD                                                
DAA85    OI    T.SDTLFLG,SDTLDROR  SET DARE CHANGED DATA FLAG                   
*                                                                               
DAA90    GOTO1 HELLO,DMCB,(=C'P',XSPFIL),(R6),WORK,0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DAA100   TM    SPAFLAG,SPAFNOWR    WRITE=NO?                                    
         BO    DAA110                                                           
         GOTO1 AIO,IOPUT+IOLOCK                                                 
DAA110   B     DAA10                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD AND RETURN OUTPUT TABLE OF DATES FOR STATIONS                 *         
***********************************************************************         
         SPACE 1                                                                
TBBLD    NTR1  ,                                                                
         L     R2,SPAFATBL                                                      
*                                                                               
         XC    KEY,KEY             READ AUTHORIZATION RECORD                    
K        USING AUTRECD,KEY                                                      
         MVI   K.AUTKTYP,AUTKTYQQ      '0D39'                                   
         MVI   K.AUTKSUB,AUTKSUBQ                                               
         MVC   K.AUTKAM,SPAKAM                                                  
         MVC   K.AUTKCLT,SPAKCLT                                                
         MVC   K.AUTKPRD,SPAKPRD                                                
         MVC   K.AUTKPRD2,SPAKPRD2                                              
         MVC   K.AUTKEST,SPAKEST                                                
         MVC   SVKEY(L'AUTKEY),KEY                                              
         B     TBB04                                                            
*                                                                               
TBB02    MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY    GET NEXT VERSION               
         MVC   K.AUTKMKT(5),EFFS                                                
*                                                                               
TBB04    GOTO1 AIO,IOHIGH+IOLOCK                                                
         CLC   K.AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                 
         BNE   TBB10               FINISHED                                     
*                                                                               
TBB06    MVC   SVKEY(L'AUTKEY),KEY                                              
         MVC   IODA,K.AUTKDA                                                    
         GOTO1 AIO,IOGET+IOLOCK                                                 
*                                                                               
         L     R6,SPAIO                                                         
         USING AUTRECD,R6                                                       
         CLI   AUDEL,AUDELQ        MAKE SURE RECORD LOOKS OK                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,AUDFLST),(2,0(R2)) START DATE                     
         GOTO1 (RF),(R1),(3,AUDFLEN),(2,2(R2))   END DATE                       
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             READ STATION RECORD                          
         MVC   K.AUTKEY(AUTKMKT-AUTKEY),SVKEY                                   
         MVC   K.AUTKMKT,SPAKMKT                                                
         MVC   K.AUTKSTA,SPAKSTA                                                
         GOTO1 AIO,IOHIGH                                                       
         CLC   K.AUTKEY,KEYSAVE                                                 
         BNE   TBB08               IF NOT THERE KEEP DATES                      
         XC    0(4,R2),0(R2)                                                    
         B     TBB02                                                            
*                                                                               
TBB08    AHI   R2,4                NEXT SLOT                                    
         B     TBB02                                                            
*                                                                               
TBB10    MVC   0(4,R2),EFFS        USED LITERAL BEFORE                          
         B     EXITOK                                                           
         EJECT                                                                  
********************************************************************            
* INITIALISATION                                                   *            
********************************************************************            
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     RF,SPACOM                                                        
         USING COMFACSD,RF                                                      
         MVC   DMGR,CDATAMGR                                                    
         MVC   DATCON,CDATCON                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   HELLO,CHELLO                                                     
         DROP  RF                                                               
         B     EXITOK                                                           
         EJECT                                                                  
********************************************************************            
* VALIDATE INPUT PARAMETERS                                        *            
********************************************************************            
         SPACE 1                                                                
VALPARM  NTR1  ,                                                                
*                                                                               
         OC    SPAKMKT,SPAKMKT     PASSED A MARKET?                             
         BZ    ERRPARM                                                          
         TM    SPAFLAG,SPAFTBL                                                  
         BZ    VPRM02                                                           
         OC    SPAFATBL,SPAFATBL   PASSED A TABLE?                              
         BZ    ERRPARM                                                          
         OC    SPAKSTA,SPAKSTA     PASSED A STATION?                            
         BZ    ERRPARM                                                          
         MVI   WHATIDO,IDOTBL                                                   
         B     VPARMX                                                           
*                                                                               
VPRM02   OC    SPASDTE,SPASDTE     REST NEED A START DATE                       
         BZ    ERRPARM                                                          
         OC    SPAEDTE,SPAEDTE     & END DATE                                   
         BZ    ERRPARM                                                          
         OC    SPAKSTA,SPAKSTA     PASSED A STATION?                            
         BNZ   VPRM04              YES                                          
*                                                                               
         MVI   WHATIDO,IDOGOAL     GOAL HAS NO STATION                          
         OC    SPAEDTE,SPAEDTE     BUT REQUIRES END DATE                        
         BZ    ERRPARM                                                          
         B     VPRM06                                                           
*                                                                               
VPRM04   MVI   WHATIDO,IDONWS      NWS & DARE HAS A STATION                     
         CLI   SPAUPDT,SPAUPDCN    UPDATE DARE CONFIRMATION DATE                
         BNE   *+8                                                              
         MVI   WHATIDO,IDODARE                                                  
         CLI   SPAUPDT,SPAUPBCN    UPDATE DARE BYR CONFIRM DATE (OM)            
         BNE   *+8                                                              
         MVI   WHATIDO,IDODARE                                                  
         CLI   SPAUPDT,SPAUPDSN    UPDATE DARE SENT DATE                        
         BNE   *+8                                                              
         MVI   WHATIDO,IDODARE                                                  
         CLI   SPAUPDT,SPAUPSDX    UPDATE DARE SENT DATE FROM DX                
         BNE   *+8                                                              
         MVI   WHATIDO,IDODARE                                                  
*                                                                               
VPRM06   GOTO1 DATCON,DMCB,(5,0),(3,TDYYMD)      TODAY                          
*                                                                               
         OC    SPAWRKDT,SPAWRKDT                 WORK REC ADD DATE SENT         
         BZ    VPRM08                                                           
         GOTO1 (RF),(R1),(2,SPAWRKDT),(3,WDTYMD)                                
*                                                                               
VPRM08   OC    SPADATE,SPADATE                   OTHER DATE SENT?               
         BZ    VPRM10                                                           
         GOTO1 (RF),(R1),(2,SPADATE),(3,XDTYMD)                                 
*                                                                               
VPRM10   GOTO1 (RF),(R1),(2,SPASDTE),(3,SDTYMD)  START DATE                     
         GOTO1 (RF),(R1),(2,SPAEDTE),(3,EDTYMD)  END DATE                       
*                                                                               
VPARMX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO I/O TO DMGR                                           *         
***********************************************************************         
         SPACE 1                                                                
AIO      NTR1  ,                                                                
         STC   R1,IOFLAG                                                        
         L     RF,SPAIO            SET IO AREA                                  
         ST    RF,IOADDR                                                        
*                                                                               
         MVI   IOBYTE,0                                                         
         TM    IOFLAG,IOLOCK       READ FOR UPDATE?                             
         BZ    *+8                 NO                                           
         OI    IOBYTE,X'80'                                                     
         NI    IOFLAG,255-IOLOCK                                                
*                                                                               
         IC    R1,IOFLAG                                                        
         LA    RF,CMDTAB           FIND COMMAND                                 
AIO06    CLI   0(RF),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,0(RF)                                                       
         BE    *+12                                                             
         AHI   RF,L'CMDTAB                                                      
         B     AIO06                                                            
*                                                                               
         MVC   IOCMD,4(RF)         SET COMMAND                                  
         MVC   IOFILE,XSPDIR       SET FILE                                     
         CLI   1(RF),IODARQ                                                     
         BE    AIO08                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DMGR,DMCB,(IOBYTE,IOCMD),IOFILE,KEY,KEY,DMWORK                   
         B     AIO10                                                            
*                                                                               
AIO08    MVC   IOFILE,XSPFIL       SET FILE                                     
         GOTO1 DMGR,DMCB,(IOBYTE,IOCMD),IOFILE,IODA,IOADDR,DMWORK               
*                                                                               
AIO10    MVC   IOERROR,8(R1)                                                    
         CLI   IOERROR,0                                                        
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERRPARM  MVI   SPAERR,SPAEPRM                                                   
         B     EXITBACK                                                         
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
EXITBACK L     R1,SAVER1           RETURN PARAMETER LIST                        
         MVC   0(SPAUTHLQ,R1),AUTHBLK                                           
         L     RD,SAVERD                                                        
         CLI   SPAERR,0            SET CC FOR RETURN                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
IOREAD   EQU   X'01'                                                            
IOHIGH   EQU   X'02'                                                            
IORSEQ   EQU   X'03'                                                            
IOGET    EQU   X'04'                                                            
IOPUT    EQU   X'05'                                                            
IOADD    EQU   X'06'                                                            
*                                                                               
IOLOCK   EQU   X'80'                                                            
*                                                                               
IODARQ   EQU   X'01'                                                            
*                                                                               
CMDTAB   DS    0XL12                                                            
         DC    AL1(IOREAD,0,0,0),CL8'DMREAD  '                                  
         DC    AL1(IOHIGH,0,0,0),CL8'DMRDHI  '                                  
         DC    AL1(IORSEQ,0,0,0),CL8'DMRSEQ  '                                  
         DC    AL1(IOGET,IODARQ,0,0),CL8'GETREC  '                              
         DC    AL1(IOPUT,IODARQ,0,0),CL8'PUTREC  '                              
         DC    AL1(IOADD,IODARQ,0,0),CL8'ADDREC  '                              
         DC    X'FF'                                                            
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
XSPDIR   DC    CL8'XSPDIR  '                                                    
XSPFIL   DC    CL8'XSPFIL  '                                                    
EFFS     DC    8X'FF'                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
RELO     DS    A                                                                
SAVER1   DS    A                                                                
SAVERD   DS    A                                                                
AUTHBLK  DS    XL(SPAUTHLQ)                                                     
*                                                                               
DMGR     DS    A                                                                
DATCON   DS    A                                                                
ADDAY    DS    A                                                                
HELLO    DS    A                                                                
*                                                                               
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
SVKEY    DS    XL32                                                             
*                                                                               
WHATIDO  DS    X                                                                
IDODARE  EQU   C'D'                                                             
IDONWS   EQU   C'N'                                                             
IDOGOAL  EQU   C'G'                                                             
IDOTBL   EQU   C'T'                                                             
*                                                                               
IOERROR  DS    X                                                                
IOFLAG   DS    X                                                                
*                                                                               
IOBYTE   DS    X                                                                
IOCMD    DS    CL8                                                              
IOFILE   DS    CL8                                                              
IODA     DS    F                                                                
IOADDR   DS    A                                                                
*                                                                               
FULL     DS    F                                                                
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
*                                                                               
SDTYMD   DS    XL3                                                              
EDTYMD   DS    XL3                                                              
TDYYMD   DS    XL3                                                              
*                                                                               
SVDUDT   DS    XL3                                                              
*                                                                               
WORK     DS    CL80                                                             
WORK2    DS    CL80                                                             
*                                                                               
WDTYMD   DS    XL3                       WORK REC ADD DATE                      
XDTYMD   DS    XL3                       NWS XFER DATE                          
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* SPAUTHD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPAUTHD                                                        
         PRINT ON                                                               
* SPGENAUTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENAUTH                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPAUTH    02/07/03'                                      
         END                                                                    
