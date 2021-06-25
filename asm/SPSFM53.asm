*          DATA SET SPSFM53    AT LEVEL 019 AS OF 03/21/17                      
*PHASE T21753A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21753  -- ESTIMATE LIST                             *         
*                                                                     *         
*  COMMENTS:     LISTS PRODUCT RECORDS                                *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM73 (LIST)                                *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
*                                                                     *         
* 01JUN/06 AKAT - SPOT LEN FILTER TO SUPPORT NEW SPOT LENGTHS         *         
***********************************************************************         
         TITLE 'T21753 - ESTIMATE LIST'                                         
T21753   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1753**,R7,RR=R3                                              
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
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   CHKNEXT                                                          
         GOTO1 =A(VK),RR=RELO                                                   
         CLI   ELSFILH+5,0           IF FILTERS INPUTTED ...                    
         BE    NOFIL                 CALL FILTER VALIDATION ROUTINE             
         LA    R2,ELSFILH                                                       
         BRAS  RE,FILVAL                                                        
NOFIL    DS    0H                    PREPARE TO LIST RECORDS                    
         MVC   ESTKEY,KEY            PREPARE TO LIST RECORDS                    
         B     XIT                                                              
CHKNEXT  CLI   MODE,LISTRECS         LIST RECORDS                               
         BE    LR                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         CLC   ELSPROK,=C'ALL'                                                  
         BE    *+16                                                             
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         B     LR                                                               
         LA    RE,HEDSPEC2                                                      
         ST    RE,SPECS                                                         
         B     LR                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         MVI   OKPFKEY,1                                                        
         NI    ELSFKEYH+1,X'FF'-X'04'                                           
         OI    ELSFKEYH+6,X'80'      IF FILTER DESCRIPTIONS HAVE BEEN           
         CLI   DESDISP,0             PREVIOUSLY DISPLAYED ... SELECT            
         BE    *+8                   FIELDS MUST BE UNPROTECTED AND             
         BAS   RE,UNSEL              DESCRIPTIONS CLEARED                       
*                                                                               
***********************************************************************         
*                                                                               
         MVI   NLISTS,12             LIST 12 RECORDS PER SCREEN                 
         MVI   COUNT,0                                                          
*                                                                               
         LA    R0,ELSSTRH            CLEAR FILTER HEADINGS                      
         LA    R2,ELSENH                                                        
         BAS   RE,CLRFIL                                                        
*                                                                               
         LA    R0,ELSSTRUH                                                      
         LA    R2,ELSENUH                                                       
         BAS   RE,CLRFIL                                                        
*                                                                               
LR03     MVC   ELSEST(3),=C'Est'                                                
         OI    ELSESTH+6,X'80'                                                  
         CLI   DATA,C'E'             DISPLAY DEMOS?                             
         BNE   LR04                                                             
         MVC   ELSSTR(L'ELSSTR),SPACES                                          
         MVC   ELSSTRU(L'ELSSTR),SPACES                                         
         MVC   ELSSTR+6(6),=CL6'Demo 1'                                         
         MVC   ELSSTRU+6(6),=CL6'------'                                        
         MVC   ELSSTR+16(6),=CL6'Demo 2'                                        
         MVC   ELSSTRU+16(6),=CL6'------'                                       
         MVC   ELSSTR+26(6),=CL6'Demo 3'                                        
         MVC   ELSSTRU+26(6),=CL6'------'                                       
         MVC   ELSSTR+36(6),=CL6'Demo 4'                                        
         MVC   ELSSTRU+36(6),=CL6'------'                                       
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR06C                   NO                                       
         LA    R1,ELSSTR+43            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+43           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
         B     LR06C                                                            
*                                                                               
LR04     MVC   ELSSTR(L'ELSSTR),SPACES DEFAULT TO DISPLAY OF DATES              
         MVC   ELSSTRU(L'ELSSTR),SPACES               AND DESCRIPTION           
         MVC   ELSSTR+6(5),=CL5'Start'                                          
         MVC   ELSSTRU+5(8),=CL8'--------'                                      
         MVC   ELSSTR+20(3),=CL3'End'                                           
         MVC   ELSSTRU+18(8),=CL8'--------'                                     
         MVC   ELSSTR+30(11),=CL11'Description'                                 
         MVC   ELSSTRU+30(20),=CL20'--------------------'                       
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR04B                   NO                                       
         LA    R1,ELSSTR+51            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+51           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
*                                                                               
LR04B    CLI   DATA,C'M'             DISPLAY MENU?                              
         BNE   LR05                                                             
         MVC   ELSSTR+30(25),SPACES                                             
         MVC   ELSSTRU+31(25),SPACES                                            
         MVC   ELSSTR+31(8),=CL8'Dpt Menu'                                      
         MVC   ELSSTRU+31(8),=CL8'--------'                                     
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR06C                                                            
         LA    R1,ELSSTR+40            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+40           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
         B     LR06C                                                            
*                                                                               
LR05     CLI   DATA,C'C'                                                        
         BNE   LR06                                                             
         MVC   ELSSTR+30(25),SPACES                                             
         MVC   ELSSTRU+30(25),SPACES                                            
         MVC   ELSSTR+31(9),=CL9'Copy Code'                                     
         MVC   ELSSTRU+31(9),=CL9'---------'                                    
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR06C                   NO                                       
         LA    R1,ELSSTR+41            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+41           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
         B     LR06C                                                            
*                                                                               
LR06     CLI   DATA,C'Z'                                                        
         BNE   LR06A                                                            
         MVC   ELSSTR+30(25),SPACES                                             
         MVC   ELSSTRU+30(25),SPACES                                            
         MVC   ELSSTR+31(8),=CL8'Position'                                      
         MVC   ELSSTRU+31(8),=CL8'--------'                                     
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR06C                   NO                                       
         LA    R1,ELSSTR+41            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+41           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
         B     LR06C                                                            
*                                                                               
LR06A    CLI   DISPLAY,C'H'                                                     
         BE    *+12                                                             
         CLI   DISPLAY,C'L'                                                     
         BNE   LR06C                                                            
         MVC   ELSSTR+30(25),SPACES                                             
         MVC   ELSSTRU+30(25),SPACES                                            
         MVC   ELSSTR+31(11),=CL11'Lock Period'                                 
         MVC   ELSSTRU+31(11),=CL11'-----------'                                
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BNE   LR06C                   NO                                       
         LA    R1,ELSSTR+43            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+43           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
*                                                                               
LR06C    CLI   DATA,C'P'                                                        
         BNE   LR07                                                             
         MVC   ELSSTR(L'ELSSTR),SPACES                                          
         MVC   ELSSTRU(L'ELSSTRU),SPACES                                        
         MVC   ELSEST(3),=CL3'Prd'                                              
         MVC   ELSSTR(12),=CL12'Product Name'                                   
         MVC   ELSSTRU(20),=CL20'--------------------'                          
         MVC   ELSSTR+23(3),=CL3'Est'                                           
         MVC   ELSSTRU+23(3),=CL3'---'                                          
         MVC   ELSSTR+29(20),=CL20'Estimate Description'                        
         MVC   ELSSTRU+29(20),=CL20'--------------------'                       
         CLI   TYPEDIS,C'D'            DISPLAY TYPE?                            
         BE    *+12                    YES                                      
         CLI   DATA,C'A'               DATE (SAME FORMAT AS REGULAR)            
         BNE   LR07                                                             
         LA    R1,ELSSTR+50            A(WHERE TO DISPLAY TYPE)                 
         LA    R2,ELSSTRU+50           A(WHERE TO DISPLAY ----)                 
         BAS   RE,DSPTYPE              SAVE A()'S AND DISPLAY                   
*                                                                               
LR07     OI    ELSHEADH+6,X'80'                                                 
         OI    ELSSTRH+6,X'80'                                                  
         OI    ELSSTRUH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         USING ESTHDR,R4                                                        
         LA    R4,KEY                                                           
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,ESTKEY            READHIGH FIRST ESTIMATE IN LIST            
         MVC   P,SPACES                                                         
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY                READ SEQUENTIAL ALL RECORDS AFTER          
         MVC   P,SPACES              FIRST ESTIMATE                             
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR30     CLI   DATA,C'P'             IF ESTIMATE IS 'ALL' - LIST SPEC-          
         BNE   LR30A                 IFIED ESTIMATE RECORDS UNTIL               
         CLC   KEY(4),ESTKEY         CHANGE OF CLIENT                           
         BNE   LRX                                                              
         CLI   ALLEST,C'A'                                                      
         BE    LR30B                                                            
         CLC   KEY+7(1),ALLEST                                                  
         BNE   LR20                                                             
         B     LR30B                                                            
*                                                                               
LR30A    CLC   KEY(7),ESTKEY         OTHERWISE ... LIST ALL ESTIMATE            
         BNE   LRX                   RECORDS UNTIL CHANGE OF PRODUCT            
*                                                                               
LR30B    OC    KEY+7(1),KEY+7        FILTER OUT CLIENT AND PRODUCT              
         BZ    LR20                  RECORDS                                    
         OC    KEY+8(5),KEY+8        FILTER OUT OTHER RECORDS                   
         BNZ   LR20                                                             
         OC    CNTL,CNTL                                                        
         BZ    LR30C                                                            
         CLC   CNTL,EKEYPRD                                                     
         BE    LR20                                                             
         MVC   CNTL,EKEYPRD                                                     
*                                                                               
LR30C    CLI   BEGRAN,C'N'           IF FILTERING ON A RANGE ...                
         BE    LR31                  DISPLAY ONLY ESTIMATE RECORDS              
         CLC   KEY+7(1),BEGRAN       IN THAT RANGE                              
         BL    LR20                                                             
         CLI   ENDRAN,C'N'                                                      
         BE    LR31                                                             
         CLC   KEY+7(1),ENDRAN                                                  
         BH    LRX                                                              
*                                                                               
***********************************************************************         
*                                                                               
LR31     GOTO1 GETREC                GET ESTIMATE RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVC   LISTAR,SPACES                                                    
*                                                                               
***********************************************************************         
*                                                                               
         CLI   REQ,C'Y'              IF FILTERING ON A REQUEST RNG ...          
         BNE   LR32B                                                            
         TM    EFLAG1,EF1REQ         IF 'Y' DISPLAY ONLY ESTIMATE REC-          
         BZ    LR20                  ORDS WITH REQUEST RANGE                    
         B     LR33A                                                            
LR32B    CLI   REQ,C'N'              IF 'N' DISPLAY ONLY ESTIMATE REC-          
         BNE   LR33A                 ORDS WITHOUT REQUEST RANGE                 
         TM    EFLAG1,EF1REQ                                                    
         BO    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33A    CLI   NMG,C'Y'             IF FILTERING ON MAKEGOOD DATES ...          
         BNE   LR33B                                                            
         TM    EFLAG1,EF1NMG        IF 'Y' DISPLAY ONLY ESTIMATE REC-           
         BZ    LR20                 ORDS WITH REQUEST RANGE                     
         B     LR33C                                                            
LR33B    CLI   NMG,C'N'             IF 'N' DISPLAY ONLY ESTIMATE REC-           
         BNE   LR33C                ORDS WITHOUT REQUEST RANGE                  
         TM    EFLAG1,EF1NMG                                                    
         BO    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33C    CLI   BOOKFL,C'Y'          IF FILTERING ON RATING BOOK ...             
         BNE   LR33D                DISPLAY ONLY ESTIMATES WITH MATCH-          
         CLC   EBOOK,BOOK           ING RATING BOOK                             
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33D    CLI   DAILY,C'Y'           IF FILTERING ON DAILY ESTIMATES ...         
         BNE   LR33E                                                            
         CLI   EDAILY,C'Y'          IF 'Y' DISPLAY ONLY DAILY ESTIMATES         
         BNE   LR20                                                             
         B     LR33F                                                            
LR33E    CLI   DAILY,C'N'           IF 'N' DISPLAY ONLY NON-DAILY EST-          
         BNE   LR33F                IMATES                                      
         CLI   SVCLDLY,C'Y'                                                     
         BNE   LR33EE                                                           
         CLI   EDAILY,C' '                                                      
         BE    LR33F                                                            
         CLI   EDAILY,0                                                         
         BE    LR33F                                                            
         B     LR20                                                             
LR33EE   CLI   EDAILY,C'N'                                                      
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33F    CLI   MENUFL,C'Y'          IF FILTERING ON DEPT MENU ...               
         BNE   LR33G                DISPLAY ONLY ESTIMATES WITH MATCH-          
         CLC   EDAYMENU,MENU        ING DAYPART MENU                            
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33G    CLI   MASTER,C'Y'          IF FILTERING ON MASTER EST'S ...            
         BNE   LR33H                                                            
         CLI   EMSTRIND,C'M'        IF 'Y' DISPLAY ONLY MASTER EST'S            
         BNE   LR20                                                             
         B     LR33I                                                            
LR33H    CLI   MASTER,C'N'          IF 'N' DISPLAY ONLY NON-MASTER              
         BNE   LR33I                ESTIMATES                                   
         CLI   EMSTRIND,C'M'                                                    
         BE    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33I    CLI   SUBEST,C'Y'          IF FILTERING ON SUB ESTIMATES ...           
         BNE   LR33J                                                            
         CLI   EMSTRIND,C'S'        IF 'Y' DISPLAY ONLY SUB ESTIMATES           
         BNE   LR20                                                             
         B     LR33K                                                            
LR33J    CLI   SUBEST,C'N'          IF 'N' DISPLAY ONLY NON-SUB EST'S           
         BNE   LR33K                                                            
         CLI   EMSTRIND,C'S'                                                    
         BE    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33K    CLI   OOWRFL,0             IF FILTERING ON OUT OF WEEK ROT-            
         BE    LR33M                ATOR ...                                    
         CLI   OOWR,0               IF 'N' DISPLAY ONLY ESTIMATES WITH          
         BNE   LR33L                NO OUT OF WEEK ROTATOR                      
         CLI   EOWSDAY,0                                                        
         BNE   LR20                 IF 'Y' DISPLAY ESTIMATES WITH OUT           
         B     LR33M                OF WEEK ROTATORS (DISPLAY DAY OF            
LR33L    CLI   EOWSDAY,0            START DATE TOO)                             
         BE    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33M    CLI   WIM,C'Y'             IF FILTERING ON WIM TRADE ...               
         BNE   LR33N                                                            
         TM    EFLAG1,EF1WITRD      IF 'Y' DISPLAY ONLY ESTIMATE REC-           
         BZ    LR20                 ORDS WITH WIM TRADE                         
         B     LR33O                                                            
LR33N    CLI   WIM,C'N'             IF 'N' DISPLAY ONLY ESTIMATE REC-           
         BNE   LR33O                ORDS WITHOUT WIM TRADE                      
         TM    EFLAG1,EF1WITRD                                                  
         BO    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33O    CLI   SLN,0                IF FILTERING ON SPOT LENGTH ...             
         BE    LR33P                ESTIMATE'S SPOT LENGTH MUST MATCH           
         CLC   ESLN,SLN             FILTER                                      
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33P    CLI   CPPRS,C'Y'           IF FILTERING ON CPPRS...                    
         BNE   LR33Q                                                            
         CLI   ECPPRS,C'Y'          IF 'Y' DISPLAY ONLY CPPRS                   
         BE    LR33R                                                            
         CLI   ECPPRS,X'00'         NULLS IS 'Y'                                
         BNE   LR20                                                             
         B     LR33R                                                            
LR33Q    CLI   CPPRS,C'N'           IF 'N' DISPLAY NON-CPPRS                    
         BNE   LR33R                                                            
         CLI   ECPPRS,C'N'                                                      
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR33R    CLI   ETYPE,0              IS ETYPE REGULAR?                           
         BNE   *+8                  NO                                          
         MVI   ETYPE,C'R'           NULL=R FOR OLD RECS                         
         CLI   TYPE,0               FILTERING ON TYPE?                          
         BE    LR34                 NO                                          
         CLC   TYPE,ETYPE                                                       
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR34     CLI   TYPEDIS,C'D'         DISPLAY TYPE?                               
         BNE   LR35                 NO                                          
         L     R1,ATYPE             A(TYPE HEADING)                             
         LA    R2,ELSSTR            A(START OF LIST HEADINGS)                   
         SR    R1,R2                DISPLACEMENT TO TYPE HEADING                
         LA    R2,LISTAR            BEGINNING OF OUR LIST LINE                  
         LA    R2,6(R2,R1)          POINT TO WHERE TO DISPLAY TYPE              
         ST    R2,ATYPE2            FOR LATER                                   
         MVC   0(1,R2),ETYPE                                                    
*                                                                               
***********************************************************************         
*                                                                               
LR35     CLI   MODE,PRINTREP                                                    
         BE    LR35BB                                                           
         CLI   DISPLAY,C'L'          IF DISPLAYING ONLY LOCKED EST'S            
         BNE   LR35A                 FILTER OUT ALL OTHERS                      
         TM    ECNTRL,X'0C'          FILTER OUT HOLD ESTIMATES                  
         BO    LR20                                                             
         TM    ECNTRL,X'08'                                                     
         BO    LR35C                                                            
         B     LR35B                                                            
LR35A    CLI   DISPLAY,C'H'                                                     
         BNE   LR36                                                             
         TM    ECNTRL,X'0C'                                                     
         BO    LR35C                                                            
LR35B    OC    ELOCKYM,ELOCKYM                                                  
         BZ    LR20                                                             
LR35BB   OC    ELOCKYM,ELOCKYM                                                  
         BZ    LR35C                                                            
         MVC   LSLOCK,SPACES                                                    
         MVC   WORK(L'ELOCKYM),ELOCKYM                                          
         NI    WORK+1,X'FF'-X'80'-X'40'                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,LSLOCK)                                  
         TM    ELOCKMON,X'80'                                                   
         BZ    *+8                                                              
         MVI   LSLOCK+6,C'-'                                                    
         TM    ELOCKMON,X'40'                                                   
         BZ    *+8                                                              
         MVI   LSLOCK+6,C'+'                                                    
         B     LR35D                                                            
LR35C    MVC   LSLOCK,SPACES                                                    
LR35D    MVC   PLOK,LSLOCK                                                      
*                                                                               
***********************************************************************         
*                                                                               
LR36     OC    DATE1,DATE1           IF FILTERING ON DATES, ALL DATES           
         BZ    LR37                  MUST FIT BETWEEN DATE1 AND DATE2           
         CLC   EEND,DATE1                                                       
         BL    LR20                                                             
         CLC   ESTART,DATE2                                                     
         BH    LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR37     OC     DEMOFIL,DEMOFIL      IF FILTERING DEMOS                         
         BZ     LR40                 IF COUNTER HITS 20, NO DEMOS MATCH         
         LA     RE,1                 SO GET NEXT RECORD                         
         LA     R6,EDEMLST                                                      
LR38     CH     RE,=H'20'                                                       
         BNL    LR20                                                            
         CLC    0(3,R6),DEMOFIL      IS DEMOFIL IN DEMOLIST?                    
         BE     LR39                                                            
         LA     R6,3(R6)             NO,BUMP TO NEXT DEMO IN DEMOLIST           
         LA     RE,1(RE)                                                        
         B      LR38                                                            
LR39     EDIT   (RE),LSPOS+4,ALIGN=LEFT                                         
*                                                                               
***********************************************************************         
*                                                                               
LR40     DS    0H                                                               
         CLI   RTYPE,X'00'                                                      
         BE    LR45                                                             
         CLC   RTYPE,ERATE                                                      
         BNE   LR20                                                             
*                                                                               
LR45     DS    0H                                                               
         CLI   DATA,C'P'                                                        
         BE    LR79                                                             
         EDIT  EKEYEST,LSESTC        COPY ESTIMATE CODE TO LIST LINE            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    *+12                                                             
         CLI   DATA,C'E'             CHECK DATA OPTION TO SEE IF DEMOS          
         BNE   LR70                  SHOULD BE SENT TO LIST LINE                
         OC    EDEMLST(3),EDEMLST                                               
         BZ    LR80                                                             
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(224),BLOCK+255                                         
         XC    ELEM,ELEM             DEMOS EXIST ...                            
         LA    R5,ELEM                                                          
         USING DBLOCK,R5             SET UP CALL TO DEMOCON                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   LR50                  AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    LR50                                                             
         MVI   DBSELMED,C'C'                                                    
LR50     MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB           CALL DEMOCON                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(14,EDEMLST),(13,BLOCK),(C'S',ELEM),EUSRNMS,  X        
               ENONTDMS                                                         
         DROP  R5                                                               
*                                                                               
         LA    R5,LSDEMOS            4 DEMOS IN LIST LINE                       
         LA    R2,EDEMLST                                                       
         LA    R3,BLOCK                                                         
*                                                                               
         LA    R6,4                                                             
LR60     CLI   0(R3),C' '                                                       
         BNH   LR65                  IF STILL MORE DEMOS ...                    
         GOTO1 =A(FMTDEMO),RR=RELO   CALL FMTDEMO                               
*                                                                               
         ZIC   R1,WORK               LENGTH OF DEMO RETURNED IN WORK            
         BCTR  R1,0                  DEMO RETURNED IN WORK+1 ... COPY           
         EX    R1,*+8                IT TO SCREEN                               
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
*                                                                               
         LA    R5,10(R5)             ADVANCE TO NEXT DEMO IN LIST LINE          
         LA    R3,11(R3)             NEXT DEMO IN BLOCK AND RECORD              
         LA    R2,3(R2)              BUMP TO NEXT DEMO IN RECORD                
         BCT   R6,LR60                                                          
LR65     MVC   PD1,LSDEM1                                                       
         MVC   PD2,LSDEM2                                                       
         MVC   PD3,LSDEM3                                                       
         MVC   PD4,LSDEM4                                                       
         B     LR80                                                             
*                                                                               
LR70     GOTO1 DATCON,DMCB,(0,ESTART),(5,LSSTD)                                 
         GOTO1 DATCON,DMCB,(0,EEND),(5,LSEDT)                                   
*                                                                               
LR71     CLI   DATA,C'Z'                                                        
         BE    LR80                                                             
         CLI   DISPLAY,C'H'                                                     
         BE    LR80                                                             
         CLI   DISPLAY,C'L'                                                     
         BE    LR80                                                             
         MVC   LSESTD,EDESC          CHECK DATA OPTION TO SEE IF START          
LR77     CLI   DATA,C'M'             DATE, END DATE, DESCRIPTION AND            
         BNE   LR78                  MENU SHOULD BE SENT TO LIST LINE           
         MVC   LSESTD,SPACES                                                    
         MVC   LSMENU(1),EDAYMENU                                               
         CLI   TYPEDIS,C'D'          DID WE WANT TO DISPLAY TYPE?               
         BNE   LR78                  NO                                         
         L     R2,ATYPE2             A(TYPE INSERTION) GOT SPACED OUT           
         MVC   0(1,R2),ETYPE                                                    
LR78     CLI   DATA,C'C'                                                        
         BNE   LR78B                                                            
         MVC   LSESTD,SPACES                                                    
         MVC   LSCOPY,ECOPY                                                     
         CLI   TYPEDIS,C'D'          DID WE WANT TO DISPLAY TYPE?               
         BNE   LR78B                 NO                                         
         L     R2,ATYPE2             A(TYPE INSERTION) GOT SPACED OUT           
         MVC   0(1,R2),ETYPE                                                    
LR78B    CLI   DATA,C'O'                                                        
         BNE   LR80                                                             
         MVC   LSESTD,SPACES                                                    
         CLI   TYPEDIS,C'D'          DID WE WANT TO DISPLAY TYPE?               
         BNE   LR80                  NO                                         
         L     R2,ATYPE2             A(TYPE INSERTION) GOT SPACED OUT           
         MVC   0(1,R2),ETYPE                                                    
         MVC   LSOOWR(2),=C'NO'                                                 
         CLI   EOWSDAY,0                                                        
         BE    LR80                                                             
         MVC   LSOOWR(3),=C'YES'                                                
         B     LR80                                                             
*                                                                               
LR79     MVC   LPPRDC,EKEYPRD                                                   
         EDIT  EKEYEST,LPESTC                                                   
         MVC   LPESTN,EDESC                                                     
         MVC   TEMPKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),TEMPKEY                                                   
         GOTO1 READ                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         USING PRDHDR,RE                                                        
         L     RE,AIO                                                           
         MVC   LPPRDN,PNAME                                                     
         DROP  RE                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,TEMPKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
LR80     CLI   MODE,PRINTREP                                                    
         BNE   LR200                                                            
         EDIT  EKEYEST,PEST                                                     
         GOTO1 DATCON,DMCB,(0,ESTART),(5,PSTART)                                
         GOTO1 DATCON,DMCB,(0,EEND),(5,PEND)                                    
         MVC   PDES,EDESC                                                       
         CLI   DATA,C'P'                                                        
         BE    *+16                                                             
         MVC   PCOP,ECOPY                                                       
         MVC   PMEN,EDAYMENU                                                    
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR210                                                            
LR200    GOTO1 LISTMON                                                          
*                                                                               
LR210    CLI   BEGRAN,C'N'                                                      
         BE    LR20                                                             
         CLI   ENDRAN,C'N'                                                      
         BNE   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
*                                                                               
PFTABLE  DS   0H                                                                
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSPROK-1),AL2(ELSPROK-T217FFD)                    
MPF02X   EQU  *                                                                 
*                                                                               
*        ESTIMATE COPY                                                          
         DC   AL1(LPF08X-*,08,PFTCPROG,(LPF08X-LPF08)/KEYLNQ,0)                 
         DC   CL3'EC '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'COPY'                ACTION                                   
LPF08    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSPROK-1),AL2(ELSPROK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LSESTC-1),AL2(LSESTC-LSESTC)                       
LPF08X   EQU  *                                                                 
*                                                                               
*        ESTIMATE DOLLARS                                                       
         DC   AL1(MPF09X-*,09,PFTCPROG,(MPF09X-MPF09)/KEYLNQ,0)                 
         DC   CL3'ED '                 MAINT                                    
         DC   CL8'ESTD'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF09    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSPROK-1),AL2(ELSPROK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LSESTC-1),AL2(LSESTC-LSESTC)                       
MPF09X   EQU  *                                                                 
*                                                                               
*        ESTIMATE MAINT DISPLAY                                                 
         DC   AL1(MPF07X-*,07,PFTCPROG,(MPF07X-MPF07)/KEYLNQ,0)                 
         DC   CL3'EM '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF07    DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSPROK-1),AL2(ELSPROK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LSESTC-1),AL2(LSESTC-LSESTC)                       
MPF07X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*        PFTABLE2                                                               
*        ESTIMATE MAINT DISPLAY                                                 
PFTABLE2 DC   AL1(M2PF07X-*,07,PFTCPROG,(M2PF07X-M2PF07)/KEYLNQ,0)              
         DC   CL3'EM '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
M2PF07   DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LPPRDC-1),AL2(LPPRDC-LPPRDC)                       
         DC   AL1(KEYTYCUR,L'LPESTC-1),AL2(LPESTC-LPPRDC)                       
M2PF07X  EQU  *                                                                 
*                                                                               
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(M2PF02X-*,02,PFTCPROG,(M2PF02X-M2PF02)/KEYLNQ,0)              
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
M2PF02   DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LPPRDC-1),AL2(LPPRDC-LPPRDC)                       
M2PF02X  EQU  *                                                                 
*                                                                               
*        ESTIMATE COPY DISPLAY                                                  
         DC   AL1(M2PF08X-*,08,PFTCPROG,(M2PF08X-M2PF08)/KEYLNQ,0)              
         DC   CL3'EC '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'COPY'                ACTION                                   
M2PF08   DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LPPRDC-1),AL2(LPPRDC-LPPRDC)                       
         DC   AL1(KEYTYCUR,L'LPESTC-1),AL2(LPESTC-LPPRDC)                       
M2PF08X  EQU  *                                                                 
*                                                                               
*        ESTIMATE DOLLAR DISPLAY                                                
         DC   AL1(M2PF09X-*,09,PFTCPROG,(M2PF09X-M2PF09)/KEYLNQ,0)              
         DC   CL3'ED '                 MAINT                                    
         DC   CL8'ESTD'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
M2PF09   DC   AL1(KEYTYTWA,L'ELSMEDK-1),AL2(ELSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ELSCLIK-1),AL2(ELSCLIK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LPPRDC-1),AL2(LPPRDC-LPPRDC)                       
         DC   AL1(KEYTYCUR,L'LPESTC-1),AL2(LPESTC-LPPRDC)                       
M2PF09X  EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X2-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X2  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
***********************************************************************         
*                          MY ERROR MESSAGES                          *         
***********************************************************************         
*                          INPUT:  R1 = A(INV FILTER ENTRY IN BSCAND)           
*                          INPUT:  R2 = A(FILTER SCREEN FIELD HEADER)           
*                          INPUT:  R3 = A(FILTER NAME MATCHED IN FVT)           
*                          OUTPUT: CONSTRUCT THE ERROR MESSAGE AND EXIT         
         USING BSCAND,R1                                                        
ERRINVFN XC    CONHEAD,CONHEAD                                                  
         LA    R5,CONHEAD                                                       
         USING INVFNAMD,R5                                                      
*                                                                               
         MVI   INVFN#,C'#'                                                      
         MVC   INVFNMSG,=C'Invalid filter name :  '                             
         MVC   INVFNIN,BFLD1       INVALID FILTER NAME                          
*                                                                               
         LR    RF,R1                                                            
         LA    RE,BLOCK                                                         
         SR    RF,RE                                                            
         SR    RE,RE               PREPARE FOR DIVISION                         
         LA    R0,32                                                            
         DR    RE,R0                                                            
         LA    RF,1(RF)            RF = FILTER # ON THE LINE                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  INVFNFN,DUB                                                      
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
*---------------------------------------------------------------------*         
ERRINVFV XC    CONHEAD,CONHEAD                                                  
         LA    R5,CONHEAD                                                       
         USING INVFVALD,R5                                                      
*                                                                               
         MVI   INVFV#,C'#'                                                      
         MVC   INVFVMS1,=C'Invalid filter value for '                           
         MVC   INVFVMS2,=C' :  '                                                
         CLI   BFLD2,C' '                                                       
         BNE   *+10                                                             
         MVC   INVFVMS1(7),=C'Missing'                                          
*                                                                               
         MVC   INVFVIN,BFLD2       INV FILTER VALUE                             
         MVC   INVFVFNA,0(R3)      FILTER NAME                                  
*                                                                               
         LR    RF,R1                                                            
         LA    RE,BLOCK                                                         
         SR    RF,RE                                                            
         SR    RE,RE               PREPARE FOR DIVISION                         
         LA    R0,32                                                            
         DR    RE,R0                                                            
         LA    RF,1(RF)            RF = FILTER # ON THE LINE                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  INVFVFN,DUB                                                      
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT3,OKVALSEL                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
*                                                                               
         OI    ELSREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    ELSREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    ELSREH+6,X'80'                                                   
*                                                                               
         CLI   16(RA),0              FIRST TIME RUN                             
         BNE   *+16                                                             
         MVI   16(RA),1                                                         
         MVI   DESPAGE,0                                                        
         MVI   OKPFKEY,1                                                        
*                                                                               
         CLI   OKPFKEY,0                                                        
         BNE   SETUP10                                                          
*                                                                               
         OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
         J     ERRPFKY                                                          
*                                                                               
SETUP10  CLI   PFKEY,4                                                          
         BE    SETUP20                                                          
         CLI   PFKEY,5                                                          
         BE    SETUP20                                                          
         CLC   ELSPROK,=C'ALL'       IF LISTING ALL PRODUCTS IN 1 EST           
         BNE   SETUP20                                                          
         GOTO1 INITPFKY,DMCB,PFTABLE2                                           
         B     SETUPX                                                           
SETUP20  GOTO1 INITPFKY,DMCB,PFTABLE                                            
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        HEDSPECS                                                     *         
***********************************************************************         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+7(L'QMED),QMED                                                
         MVC   H4+11(L'MEDNM),MEDNM                                             
         MVC   H4+32(L'QCLT),QCLT                                               
         MVC   H4+38(L'CLTNM),CLTNM                                             
         CLC   ELSPROK,=C'ALL'                                                  
         BE    HEDX                                                             
         LA    R6,KEY                                                           
         USING ESTHDR,R6                                                        
         MVC   H4+77(L'EKEYPRD),EKEYPRD                                         
         MVC   H4+83(L'PRDNM),PRDNM                                             
         DROP  R6                                                               
HEDX     XIT1                                                                   
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H4,1,C'Media:'                                                   
         SSPEC H4,25,C'Client:'                                                 
         SSPEC H4,68,C'Product:'                                                
*                                                                               
         SSPEC H6,1,C'Est'                                                      
         SSPEC H7,1,C'---'                                                      
         SSPEC H6,8,C'Start'                                                    
         SSPEC H7,7,C'--------'                                                 
         SSPEC H6,18,C'End'                                                     
         SSPEC H7,16,C'--------'                                                
         SSPEC H6,25,C'Description'                                             
         SSPEC H7,25,C'--------------------'                                    
         SSPEC H6,46,C'Lock Period'                                             
         SSPEC H7,46,C'-----------'                                             
         SSPEC H6,58,C'Demo 1'                                                  
         SSPEC H7,58,C'------'                                                  
         SSPEC H6,68,C'Demo 2'                                                  
         SSPEC H7,68,C'------'                                                  
         SSPEC H6,78,C'Demo 3'                                                  
         SSPEC H7,78,C'------'                                                  
         SSPEC H6,88,C'Demo 4'                                                  
         SSPEC H7,88,C'------'                                                  
         SSPEC H6,98,C'Dpt Menu'                                                
         SSPEC H7,98,C'--------'                                                
         SSPEC H6,108,C'Copy Code'                                              
         SSPEC H7,108,C'---------'                                              
         DC    X'00'                                                            
*                                                                               
HEDSPEC2 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H4,1,C'Media:'                                                   
         SSPEC H4,25,C'Client:'                                                 
*                                                                               
         SSPEC H6,1,C'Est'                                                      
         SSPEC H7,1,C'---'                                                      
         SSPEC H6,9,C'Start'                                                    
         SSPEC H7,8,C'--------'                                                 
         SSPEC H6,19,C'End'                                                     
         SSPEC H7,17,C'--------'                                                
         SSPEC H6,26,C'Description'                                             
         SSPEC H7,26,C'--------------------'                                    
         DC    X'00'                                                            
***********************************************************************         
*        FILVAL                                                       *         
***********************************************************************         
FILVAL   NTR1                                                                   
         L     RF,ACOMFACS           PARSNIP OUTPUT AT AIO3                     
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         L     R6,AIO3                                                          
         GOTO1 (RF),DMCB,(R2),(15,(R6)),0                                       
         ZICM  R0,DMCB+4,1                                                      
         BNZ   *+8                                                              
         J     ERRINVFS              INVALID FILTER INPUT SYNTAX                
*                                                                               
         GOTO1 =A(PARTOSCN),RR=RELO  CHANGE PARSNIP BLK TO BE SCANNER           
*                                    LIKE BLOCK                                 
*                                                                               
*        CLI   8(R2),C'?'                                                       
*        BNE   *+12                                                             
*        GOTO1 =A(DISFTAB),RR=RELO   DISPLAY FILTER SYNTAX AND EXIT             
*                                                                               
         CLI   8(R2),C','                                                       
         JE    ERRINVFS              INVALID FILTER INPUT SYNTAX                
         CLI   8(R2),C'='                                                       
         JE    ERRINVFS                                                         
         CLI   8(R2),C' '                                                       
         JE    ERRINVFS                                                         
*                                                                               
         LA    R1,BLOCK              INPUTTED FILTERS                           
         USING BSCAND,R1             BIG SCAN BLOCK                             
         LA    R3,FILVTAB            FILTER VALIDATION TABLE                    
         USING FILVTABD,R3                                                      
         LA    R5,FILTER             RECORD FILTER                              
         USING FILTERD,R5                                                       
         ST    R5,NEXTAVRF           RESET NEXT AVAILABLE REC FILTER            
         LA    RE,FILSPACE           RESET FILSPACE                             
         ST    RE,NEXTAVSP                                                      
         STC   R0,FCNTR              RESET FILTER COUNTER                       
         MVI   SDFLAG,1              ALLOW CHANGE OF DISPLAY FORMAT             
*                                                                               
***********************************************************************         
*                                                                               
FV10     CLI   BFLD1LEN,4            FILTER NAME MUST BE 3,4,5,6 OR 7           
         BE    FV14                  CHARACTERS LONG                            
         CLI   BFLD1LEN,7                                                       
         BE    FV13                                                             
         CLI   BFLD1LEN,5                                                       
         BE    FV17                                                             
         CLI   BFLD1LEN,6                                                       
         BE    FV18                                                             
         CLI   BFLD1LEN,3                                                       
         BNE   ERRINVFN                                                         
*                                                                               
FV12     CLC   FILVTNA3,BFLD1        IF FILTER NAME IS 3 CHARACTERS             
         BE    FV20                  LONG IT MUST BE 'EST' 'REQ'                
         XC    HALF,HALF             'NMG' OR 'SLN'                             
         ZIC   RF,FILVTNVF                                                      
         MVC   HALF+1,FILVTFVL                                                  
         MH    RF,HALF                                                          
         LA    R3,FILVTOVQ(RF,R3)                                               
         CLI   0(R3),X'00'                                                      
         BNE   FV12                                                             
         B     ERRINVFN                                                         
*                                                                               
FV13     CLC   FILVTNA7,BFLD1        IF FILTER NAME IS 7 CHARACTERS             
         BE    FV20                  LONG IT MUST BE 'DISPLAY' OR               
         XC    HALF,HALF             'DPTMENU'                                  
         ZIC   RF,FILVTNVF                                                      
         MVC   HALF+1,FILVTFVL                                                  
         MH    RF,HALF                                                          
         LA    R3,FILVTOVQ(RF,R3)                                               
         CLI   0(R3),X'00'                                                      
         BNE   FV13                                                             
         B     ERRINVFN                                                         
*                                                                               
FV14     DS    0H                                                               
         CLC   BFLD1(4),=C'DATE'     DATE HAS TOO MANY COMBINATIONS TO          
         BE    FV15                  CHECK FOR...THIS IS MORE EFFICIENT         
         CLI   BFLD2LEN,3                                                       
         BL    FV14A                                                            
         CLI   BFLD2LEN,7                                                       
         BE    FV15                                                             
         CLI   BFLD2LEN,4                                                       
         BNH   FV15                                                             
FV14A    DS    0H                                                               
         LA    R3,BFLD1                                                         
         B     ERRINVFV                                                         
*                                                                               
FV15     DS    0H                                                               
         CLC   FILVTNA4,BFLD1        IF FILTER NAME IS 4 CHARACTERS             
         BNE   FV16                  LONG IT MUST BE 'DATE' 'DEMO'              
         CLI   FILVTFVL,0            'DATA' 'BOOK' 'TYPE' OR 'OOWR'             
         BE    FV20                                                             
         CLC   FILVTFVL,BFLD2LEN                                                
         BE    FV20                                                             
FV16     XC    HALF,HALF                                                        
         ZIC   RF,FILVTNVF                                                      
         MVC   HALF+1,FILVTFVL                                                  
         MH    RF,HALF                                                          
         LA    R3,FILVTOVQ(RF,R3)                                               
         CLI   0(R3),X'00'                                                      
         BNE   FV15                                                             
         B     ERRINVFN                                                         
*                                                                               
FV17     CLC   FILVTNA5,BFLD1        IF FILTER NAME IS 5 CHARACTERS             
         BE    FV20                                                             
*                                                                               
*        BNE   FV17A                 LONG IT MUST BE 'DAILY' 'WMTER'            
*        CLI   FILVTFVL,0                                                       
*        BE    FV20                                                             
*        CLC   FILVTFVL,BFLD2LEN                                                
*        BE    FV20                                                             
*                                                                               
* invalid filter value error message goes here                                  
*                                                                               
FV17A    XC    HALF,HALF                                                        
         ZIC   RF,FILVTNVF                                                      
         MVC   HALF+1,FILVTFVL                                                  
         MH    RF,HALF                                                          
         LA    R3,FILVTOVQ(RF,R3)                                               
         CLI   0(R3),X'00'                                                      
         BNE   FV17                                                             
         B     ERRINVFN                                                         
*                                                                               
FV18     CLC   FILVTNA6,BFLD1        IF FILTER NAME IS 6 CHARACTERS             
         BE    FV20                                                             
*        BNE   FV18A                 LONG IT MUST BE 'MASTER' OR                
*        CLI   FILVTFVL,0            'SUBEST'                                   
*        BE    FV20                                                             
*        CLC   FILVTFVL,BFLD2LEN                                                
*        BE    FV20                                                             
FV18A    XC    HALF,HALF                                                        
         ZIC   RF,FILVTNVF                                                      
         MVC   HALF+1,FILVTFVL                                                  
         MH    RF,HALF                                                          
         LA    R3,FILVTOVQ(RF,R3)                                               
         CLI   0(R3),X'00'                                                      
         BNE   FV18                                                             
*                                                                               
***********************************************************************         
*                                                                               
FV20     LA    R4,FILVTLIS           IF NO VALID FILTERS LISTED FOR             
         ZICM  RF,FILVTNVF,1         NAME, JUMP RIGHT TO CALLING TRANS-         
         BZ    FV40                  LATION ROUTINE                             
         ZIC   RE,FILVTFVL                                                      
         LR    R6,RE                                                            
         BCTR  R6,0                                                             
FV30     EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),BFLD2         IF FILTER VALUE MATCHES AND HAS            
         BNE   FV37                  SAME LENGTH, CALL TRANSLATION              
         CLC   BFLD2LEN,FILVTFVL     ROUTINE                                    
         BNH   FV40                                                             
         B     ERRINVFV                                                         
FV37     LA    R4,0(RE,R4)           BUMP FILTER VALUE LIST                     
         BCT   RF,FV30                                                          
         B     ERRINVFV                                                         
*                                                                               
FV40     ICM   RF,15,FILVTTRT        CALL TRANSLATION ROUTINE                   
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         L     R5,NEXTAVRF         A(NEXT AVAILABLE REC FILTER ENTRY)           
         LA    R1,BSCANLNQ(R1)     BUMP SCANNER TABLE                           
         LA    R3,FILVTAB          RESET FVT                                    
         BCTR  R0,0                DECREMENT AGAIN FOR PARSNIP                  
         BCT   R0,FV10             NEXT FILTER INPUT,SRT                        
FVX      B     XIT                                                              
         DROP  R1                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TYPE HEADING                                         *         
***********************************************************************         
DSPTYPE  NTR1                                                                   
*                                                                               
         ST    R1,ATYPE                                                         
         MVC   0(4,R1),=CL4'Type'                                               
         MVC   0(4,R2),=CL4'----'                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CLEAR OUT FILTER HEADING FIELDS                                        
***********************************************************************         
*                                                                               
CLRFIL   NTR1                                                                   
CLRFIL10 ZIC   R1,0(R2)              LENGTH OF FIELD + HEADER                   
         SH    R1,=H'9'              MINUS HEADER AND 1 FOR EX                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES        BLANK OUT FIELD                            
         OI    6(R2),X'80'           TRANSMIT                                   
*                                                                               
         ZIC   R1,0(R2)              RESTORE LENGTH                             
         AR    R2,R1                 NEXT SCREEN FIELD                          
         CR    R2,R0                 LAST FIELD TO CLEAR?                       
         BH    CLRFILX                                                          
         B     CLRFIL10                                                         
CLRFILX  XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
*        UNSEL - UNPROTECT ALL SEL FLDS                               *         
***********************************************************************         
UNSEL    NTR1                                                                   
         LA    RE,ELSSELH                                                       
         LA    RF,ELSENDH                                                       
UNS10    OI    6(RE),X'80'           UNPROTECT ALL SELECT FIELDS AND            
         CLI   0(RE),11              CLEAR DATA IN FIELD                        
         BNE   *+8                                                              
         NI    1(RE),X'FF'-X'20'                                                
         ZIC   R1,0(RE)                                                         
         SH    R1,=Y(9)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                                                    
         LA    RE,9(R1,RE)           BUMP TO NEXT SELECT HEADER                 
         CR    RF,RE                                                            
         BNL   UNS10                                                            
*                                                                               
UNSX     MVI   DESDISP,0             DESCRIPTIONS HAVE BEEN ERASED              
         MVI   DESPAGE,0             RESET DISPLAY FILTER TABLE FLAG            
*                                                                               
         XC    KEY,KEY               RESTART THE LISTING                        
         LA    RE,ELSSELH                                                       
         OI    6(RE),X'80'+X'40'     POSITION CURSOR TO 1ST SEL FLD             
         B     XIT                                                              
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINVFS MVC   CONHEAD(46),=C'Invalid filter syntax - type ? for more d+        
               etail'                                                           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         J     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         J     SPERREX                                                          
ERRPFKY  MVC   ERRNUM,=AL2(PFKYINV)                                             
         J     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*        GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
PFERR    EQU   559                 INVALID PFKEY                                
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
BADDATE  EQU   20                  INVALID DATES                                
PFKYINV  EQU   1002                INVALID PF KEY                               
DEMERR   EQU   719                 INVALID DEMO NAME                            
ESTERR   EQU   720                 INVALID ESTIMATE NUM                         
MENERR2  EQU   699                 DPT MENU MUST BE 2 CHARS LONG                
FNDERR   EQU   591                 RECORD NOT FOUND                             
SPTLNERR EQU   673                 SPOT LENGTH NOT VALID                        
ALLERR1  EQU   737                 ESTIMATE REQUIRED FOR ALL                    
ESTERR1  EQU   563                 EST CODE MUST BE NUMERIC                     
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1-255                  
DATAERR3 EQU   738                 DATA OPTION INPUTTED > 1                     
DATAERR1 EQU   739                 DATA AND DEMO OPTION INVALID                 
DATAERR2 EQU   740                 DATA AND DISPLAY OPTION INVALID              
DATAERR4 EQU   741                 DISPLAY AND DEMO OPTION INVALID              
DATAERR5 EQU   742                 DEMO OPTION INPUTTED > 1                     
DATAERR6 EQU   743                 DISPLAY OPTION INPUTTED > 1                  
OPTREP1  EQU   744                 BOOK OPTION INPUTTED > 1                     
OPTREP2  EQU   745                 DAILY OPTION INPUTTED > 1                    
OPTREP3  EQU   746                 DATE OPTION INPUTTED > 1                     
OPTREP4  EQU   747                 DPTMENU OPTION INPUTTED > 1                  
OPTREP5  EQU   748                 EST OPTION INPUTTED > 1                      
OPTREP6  EQU   749                 MASTER OPTION INPUTTED > 1                   
OPTREP7  EQU   750                 NMG OPTION INPUTTED > 1                      
OPTREP8  EQU   757                 OOWR OPTION INPUTTED > 1                     
OPTREP9  EQU   758                 REQ OPTION INPUTTED > 1                      
OPTREP10 EQU   759                 SLN OPTION INPUTTED > 1                      
OPTREP11 EQU   760                 SUBEST OPTION INPUTTED > 1                   
OPTREP12 EQU   761                 WIMTR OPTION INPUTTED > 1                    
OPTREP13 EQU   850                 CPPRS OPTION INPUTTED > 1                    
OPTREP14 EQU   991                 TYPE OPTION INPUTTED > 1                     
PROERR1  EQU   766                 CAN'T LIST ACROSS PRO WITH DATE              
PROERR2  EQU   762                 CAN'T LIST ACROSS PRO WITH DATA              
PROERR3  EQU   763                 CAN'T LIST ACROSS PRO WITH DEMO              
PROERR4  EQU   764                 CAN'T LIST ACROSS PRO WITH DISPLAY           
PROERR5  EQU   765                 CAN'T LIST ACROSS PRO WITH EST               
CERR1    EQU   774                 CAN'T LIST ACROSS PRD                        
CERR2    EQU   775                 MUST LIST ACROSS PRD                         
CERR3    EQU   776                 NO EST WHEN LISTING ACROSS PRD               
INVRTYPE EQU   1157                RTYPE MUST BE 1 CHAR LONG                    
BEGRAN   DC    C'N'                BEGINNING RANGE                              
ENDRAN   DC    C'N'                END RANGE                                    
UL       DC    40C'-'                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALID SPOT LENGTHS                                           *         
***********************************************************************         
*                                                                               
SLNTABC  DS    0C                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        FILTER VALIDATION TABLE (FVT)                                *         
***********************************************************************         
*        FILTER NAME,# VALID VALUES,LENGTH OF EACH VALID VALUE                  
*        TRANSLATE ROUTINE,VALID VALUES                                         
FILVTAB  DC    CL7'BOOK   ',AL1(00),AL1(00)          BOOK                       
         DC    AL4(TRBOOK)                                                      
*                                                                               
         DC    CL7'DAILY  ',AL1(02),AL1(01)          DAILY                      
         DC    AL4(TRDAILY),C'YN'                                               
*                                                                               
         DC    CL7'DATA   ',AL1(01),AL1(03)          DATA                       
         DC    AL4(TRDATA),C'CTL'                                               
*                                                                               
         DC    CL7'DATA   ',AL1(06),AL1(04)          DATA                       
         DC    AL4(TRDATA),C'DATENAMEMENUDEMOCOPYTYPE'                          
*                                                                               
         DC    CL7'DATA   ',AL1(02),AL1(07)          DATA                       
         DC    AL4(TRDATA),C'DAYPARTCONTROL'                                    
*                                                                               
         DC    CL7'DATE   ',AL1(00),AL1(00)          DATA                       
         DC    AL4(TRDATE)                                                      
*                                                                               
         DC    CL7'DEMO   ',AL1(00),AL1(00)          DEMO                       
         DC    AL4(TRDEMO)                                                      
*                                                                               
         DC    CL7'DISPLAY',AL1(02),AL1(04)          DISPLAY                    
         DC    AL4(TRDISP),C'LOCKHOLD'                                          
*                                                                               
         DC    CL7'DPTMENU',AL1(00),AL1(00)          DPTMENU                    
         DC    AL4(TRMENU)                                                      
*                                                                               
         DC    CL7'EST    ',AL1(00),AL1(00)          EST                        
         DC    AL4(TREST)                                                       
*                                                                               
         DC    CL7'MASTER ',AL1(02),AL1(01)          MASTER                     
         DC    AL4(TRMST),C'YN'                                                 
*                                                                               
         DC    CL7'NMG    ',AL1(02),AL1(01)          NMG                        
         DC    AL4(TRNMG),C'YN'                                                 
*                                                                               
         DC    CL7'OOWR   ',AL1(02),AL1(01)          OOWR                       
         DC    AL4(TROOWR),C'YN'                                                
*                                                                               
         DC    CL7'REQ    ',AL1(02),AL1(01)          REQ                        
         DC    AL4(TRREQ),C'YN'                                                 
*                                                                               
         DC    CL7'SLN    ',AL1(00),AL1(00)          SLN                        
         DC    AL4(TRSLN)                                                       
*                                                                               
         DC    CL7'SUBEST ',AL1(02),AL1(01)          SUBEST                     
         DC    AL4(TRSUB),C'YN'                                                 
*                                                                               
         DC    CL7'WIMTR  ',AL1(02),AL1(01)          WIMTR                      
         DC    AL4(TRWIM),C'YN'                                                 
*                                                                               
         DC    CL7'TYPE   ',AL1(04),AL1(03)          TYPE                       
         DC    AL4(TRTYPE),C'STWREGBARDIS'                                      
*                                                                               
         DC    CL7'RTYPE  ',AL1(00),AL1(00)          RATE TYPE                  
         DC    AL4(TRRTYPE)                                                     
*                                                                               
         DC    CL7'CPPRS  ',AL1(02),AL1(01)          CPPRS                      
         DC    AL4(TRCPPRS),C'YN'                                               
         DC    XL1'00'                  END OF FILVTAB                          
         EJECT                                                                  
***********************************************************************         
*        FILTER DESCRIPTION TABLE                                     *         
*        FILTER NAME, FILTER DESCRIPTION, FILTER VALUE                *         
***********************************************************************         
FIDETAB  DC    CL7'BOOK'                                                        
         DC    CL30'Display All With Particular   '                             
         DC    CL25'BOOK=10/85'                                                 
         DC    CL7' '                                                           
         DC    CL30'Rating Book'                                                
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DAILY'                                                       
         DC    CL30'Display Only Daily Estimates'                               
         DC    CL25'DAILY=Y'                                                    
*                                                                               
         DC    CL7'DATA'                                                        
         DC    CL30'Display Estimate Copy Code'                                 
         DC    CL25'DATA=COPY'                                                  
*                                                                               
         DC    CL7'DATA'                                                        
         DC    CL30'Display Estimate Start and End'                             
         DC    CL25'DATA=DATE'                                                  
         DC    CL7' '                                                           
         DC    CL30'Dates'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DATA'                                                        
         DC    CL30'Display Estimate First Four'                                
         DC    CL25'DATA=DEMO'                                                  
         DC    CL7' '                                                           
         DC    CL30'Demos'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DATA'                                                        
         DC    CL30'Display Estimate Name'                                      
         DC    CL25'DATA=NAME'                                                  
*                                                                               
         DC    CL7'DATA'                                                        
         DC    CL30'Display Estimate Daypart'                                   
         DC    CL25'DATA=DAYPART or DATA=MENU'                                  
         DC    CL7' '                                                           
         DC    CL30'Menu Number'                                                
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DATE'                                                        
         DC    CL30'List Estimates within Date'                                 
         DC    CL25'DATE=JAN01/yy-FEB20/yy'                                     
         DC    CL7' '                                                           
         DC    CL30'Range'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DEMO'                                                        
         DC    CL30'Display All with Particular'                                
         DC    CL25'DEMO=WM2554'                                                
         DC    CL7' '                                                           
         DC    CL30'Demographic'                                                
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DISPLAY'                                                     
         DC    CL30'Display All Locked or Held'                                 
         DC    CL25'DISPLAY=LOCK or DISPLAY=HOLD'                               
         DC    CL7' '                                                           
         DC    CL30'Estimates'                                                  
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'DPTMENU'                                                     
         DC    CL30'Display All with Particular'                                
         DC    CL25'DPTMENU=1'                                                  
         DC    CL7' '                                                           
         DC    CL30'Department Menu'                                            
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'EST'                                                         
         DC    CL30'Limit Display to One Estimate'                              
         DC    CL25'EST=nnn'                                                    
*                                                                               
         DC    CL7'EST'                                                         
         DC    CL30'Limit Display to Estimate'                                  
         DC    CL25'EST=nnn-nnn'                                                
         DC    CL7' '                                                           
         DC    CL30'Range'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'MASTER'                                                      
         DC    CL30'Display Only Master Estimates'                              
         DC    CL25'MASTER=Y'                                                   
*                                                                               
         DC    CL7'NMG'                                                         
         DC    CL30'Display Estimate with'                                      
         DC    CL25'NMG=Y'                                                      
         DC    CL7' '                                                           
         DC    CL30'Makegood Dates'                                             
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'OOWR'                                                        
         DC    CL30'Display Estimate with Out of'                               
         DC    CL25'OOWR=Y'                                                     
         DC    CL7' '                                                           
         DC    CL30'Week Rotator'                                               
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'REQ'                                                         
         DC    CL30'Display Estimate with Request'                              
         DC    CL25'REQ=Y'                                                      
         DC    CL7' '                                                           
         DC    CL30'Range'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'SLN'                                                         
         DC    CL30'Display All with Particular'                                
         DC    CL25'SLN=30'                                                     
         DC    CL7' '                                                           
         DC    CL30'Spot Length'                                                
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'SUBEST'                                                      
         DC    CL30'Display Only Sub-Estimates'                                 
         DC    CL25'SUBEST=Y'                                                   
*                                                                               
         DC    CL7'WIMTR'                                                       
         DC    CL30'Display Estimate with WIM'                                  
         DC    CL25'WIMTR=Y'                                                    
         DC    CL7' '                                                           
         DC    CL30'Trade'                                                      
         DC    CL25' '                                                          
*                                                                               
         DC    CL7'CPPRS'                                                       
         DC    CL30' '                                                          
         DC    CL25'CPPRS=Y'                                                    
*                                                                               
         DC    X'00'               END OF FIDETAB                               
*                                                                               
***********************************************************************         
*        FILTER TRANSLATION ROUTINES                                  *         
***********************************************************************         
*                                                                               
TRBOOK   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+0-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP1)                                             
         LA    R2,ELSFILH                                                       
         CLI   BOOKFL,C'Y'                                                      
         JE    SPERREX                                                          
*                                                                               
         XC    BOOK,BOOK                                                        
         CLC   BFLD2(6),=C'LATEST'                                              
         BE    TRBOOKX                                                          
         MVC   ERRNUM,=AL2(BADDATE)                                             
         GOTO1 DATVAL,DMCB,(2,BFLD2),WORK                                       
         LA    R2,ELSFILH                                                       
         OC    DMCB(4),DMCB                                                     
         JZ    SPERREX                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   BOOK,WORK+10                                                     
*                                                                               
TRBOOKX  MVI   BOOKFL,C'Y'                                                      
         ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRDAILY  NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP2)                                             
         LA    R2,ELSFILH                                                       
         OC    DAILY,DAILY                                                      
         JNZ   SPERREX                                                          
*                                                                               
         MVI   DAILY,C'N'                                                       
         CLI   BFLD2,C'N'                                                       
         BE    TRDAILYX                                                         
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   DAILY,C'Y'                                                       
*                                                                               
TRDAILYX ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
TRDATE   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+0-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP3)                                             
         LA    R2,ELSFILH                                                       
         OC    DATE1,DATE1                                                      
         JNZ   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(BADDATE)                                             
         USING PERVALD,R6                                                       
         LA    R6,OUTDATE                                                       
         GOTO1 PERVAL,DMCB,(17,BFLD2),(R6)                                      
         CLI   4(R1),X'00'                                                      
         JNE   SPERREX               INVALID DATES                              
         MVC   DATE1,PVALESTA        START DATE                                 
         MVC   DATE2,PVALEEND        END DATE                                   
*                                                                               
TRDATEX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R6                                                               
*                                                                               
**********************************************************************          
*                                                                               
TREST    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R6                                                        
         LR    R6,R1                                                            
         MVC   ERRNUM,=AL2(ESTERR)                                              
*                                                                               
         CLI   BFLD2LEN,0                                                       
         JE    SPERREX                                                          
*                                                                               
         LA    RE,EPROF+1-ESTHDR     EST=NNN   OR   EST=NNN-NNN                 
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         LA    R2,ELSFILH                                                       
         MVC   ERRNUM,=AL2(PROERR5)                                             
         CLI   DATA,C'P'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(OPTREP5)                                             
         CLI   BEGRAN,C'N'                                                      
         JNE   SPERREX                                                          
*                                                                               
         CLC   BFLD2(2),=C'NO'                                                  
         BNE   TREST03                                                          
         MVI   BEGRAN,X'01'                                                     
         MVI   ENDRAN,X'FF'                                                     
         B     TRESTX                                                           
*                                                                               
TREST03  MVI   BEGRAN,C'N'                                                      
         MVI   ENDRAN,C'N'                                                      
*                                                                               
         MVI   TRFLAG,1              PREPARE TO VALIDATE 1ST HALF OF            
         LA    R1,BFLD2              RANGE (R1 AT FIELD, R0 STORES              
         LA    RE,0                  LENGTH, R3 MAXIMUM LENGTH)                 
         B     TREST05                                                          
*                                                                               
TREST04  MVI   TRFLAG,2              PREPARE TO VALIDATE SECOND HALF            
         LA    R1,1(R1)              OF RANGE (R1 AND R4 AT FIELD, R4           
         LR    R4,R1                 WILL NOT BE BUMPED, RE STORES              
         LA    RE,0                  LENGTH, R3 MAXIMUM LENGTH)                 
TREST05  LA    R3,3                                                             
*                                                                               
TREST10  CLI   0(R1),C'-'                                                       
         BE    TREST20                                                          
         CLI   0(R1),C'0'            VALUES OF RANGE MUST FIT BETWEEN           
         BL    TREST30               0 AND 9                                    
         CLI   0(R1),C'9'                                                       
         BH    TREST30                                                          
         CR    RE,R3                 EACH HALF OF RANGE CAN ONLY BE 3           
         JH    SPERREX               LONG                                       
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         B     TREST10                                                          
*                                                                               
TREST20  CLI   TRFLAG,2              2ND HALF OF RANGE CANNOT END IN -          
         JE    SPERREX                                                          
         LA    R3,0                  EACH HALF OF RANGE MUST BE AT              
         CR    RE,R3                 LEAST 1 LONG                               
         JE    SPERREX                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BFLD2(0)                                                     
         CVB   RE,DUB                                                           
         STC   RE,BEGRAN                                                        
         B     TREST04                                                          
*                                                                               
TREST30  CLI   0(R1),C' '                                                       
         JNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(ESTERR)   INVALID ESTIMATE NUMBER                    
         CHI   RE,0                  PREVENT DEATH WITH (EST=6-)                
         JE    SPERREX                                                          
         CLI   TRFLAG,2                                                         
         BE    TREST35                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                IF ONLY ONE ESTIMATE ENTERED (NO           
         B     *+10                  RANGE)...DEFAULT ENDRAN TO BEGRAN          
         PACK  DUB,BFLD2(0)                                                     
         CVB   RE,DUB                                                           
         STC   RE,BEGRAN                                                        
         STC   RE,ENDRAN                                                        
         B     TRESTX                                                           
TREST35  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RE,DUB                                                           
         STC   RE,ENDRAN                                                        
*                                                                               
TRESTX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRDEMO   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         LA    RE,EPROF+2-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         LA    R2,ELSFILH                                                       
         MVC   ERRNUM,=AL2(PROERR3)                                             
         CLI   DATA,C'P'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR5)                                            
         CLI   DATA,C'Z'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR4)                                            
         CLI   DATA,C'L'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR1)                                            
         CLI   DATA,0                                                           
         JNE   SPERREX                                                          
*                                                                               
         MVI   DATA,C'Z'                                                        
         MVC   ERRNUM,=AL2(DEMERR)                                              
         ZIC   RE,BFLD2LEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEMPFLD(0),BFLD2   STORE IN FAKE TWA FIELD                       
         MVI   TEMPFLDH+0,15         LENGTH OF FAKE TWA FIELD                   
         XC    ELEM,ELEM                                                        
         L     R2,AIO2                                                          
         USING DBLOCK,R2             SET UP CALL TO DEMOVAL                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'        SET DBFILE = TPT                           
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'             SET DBSELMED = R FOR RADIO                 
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   TRDEMO10              AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    TRDEMO10                                                         
         MVI   DBSELMED,C'C'                                                    
TRDEMO10 MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0         CALL DEMOVAL                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,TEMPFLDH),(14,ELEM),(C'S',(R2)),EUSRNMS             
         CLI   DMCB+4,0                                                         
         JE    SPERREX               INVALID DEMO                               
*                                                                               
         MVC   DEMOFIL(3),ELEM       STORE 3 CHARACTER VALUE OF DEMO            
*                                                                               
TRDEMOX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP R6                                                                
         DROP R3                                                                
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRDATA   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         LA    R2,ELSFILH                                                       
         CLI   DATA,C'P'           ACROSS ALL PRODUCTS?                         
         BNE   TRDATA10            NO                                           
         MVC   ERRNUM,=AL2(CERR1)                                               
         CLC   BFLD2,=CL17'CONTROL'                                             
         BE    *+14                                                             
         CLC   BFLD2,=CL17'CTL'                                                 
         BNE   TRDATA01                                                         
         LA    R2,ELSESTKH                                                      
         MVC   ERRNUM,=AL2(CERR3)                                               
         OC    ELSESTK,ELSESTK                                                  
         JNZ   SPERREX                                                          
         MVC   CNTL,=X'C0C0C0'       LESS THAN AAA                              
         B     TRDATAX                                                          
*                                                                               
TRDATA01 CLC   BFLD2,=CL17'TYPE'                                                
         JNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR3)                                            
         CLI   TYPEDIS,0                                                        
         JNE   SPERREX                                                          
         MVI   TYPEDIS,C'D'          LIST TYPE ACROSS ALL PRODUCTS              
         B     TRDATAX                                                          
*                                                                               
TRDATA10 LA    R2,ELSFILH                                                       
         MVC   ERRNUM,=AL2(CERR2)                                               
         CLC   BFLD2,=CL17'CONTROL'                                             
         JE    SPERREX                                                          
         CLC   BFLD2,=CL17'CTL'                                                 
         JE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(PROERR2)                                             
         CLI   DATA,C'P'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR1)                                            
         CLI   DATA,C'Z'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR2)                                            
         CLI   DATA,C'L'                                                        
         JE    SPERREX                                                          
*                                                                               
         CLC   BFLD2,=CL17'TYPE'    ALLOW 2 DATA OPTS IF 1 IS DATA=TYPE         
         BNE   TRDATA05                                                         
         MVC   ERRNUM,=AL2(DATAERR3)                                            
         CLI   TYPEDIS,0                                                        
         JNE   SPERREX                                                          
         MVI   TYPEDIS,C'D'          LIST TYPE ACROSS ONE PRODUCT               
         B     TRDATAX                                                          
*                                                                               
TRDATA05 MVC   ERRNUM,=AL2(DATAERR3)                                            
         CLI   DATA,0                                                           
         JNE   SPERREX                                                          
*                                                                               
         CLC   BFLD2,=CL17'DEMO'                                                
         BNE   *+12                                                             
         MVI   DATA,C'E'                                                        
         B     TRDATAX                                                          
*                                                                               
         CLC   BFLD2,=CL17'DATE'                                                
         BNE   *+12                                                             
         MVI   DATA,C'A'                                                        
         B     TRDATAX                                                          
*                                                                               
         CLC   BFLD2,=CL17'NAME'                                                
         BNE   *+12                                                             
         MVI   DATA,C'A'                                                        
         B     TRDATAX                                                          
*                                                                               
         CLC   BFLD2,=CL17'COPY'                                                
         BNE   *+12                                                             
         MVI   DATA,C'C'                                                        
         B     TRDATAX                                                          
*                                                                               
         CLC   BFLD2,=CL17'MENU'                                                
         BE    *+14                                                             
         CLC   BFLD2,=CL17'DAYPART'                                             
         JNE   SPERREX                                                          
         MVI   DATA,C'M'                                                        
*                                                                               
TRDATAX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRMST    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP6)                                             
         LA    R2,ELSFILH                                                       
         OC    MASTER,MASTER                                                    
         JNZ   SPERREX                                                          
*                                                                               
         MVI   MASTER,C'N'                                                      
         CLI   BFLD2,C'N'                                                       
         BE    TRDAILYX                                                         
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   MASTER,C'Y'                                                      
*                                                                               
TRMSTX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRSUB    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP11)                                            
         LA    R2,ELSFILH                                                       
         OC    SUBEST,SUBEST                                                    
         JNZ   SPERREX                                                          
*                                                                               
         MVI   SUBEST,C'N'                                                      
         CLI   BFLD2,C'N'                                                       
         BE    TRDAILYX                                                         
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   SUBEST,C'Y'                                                      
*                                                                               
TRSUBX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRMENU   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+0-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP4)                                             
         LA    R2,ELSFILH                                                       
         CLI   MENUFL,C'Y'                                                      
         JE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(MENERR2)  MUST BE ONE CHARACTER LONG                 
         CLI   BFLD2LEN,1                                                       
         JNE   SPERREX                                                          
*                                                                               
         MVC   MENU,BFLD2            IS DEPARTMENT MENU ON FILE?                
         MVC   DMCB(2),AGENCY        AGENCY                                     
         MVC   DMCB+2(1),QMED        MEDIA                                      
         MVC   DMCB+3(1),MENU        DEPARTMENT MENU                            
         GOTO1 DPTRD,DMCB,,ELEM,DATAMGR                                         
         MVC   ERRNUM,=AL2(FNDERR)                                              
         CLI   DMCB+8,X'FF'          IF MENU NOT ON FILE ... ERROR              
         JE    SPERREX                                                          
*                                                                               
TRMENUX  MVI   MENUFL,C'Y'                                                      
         ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRREQ    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP9)                                             
         LA    R2,ELSFILH                                                       
         OC    REQ,REQ                                                          
         JNZ   SPERREX                                                          
*                                                                               
         MVI   REQ,C'N'                                                         
         CLI   BFLD2,C'N'                                                       
         BE    TRREQX                                                           
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   REQ,C'Y'                                                         
*                                                                               
TRREQX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRSLN    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP10)                                            
         LA    R2,ELSFILH                                                       
         OC    SLN,SLN                                                          
         JNZ   SPERREX                                                          
*                                                                               
         MVI   SLN,0                                                            
         CLI   BFLD2,0                                                          
         BE    TRSLNX                                                           
*                                                                               
         MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 CALLOV,DMCB,0         GET SPSLENTAB                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   QMED,C'T'                                                        
         BE    TRSLN10                                                          
         CLI   QMED,C'N'                                                        
         BE    TRSLN10                                                          
         CLI   QMED,C'C'                                                        
         BE    TRSLN10                                                          
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   QMED,C'R'                                                        
         BE    TRSLN10                                                          
         CLI   QMED,C'X'                                                        
         BE    TRSLN10                                                          
         DC    H'0'                                                             
*                                                                               
TRSLN10  CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    TRSLN15                                                          
         CLC   0(2,R1),AGENCY      ELSE MATCH AGY                               
         BNE   *+14                                                             
TRSLN15  CLC   BYTE,2(R1)          AND MEDIA                                    
         BE    TRSLN20                                                          
*                                                                               
         BXLE  R1,RE,TRSLN10                                                    
         DC    H'0'                                                             
*                                                                               
TRSLN20  AHI   R1,4                POINT BEYOND HEADER                          
*                                                                               
         MVC   ERRNUM,=AL2(SPTLNERR)                                            
         ICM   R4,15,BFLD2B        GET SLN                                      
         AR    R4,R4               X 2                                          
         AR    R4,R1               POINT TO ENTRY                               
         CLI   1(R4),0             SLN VALID?                                   
         BE    SPERREX             NO                                           
         MVC   SLN,BFLD2B+3        SAVE SPOT LEN THAT USER INPUT                
*                                                                               
TRSLNX   LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRNMG    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP7)                                             
         LA    R2,ELSFILH                                                       
         OC    NMG,NMG                                                          
         JNZ   SPERREX                                                          
*                                                                               
         MVI   NMG,C'N'                                                         
         CLI   BFLD2,C'N'                                                       
         BE    TRNMGX                                                           
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   NMG,C'Y'                                                         
*                                                                               
TRNMGX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TROOWR   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP8)                                             
         LA    R2,ELSFILH                                                       
         CLI   OOWRFL,1                                                         
         JE    SPERREX                                                          
*                                                                               
         MVI   OOWRFL,1                                                         
         MVI   OOWR,0                                                           
         CLI   BFLD2,C'N'                                                       
         BE    TROOWRX                                                          
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   OOWR,1                                                           
*                                                                               
TROOWRX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRDISP   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+4-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         LA    R2,ELSFILH                                                       
         MVC   ERRNUM,=AL2(PROERR4)                                             
         CLI   DATA,C'P'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR6)                                            
         CLI   DATA,C'L'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR4)                                            
         CLI   DATA,C'Z'                                                        
         JE    SPERREX                                                          
         MVC   ERRNUM,=AL2(DATAERR2)                                            
         CLI   DATA,0                                                           
         JNE   SPERREX                                                          
*                                                                               
         MVI   DATA,C'L'                                                        
         MVI   DISPLAY,C'H'                                                     
         CLC   BFLD2,=CL17'HOLD'                                                
         BE    TRDISPX                                                          
*                                                                               
         CLC   BFLD2,=CL17'LOCK'                                                
         JNE   SPERREX                                                          
         MVI   DISPLAY,C'L'                                                     
*                                                                               
TRDISPX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*                                                                               
*                                                                               
**********************************************************************          
*                                                                               
TRWIM    NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+3-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP12)                                            
         LA    R2,ELSFILH                                                       
         OC    WIM,WIM                                                          
         JNZ   SPERREX                                                          
*                                                                               
         MVI   WIM,C'N'                                                         
         CLI   BFLD2,C'N'                                                       
         BE    TRWIMX                                                           
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   WIM,C'Y'                                                         
*                                                                               
TRWIMX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRCPPRS  NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,ECPPRS-ESTHDR                                                 
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP13)                                            
         LA    R2,ELSFILH                                                       
         OC    CPPRS,CPPRS                                                      
         JNZ   SPERREX                                                          
*                                                                               
         MVI   CPPRS,C'N'                                                       
         CLI   BFLD2,C'N'                                                       
         BE    TRCPPRSX                                                         
*                                                                               
         CLI   BFLD2,C'Y'                                                       
         JNE   SPERREX                                                          
         MVI   CPPRS,C'Y'                                                       
*                                                                               
TRCPPRSX ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRTYPE   NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+4-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(OPTREP14)                                            
         LA    R2,ELSFILH                                                       
         OC    TYPE,TYPE                                                        
         JNZ   SPERREX                                                          
*                                                                               
         CLC   BFLD2,=CL17'STW'                                                 
         BNE   *+12                                                             
         MVI   TYPE,C'S'                                                        
         B     TRTYPEX                                                          
*                                                                               
         CLC   BFLD2,=CL17'BAR'                                                 
         BNE   *+12                                                             
         MVI   TYPE,C'B'                                                        
         B     TRTYPEX                                                          
*                                                                               
         MVI   TYPE,C'R'                                                        
*                                                                               
TRTYPEX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
TRRTYPE  NTR1  BASE=*,LABEL=*                                                   
         USING BSCAND,R3                                                        
         LR    R3,R1                                                            
         LA    RE,EPROF+0-ESTHDR                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVC   ERRNUM,=AL2(INVRTYPE)                                            
         CLI   BFLD2LEN,1                                                       
         JNE   SPERREX                                                          
*                                                                               
         MVC   RTYPE,BFLD2                                                      
*                                                                               
TRRTYPX  ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    ESTKEY,ESTKEY                                                    
         MVC   ELSMEDN,SPACES        CLEAR MEDIA NAME, CLIENT NAME,             
         OI    ELSMEDNH+6,X'80'      PRODUCT NAME AND ESTIMATE DES-             
         MVC   ELSCLIN,SPACES        CRIPTION FROM SCREEN                       
         OI    ELSCLINH+6,X'80'                                                 
         MVC   ELSPRON,SPACES                                                   
         OI    ELSPRONH+6,X'80'                                                 
         MVC   ELSESTD,SPACES                                                   
         OI    ELSESTDH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ELSMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ELSMEDN,MEDNM         MEDIA NAME                                 
         OI    ELSMEDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         XC    KEY,KEY               BEGIN TO SET UP KEY FOR READHIGH           
         LA    R6,KEY                                                           
         USING ESTHDR,R6                                                        
         MVI   EKEYTYPE,X'00'        RECORD TYPE X'00'                          
         MVC   EKEYAM,BAGYMD         VALIDATED BINARY MEDIA CODE                
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ELSCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE, MOVE BINARY          
         MVC   EKEYCLT,BCLT          REP INTO KEY AND TRANSMIT CLIENT           
         MVC   ELSCLIN,CLTNM         NAME                                       
         OI    ELSCLINH+6,X'80'                                                 
         USING CLTHDR,RE                                                        
         L     RE,AIO                                                           
         MVC   SVCLDLY,CDAILY                                                   
         DROP  RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
         CLI   ELSPROKH+5,0                                                     
         BNE   VK17                                                             
         MVC   ELSPROK(3),=C'ALL'                                               
         OI    ELSPROKH+6,X'80'                                                 
*                                                                               
VK17     CLC   ELSPROK(3),=C'ALL'                                               
         BNE   VK20                                                             
*                                                                               
         LA    R2,ELSESTKH           IF PRODUCT IS INPUTTED AS 'ALL'            
         ZICM  RE,ELSESTKH+5,1       USER WILL LIST SPECIFIED ESTIMATE          
         BNZ   VK17A                 ACROSS ALL PRODUCTS                        
         MVI   ALLEST,C'A'                                                      
         B     VK19A                                                            
*                                                                               
VK17A    MVC   EKEYPRD,=C'AA '                                                  
         MVC   ERRNUM,=AL2(ESTERR1)                                             
         TM    4(R2),X'08'           VALID NUMERIC?                             
         JZ    SPERREX               NO...ERROR                                 
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,ELSESTK(0)                                                   
         CVB   RE,DUB                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR2)                                             
         CH    RE,=H'00'                                                        
         JNH   SPERREX                                                          
         CH    RE,=H'255'                                                       
         JH    SPERREX                                                          
         STC   RE,ALLEST                                                        
VK19A    MVC   ELSESTD,SPACES                                                   
         OI    ELSESTDH+6,X'80'                                                 
         B     VK50                                                             
*                                                                               
***********************************************************************         
*                                                                               
VK20     LA    R2,ELSPROKH           PRODUCT                                    
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   EKEYPRD,QPRD                                                     
         MVC   ELSPRON,PRDNM                                                    
         OI    ELSPRONH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
VK35     ZICM  RE,ELSESTKH+5,1       USER FILTERING ON ESTIMATE?                
         BZ    VK40                                                             
*                                                                               
         LA    R2,ELSESTKH                                                      
         MVC   ERRNUM,=AL2(ESTERR1)                                             
         TM    4(R2),X'08'           VALID NUMERIC?                             
         JZ    SPERREX               NO...ERROR                                 
*                                                                               
         ZICM  RE,ELSESTKH+5,1       GET BINARY ESTIMATE                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,ELSESTK(0)                                                   
         CVB   RE,DUB                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR2)                                             
         CH    RE,=H'00'                                                        
         JNH   SPERREX                                                          
         CH    RE,=H'255'                                                       
         JH    SPERREX                                                          
         STC   RE,EKEYEST            YES ... MOVE ESTIMATE CODE TO KEY          
*                                                                               
***********************************************************************         
*                                                                               
VK40     MVC   ELSESTD,SPACES        INITIALIZE ESTIMATE DESCRIPTION            
         OI    ELSESTDH+6,X'80'      TO SPACES                                  
*                                                                               
         CLI   ELSESTKH+5,0          USER FILTERING ON ESTIMATE?                
         BE    VK50                                                             
*                                                                               
         MVC   TEMPKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                  YES ... IF VALID ESTIMATE CODE             
         CLC   KEY(L'EKEY),KEYSAVE   MOVE ESTIMATE DESCRIP. TO SCREEN           
         BNE   VK49                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R6,AIO                                                           
         MVC   ELSESTD,EDESC                                                    
         OI    ELSESTDH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
VK49     DS    0H                                                               
         MVC   KEY,TEMPKEY                                                      
*                                                                               
***********************************************************************         
*                                                                               
VK50     XC    FILT,FILT                                                        
         CLC   ELSPROK(3),=C'ALL'                                               
         BNE   VKX                                                              
         MVI   DATA,C'P'                                                        
VKX      DS    0H                                                               
         XIT                                                                    
         EJECT                                                                  
***********************************************************************         
*         FMTDEMO                                                     *         
***********************************************************************         
* ROUTINE TO FORMAT DEMOS ... R3 POINTS TO 10 CHARACTER DESCRIPTION   *         
* ... R2 POINTS TO 3 BYTE DEMO ... WORK(1) RETURNS LENGTH ...         *         
* WORK+1 RETURNS DESCRIPTION                                          *         
***********************************************************************         
FMTDEMO  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R4                                                        
         MVC   WORK(11),SPACES       INITIALIZE WORK                            
         MVI   WORK,0                                                           
         CLI   0(R3),C' '            IF NO DEMO TO FORMAT ... EXIT              
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R5,10(R3)                                                        
FMTD5    CLI   0(R5),C' '            SCAN BACKWARDS FOR NON-SPACE               
         BH    FMTD10                                                           
         BCTR  R5,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK               LENGTH OF DEMO INTO WORK                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(R3)       DEMO DESCRIPTION INTO WORK+1               
*                                                                               
         CLI   1(R2),X'21'           IF DOING A USER DEMO, INSERT               
         BNE   FMTD20                USER DEMO HEADER                           
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
         MVC   WORK+4(7),WORK+11                                                
         ZIC   R0,2(R2)              USER NAME NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK               UPDATE LENGTH                              
         AH    R1,=H'3'                                                         
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM                                                 
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1     IF DEMO MATCHES WEIGHTED DEMO              
         MVC   WORK+1(2),=C'W/'      INSERT HEADER                              
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK               UPDATE LENGTH                              
         AH    R1,=H'2'                                                         
         STC   R1,WORK                                                          
FMTDEMOX XIT1                                                                   
         DROP  R4                                                               
***********************************************************************         
*                          PARTOSCAN                                  *         
* REFORMAT PARSNIP BLK IN AIO3 TO LOOK ALMOST LIKE SCANNER OUTPUT     *         
* EXCEPT FLD2 IS LENGTH 16 FOR DATES LIKE JAN1/99-FEB20/99            *         
***********************************************************************         
PARTOSCN NTR1 BASE=*,LABEL=*                                                    
         L     R2,AIO3                                                          
         USING PSND,R2             PARSNIP BLK                                  
         LA    R5,BLOCK                                                         
         USING BSCAND,R5           NEW BIG SCANNER-ISH BLK                      
*                                                                               
PAR10    XC    0(BSCANLNQ,R5),0(R5)                                             
         MVC   BFLD1,SPACES                                                     
         MVC   BFLD2,SPACES                                                     
         LTR   R2,R2               ANY MORE?                                    
         BZ    PARX                                                             
PAR12    CLI   PSNTAG,C'F'         FIELD IS LEFT OF '=' IS FLD1                 
         BE    PAR20                                                            
         CLI   PSNTAG,C'V'         VALUE IS RIGHT OF '=' IS FLD2                
         BE    PAR30                                                            
         B     PARX                                                             
*                                                                               
PAR20    MVC   BFLD1LEN,PSNLEN     MOVE COMPONENTS                              
         MVC   BFLD1VAL,PSNSTAT                                                 
         MVC   BFLD1B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'10'           CAN'T BE >10                                 
         BNH   *+8                                                              
         LA    R1,10                                                            
         SH    R1,=H'1'                                                         
         BM    PAR22                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BFLD1(0),0(R4)                                                   
PAR22    L     R6,PSNFLD           A(NEXT FIELD)                                
         L     R2,PSNVAL           A(NEXT VALUE)                                
         B     PAR12                                                            
*                                                                               
PAR30    MVC   BFLD2LEN,PSNLEN     MOVE COMPONENTS                              
         MVC   BFLD2VAL,PSNSTAT                                                 
         MVC   BFLD2B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'17'           CAN'T BE >16                                 
         BNH   *+8                                                              
         LA    R1,17                                                            
         SH    R1,=H'1'                                                         
         BM    PAR32                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BFLD2(0),0(R4)                                                   
PAR32    LR    R2,R6               NEXT FIELD                                   
         LA    R5,BSCANLNQ(R5)                                                  
         B     PAR10                                                            
*                                                                               
PARX     XIT1                                                                   
         LTORG                                                                  
         DROP  R5,R2                                                            
         EJECT                                                                  
*                                                                               
*&&DO                                                                           
***********************************************************************         
*        DISFTAB - DISPLAY FILTER DESCRIPTION TABLE                   *         
***********************************************************************         
DISFTAB  NTR1  BASE=*,LABEL=*                                                   
         CLI   DESPAGE,0             IF 1ST SCREEN OF DESCRIPTIONS ...          
         BNE   DFT20                                                            
*                                                                               
         MVI   DESDISP,1             DESCRIPTIONS HAVE BEEN DISPLAYED           
         MVI   OKPFKEY,0             PFKEYS HAVE BEEN HIDDEN                    
         OI    ELSFKEYH+1,X'0C'                                                 
         OI    ELSFKEYH+6,X'80'                                                 
*                                                                               
         LA    RE,ELSHEADH           CLEAR, PROTECT AND TRANSMIT ALL            
         LA    RF,ELSENDH            FIELDS AFTER THE KEY                       
DFT10    OI    6(RE),X'80'                                                      
         OI    1(RE),X'20'                                                      
         ZIC   R1,0(RE)                                                         
         SH    R1,=Y(9)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                                                    
         LA    RE,9(R1,RE)           BUMP TO NEXT FLD HEADER                    
         CR    RF,RE                                                            
         BNL   DFT10                                                            
*                                                                               
         LA    R3,ELSFDH1            MOVE HEADERS AND DESCRIPTIONS TO           
         USING FDTLND,R3             TOP OF SCREEN                              
         MVC   FDTLNST(10),=CL10'FilterName'                                    
         MVC   FDTLNDES(11),=CL11'Description'                                  
         MVC   FDTLNFV(13),=CL13'FilterExample'                                 
         LA    R3,ELSFDH2                                                       
         MVC   FDTLNST(10),UL                                                   
         MVC   FDTLNDES(11),UL                                                  
         MVC   FDTLNFV(13),UL                                                   
         MVC   ELSLINE(38),=C'Filter Syntax:  FilterName=FilterValue'           
*                                                                               
***********************************************************************         
*                                                                               
DFT20    LA    R2,FIDETAB            MULTIPLY THE SCREEN NUMBER BY # OF         
         USING FIDETABD,R2           LINES TO DISPLAY AT ONE TIME BY            
         ZIC   RE,DESPAGE            LENGTH OF EACH ENTRY IN THE FIELD          
         MH    RE,=Y(11)             DESCRIPTION TABLE ...                      
         MH    RE,=Y(FIDELENQ)       STARTING POINT SAVED INTO R2               
         AR    R2,RE                                                            
*                                                                               
         LA    R3,ELSFDTLH           LINE TO START LISTING DESCRIPTIONS         
         LA    R0,11                 # LINES TO DISPLAY                         
*                                                                               
DFT30    OI    6(R3),X'80'           MOVE FILTER NAME, DESCRIPTION AND          
         LA    R3,8(R3)              EXAMPLE INTO LINE                          
         MVC   FDTLNFN,FIDEFN                                                   
         MVC   FDTLNDES,FIDEDES                                                 
         MVC   FDTLNFV,FIDEFV                                                   
*                                                                               
         LA    R3,85(R3)             BUMP TO NEXT SELECT FIELD AND NEXT         
         LA    R2,FIDELENQ(R2)       ENTRY IN FIELD DESCRIPTION TABLE           
         CLI   0(R2),0                                                          
         BE    DFT40                                                            
         BCT   R0,DFT30                                                         
*                                                                               
***********************************************************************         
*                                                                               
         IC    RE,DESPAGE            END OF SCREEN REACHED BUT MORE             
         LA    RE,1(RE)              DESCRIPTIONS TO LIST ... INCREMENT         
         STC   RE,DESPAGE            PAGE                                       
*                                                                               
         LA    R2,ELSFILH                                                       
         MVC   CONHEAD(55),=C'Filter description table displayed - hit +        
               enter for next'                                                  
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
***********************************************************************         
*                                                                               
DFT40    MVI   DESPAGE,0             END OF DESCRIPTIONS REACHED BEFORE         
         BCTR  R0,0                  END OF PAGE ... CLEAR PAGE AND             
DFT50    OI    6(R3),X'80'           REST OF THE SCREEN                         
         LA    R3,8(R3)                                                         
         XC    0(74,R3),0(R3)                                                   
         LA    R3,85(R3)                                                        
         BCT   R0,DFT50                                                         
*                                                                               
         LA    R2,ELSFILH                                                       
         MVC   CONHEAD(58),=C'Filter description table ended - hit ente+        
               r for first again'                                               
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
         DROP  R2,R3                                                            
DFTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
***********************************************************************         
*        TABLE DSECTS                                                 *         
***********************************************************************         
FILTERD  DSECT                     REC FILTER DESECT                            
FILTDIS  DS    H                   DISPLACEMENT FROM CLTREC                     
FILTNF   DS    X                   # OF ALTERNATIVE FILTER VALUE                
FILTFL   DS    X                   LENGTH OF EACH FILTER VALUE                  
FILTADD  DS    A                   A(FILTER VALUE LIST)                         
FILTERQ  EQU   *-FILTERD                                                        
*                                                                               
FILVTABD DSECT                     FILTER VALIDATION TABLE DESECT               
FILVTNA7 DS    CL7                 7 CHARACTER FILTER NAME                      
         ORG   FILVTNA7                                                         
FILVTNA4 DS    CL4                                                              
         DS    CL3                                                              
         ORG   FILVTNA7                                                         
FILVTNA3 DS    CL3                                                              
         DS    CL4                                                              
         ORG   FILVTNA7                                                         
FILVTNA5 DS    CL5                                                              
         DS    CL2                                                              
         ORG   FILVTNA7                                                         
FILVTNA6 DS    CL6                                                              
         DS    CL1                                                              
FILVTNVF DS    X                   # OF VALID FILTER VALUE                      
FILVTFVL DS    X                   LENGTH OF EACH FILTER VALUE                  
FILVTTRT DS    AL4                 TRANSLATE ROUTINE ADDRESS                    
FILVTOVQ EQU   *-FILVTABD          OVERHEAD LENGTH                              
FILVTLIS DS    0C                  BEGIN VALID FILTER VALUE LIST                
*                                                                               
PCTTABD  DSECT                     EXTRA/PROFILE CONVERTION TABLE DSECT         
PCTTALEN DS    X                   OVERALL LENGTH OF THE ENTRY                  
PCTTDLEN DS    X                   DISPLAY LENGTH                               
PCTTNV   DS    X                   # VALUES IN THE LIST                         
PCTTOVQ  EQU   *-FILVTABD          OVERHEAD LENGTH                              
PCTTLIS  DS    0C                  LIST BEGINS                                  
*                                                                               
INVFNAMD DSECT                     INVALID FILTER NAME MESSAGE DSECT            
INVFN#   DC    C'#'                                                             
INVFNFN  DS    CL2                 FILTER # ON THE LINE                         
         DS    CL2                                                              
INVFNMSG DC    CL23'Invalid filter name :  '                                    
INVFNIN  DS    CL10                USER INPUTED FILTER NAME                     
INVFVALD DSECT                     INVALID FILTER VALUE MESSAGE ESECT           
INVFV#   DC    C'#'                                                             
INVFVFN  DS    CL2                 FILTER # ON THE LINE                         
         DS    CL2                                                              
INVFVMS1 DC    CL25'INVALID FILTER VALUE FOR '                                  
INVFVFNA DS    CL3                 FILTER NAME                                  
INVFVMS2 DC    CL4' :  '                                                        
INVFVIN  DS    CL10                USER INPUTED FILTER NAME                     
*                                                                               
FIDETABD DSECT                     FILTER DESCRIPTION TABLE (FDT) DSECT         
FIDEFN   DS    CL7                 FILTER NAME                                  
FIDEDES  DS    CL40                FILTER DESCRIPTION                           
FIDEFV   DS    CL25                FILTER VALUES                                
FIDELENQ EQU   *-FIDETABD          ENTRY LENGTH                                 
*                                                                               
FDTLND   DSECT                     FDT LIST LINE DSECT                          
FDTLNST  DS    0C                                                               
         DS    CL3                                                              
FDTLNFN  DS    CL7                 FILTER NAME                                  
         DS    CL4                                                              
FDTLNDES DS    CL30                FILTER DESCRIPTION                           
         DS    CL5                                                              
FDTLNFV  DS    CL25                FILTER VALUES                                
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD                                
         EJECT                                                                  
       ++INCLUDE SPGENEST          ESTIMATE RECORD                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM73D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE DDPERVALD         PERVAL OUTPUT BLOCK                          
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
ATYPE    DS    A                                                                
ATYPE2   DS    A                                                                
ALLEST   DS    XL1                                                              
ESTKEY   DS    CL13                                                             
TEMPKEY  DS    CL13                                                             
DISFLAG  DS    XL1                                                              
TRFLAG   DS    XL1                                                              
CLT3C    DS    CL3                                                              
ERRNUM   DS    XL2                                                              
COUNT    DS    XL1                                                              
OKPFKEY  DS    XL1                                                              
DESDISP  DS    XL1                                                              
DESPAGE  DS    XL1                                                              
SDFLAG   DS    XL1                                                              
NEXTAVSP DS    F                                                                
NEXTAVRF DS    F                                                                
FILTER   DS    30CL6                                                            
FCNTR    DS    XL1                                                              
FILSPACE DS    CL200                                                            
OUTDATE  DS    XL56                                                             
OVSYS    DS    CL1                                                              
SVCLEX   DS    CL15                                                             
TEMPFLDH DS    CL8                                                              
TEMPFLD  DS    CL7                                                              
SVCLDLY  DS    XL1                                                              
*                                                                               
FILT     DS    0XL45                                                            
DATE     DS    XL1                                                              
EST      DS    XL1                                                              
DEMO     DS    XL1                                                              
DISPLAY  DS    XL1                                                              
DATA     DS    CL1                                                              
DEMOFIL  DS    CL7                                                              
REQ      DS    CL1                                                              
NMG      DS    CL1                                                              
BOOK     DS    CL2                                                              
BOOKFL   DS    CL1                                                              
DAILY    DS    CL1                                                              
MENU     DS    CL1                                                              
MENUFL   DS    CL1                                                              
MASTER   DS    CL1                                                              
SUBEST   DS    CL1                                                              
OOWR     DS    CL1                                                              
OOWRFL   DS    CL1                                                              
SLN      DS    CL1                                                              
CPPRS    DS    CL1                                                              
WIM      DS    CL1                                                              
DATE1    DS    CL6                                                              
DATE2    DS    CL6                                                              
CNTL     DS    CL3                                                              
TYPE     DS    CL1                                                              
TYPEDIS  DS    CL1                                                              
RTYPE    DS    CL1                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        BIG SCANNER DSECT                                            *         
***********************************************************************         
*                                                                               
BSCAND   DSECT                     SCANNER LIKE OUTPUT                          
BFLD1LEN DS    CL1                                                              
BFLD2LEN DS    CL1                                                              
BFLD1VAL DS    CL1                                                              
BFLD2VAL DS    CL1                                                              
BFLD1B   DS    CL4                                                              
BFLD2B   DS    CL4                                                              
BFLD1    DS    CL10                                                             
BFLD2    DS    CL17                EXCEPT FLD 2 = LEN 16                        
BSCANLNQ EQU   *-BSCAND                                                         
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
*                                                                               
         ORG   LISTAR                DEFAULT                                    
LSESTC   DS    CL3                   ESTIMATE CODE                              
         DS    CL7                                                              
LSSTD    DS    CL8                   START DATE                                 
         DS    CL5                                                              
LSEDT    DS    CL8                   END DATE                                   
         DS    CL4                                                              
LSESTD   DS    CL19                  ESTIMATE DESCIPTION                        
         DS    CL1                                                              
*                                                                               
         ORG   LSESTD                LOCK PERIOD                                
         DS    CL3                                                              
LSLOCK   DS    CL11                                                             
*                                                                               
         ORG   LSESTD                DEMO POSITION                              
LSPOS    DS    CL10                                                             
*                                                                               
         ORG   LSESTD                COPY CODE                                  
         DS    CL5                                                              
LSCOPY   DS    CL1                                                              
*                                                                               
         ORG   LSESTD                DPT MENU                                   
         DS    CL4                                                              
LSMENU   DS    CL4                                                              
*                                                                               
         ORG   LSESTD                OUT OF WEEK ROTATOR                        
         DS    CL7                                                              
LSOOWR   DS    CL4                                                              
*                                                                               
         ORG   LSSTD                                                            
         DS    CL1                                                              
LSDEMOS  DS    0CL28                                                            
LSDEM1   DS    CL6                   DEMO 1                                     
         DS    CL4                                                              
LSDEM2   DS    CL6                   DEMO 2                                     
         DS    CL4                                                              
LSDEM3   DS    CL6                   DEMO 3                                     
         DS    CL4                                                              
LSDEM4   DS    CL6                   DEMO 4                                     
*                                                                               
         ORG   LISTAR                                                           
LPPRDC   DS    CL3                   PRODUCT CODE                               
         DS    CL2                                                              
LPPRDN   DS    CL20                  PRODUCT DESCIPTION                         
         DS    CL3                                                              
LPESTC   DS    CL3                   ESTIMATE CODE                              
         DS    CL3                                                              
LPESTN   DS    CL20                  ESTIMATE NAME                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG  P                                                                 
PEST     DS    CL3                                                              
         DS    CL4                                                              
PSTART   DS    CL8                                                              
         DS    CL1                                                              
PEND     DS    CL8                                                              
         DS    CL1                                                              
PDES     DS    CL20                                                             
         DS    CL3                                                              
PLOK     DS    CL9                                                              
         DS    CL1                                                              
PD1      DS    CL7                                                              
         DS    CL3                                                              
PD2      DS    CL7                                                              
         DS    CL3                                                              
PD3      DS    CL7                                                              
         DS    CL3                                                              
PD4      DS    CL7                                                              
         DS    CL6                                                              
PMEN     DS    CL1                                                              
         DS    CL10                                                             
PCOP     DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPSFM53   03/21/17'                                      
         END                                                                    
