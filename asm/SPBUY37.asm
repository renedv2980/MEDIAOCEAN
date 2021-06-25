*          DATA SET SPBUY37    AT LEVEL 047 AS OF 04/02/13                      
*PHASE T21137C    <======================                                       
T21137   TITLE 'SPBUY37 - SPOTPAK BUY MOVE'                                     
T21137   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21137,RR=R7                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R9,AOVWORK                                                       
         USING OVWORKD,R9                                                       
         XC    0(256,R9),0(R9)                                                  
         XC    256(256,R9),256(R9)                                              
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         XC    BUYMSG,BUYMSG                                                    
*                                                                               
         C     R7,RELO                                                          
         BE    B2                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R7,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
B2       CLC   SVBUTRCD,=C'*MV'                                                 
         BNE   B4                                                               
         TM    MOVINP1H+4,X'20'    INPUT LINE WAS CHANGED?                      
         BZ    B4                  YES                                          
         CLI   SVDSPMOD,C'C'       TEST MODE=CHECK                              
         BNE   B10                 NO - PROCESS                                 
         B     B100                YES - DISPLAY ONLY                           
*                                                                               
B4       BRAS  RE,VALIN            VALIDATE MOVE INPUT DATA                     
*                                                                               
B6       BRAS  RE,DISPLAY                                                       
*                                                                               
B8       OI    MOVINP1H+4,X'20'    SET VALIDATED                                
         MVC   BUTRCODE,=C'*MV'                                                 
         B     EXIT                                                             
*                                                                               
B10      CLI   PFKEY,0                                                          
         BNE   BPF                                                              
         LA    R2,MOVDS1AH         POINT TO FIRST SKED LINE                     
         USING LINED,R2                                                         
*                                                                               
B12      OC    LINLIN,LINLIN       ANY DATA ON THIS LINE                        
         BZ    B16                 NO - DONE                                    
         TM    LINHDRBH+1,X'20'    TEST SKED AREA PROTECTED                     
         BO    B14                 YES - SKIP                                   
         TM    LINHDRBH+4,X'20'    TEST SKED AREA CHANGED                       
         BZ    B20                 YES - GO PROCESS                             
*                                                                               
B14      LA    R2,LINNEXT                                                       
         LA    R0,MOVPFKH                                                       
         CR    R2,R0                                                            
         BL    B12                                                              
*                                                                               
B16      B     B100                GO CONTINUE DISPLAY                          
*                                                                               
B20      LA    R2,MOVDS1AH         POINT TO FIRST                               
         LA    R4,SVMVBUYS         LINENUMS/DISK ADDRS                          
*                                                                               
B22      XC    FROMDATA,FROMDATA                                                
         LA    R0,TODATA                                                        
         LHI   R1,TODATAX-TODATA                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    R2,FROMTWA          SAVE FLDHDR ADDRESS                          
         MVC   FROMCLT,SVCLT       FROM CLT                                     
         MVC   FROMPRD,SVPRD       PRD                                          
         MVC   FROMEST,SVEST       EST                                          
         MVC   FROMSTP,SVSTARTP    EST START                                    
         MVC   FROMENDP,SVENDP     EST END                                      
         MVC   FROMLINE,1(R4)      LINE NUMBER                                  
         MVC   FROMDA,2(R4)        DISK ADDRESS                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),FROMDA                                                 
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
* SAVE FROM RECORD IN AREC3                                                     
*                                                                               
B24      MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3                                                   
         GOTO1 MOVEREC                                                          
*                                                                               
         BRAS  RE,CHKINPER         MAKE SURE ALL SPOTS IN BUY PER               
         BRAS  RE,GETOLD                                                        
         MVC   FROLDSKD,SKED                                                    
*                                                                               
         LA    R2,LINNEXT          NEXT INPUT LINE                              
         LA    R4,8(R4)            NEXT LINENUM/DISKADDR                        
*                                                                               
* BUILD 'TO' DATA TABLE                                                         
*                                                                               
         LA    R7,SVMVLIST         'TO' CLT/PRD/EST CODES                       
         USING MVLISTD,R7                                                       
*                                                                               
         LA    R5,TODATA                                                        
         USING TODATAD,R5                                                       
*                                                                               
B30      ST    R2,TOTWA            SAVE FLDHDR ADDRESS                          
         ST    R7,TOMVLIST         SAVE A(SVMVLIST ENTRY)                       
         ST    R4,TOMVBUY          SAVE A(SVMVBUY ENTRY)                        
         MVC   TOCLT,MVBCLT        TO CLT                                       
         MVC   TOPRD,MVBPRD        PRD                                          
         MVC   TOEST,MVBEST        EST                                          
         MVC   TOLINE,1(R4)        LINE NUMBER                                  
         MVC   TOSTP,MVSTARTP      EST START                                    
         MVC   TOENDP,MVENDP       EST END                                      
         MVC   TODA,2(R4)          DISK ADDRESS                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),TODA                                                   
*                                                                               
         XC    SKED,SKED           CLEAR SKED BUILD AREA                        
         OC    TODA,TODA           TEST ANY OLD RECORD                          
         BZ    B32                                                              
         CLC   TODA,=X'FFFFFFFF'   TEST 'TO' LINE NOT FOUND                     
         BE    B34                 YES- DON'T BLOW UP !                         
* READ OLD RECORD AND EXTRACT SKED DATA                                         
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,GETOLD                                                        
         MVC   TOOLDSKD,SKED       AND LEAVE IN SKED BUILD AREA                 
*                                                                               
B32      BRAS  RE,EDITSKD          EDIT 'TO' LINE SKED DATA                     
         BNE   B20ERR                                                           
         MVC   TONEWSKD,SKED       SAVE SKED DATA                               
*                                                                               
B34      LA    R2,LINNEXT                                                       
         LA    R5,L'TODATA(R5)                                                  
         LA    R4,8(R4)            NEXT LINENUM/DA                              
         LA    R7,MVNEXT                                                        
         OC    MVQCLT,MVQCLT       TEST MORE 'TO' DATA                          
         BNZ   B30                                                              
         B     B40                 DONE WITH THIS FROM/TO SET                   
*                                                                               
B20ERR   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(SKEDERR)                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LA    R0,LINHDRBH         A(FLDHDR)                                    
         SR    R0,R3               R3 = A(TWA)                                  
         STH   R0,TIOBCURD                                                      
         LA    R0,LINSKED          START OF SKED FIELD                          
         L     RE,FADDR            EDITSK SET FIELD ADDR HERE                   
         SR    RE,R0                                                            
         STC   RE,TIOBCURI         POINT TO THE PLACE IN FIELD                  
         GOTO1 ERROR                                                            
         EJECT                                                                  
*==============================================================                 
* ONE SET OF BUYS VALIDATED -                                                   
* SEE IF THERE ARE ENOUGH SPOTS                                                 
*==============================================================                 
         SPACE                                                                  
B40      XC    SKED,SKED                                                        
*                                                                               
         LA    R5,TODATA                                                        
         USING TODATAD,R5                                                       
*                                                                               
B42      SR    R6,R6               CLEAR INDEX REG                              
*                                                                               
B44      SR    RE,RE                                                            
         IC    RE,TOOLDSKD(R6)                                                  
         SR    RF,RF                                                            
         IC    RF,TONEWSKD(R6)                                                  
         SR    RE,RF               OLD-NEW GIVES SIGNED CHANGE                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SKED(R6)                                                      
         SLL   R0,24                                                            
         SRA   R0,24                                                            
         AR    R0,RE                                                            
         STC   R0,SKED(R6)                                                      
*                                                                               
         AHI   R6,1                                                             
         CHI   R6,14                                                            
         BL    B44                                                              
*                                                                               
         AHI   R5,L'TODATA                                                      
         OC    0(4,R5),0(R5)       TEST FOR MORE 'TO' ENTRIES                   
         BNZ   B42                                                              
*                                                                               
* NOW COMPARE TO AVAILABLE SPOTS IN FROLDSKD                                    
*                                                                               
         SR    R6,R6               CLEAR INDEX REG                              
*                                                                               
B50      SR    RE,RE                                                            
         IC    RE,SKED(R6)         GET CHANGE IN NPW                            
         SLL   RE,24                                                            
         SRA   RE,24               SET ALGEGBRAIC VALUE IN REG                  
         STC   RE,FRNEWSKD(R6)     AND SAVE IN NEWSKD                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FROLDSKD(R6)     CURRENT NPW                                  
         SLL   RF,24                                                            
         SRA   RF,24                                                            
         AR    RF,RE                                                            
         BM    B50ERR                                                           
         AHI   R6,1                                                             
         CHI   R6,14                                                            
         BL    B50                                                              
         B     B60                                                              
*                                                                               
B50ERR   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(MVNOSPTS)                                              
         L     R2,FROMTWA                                                       
         LA    R2,LINNEXT          POINT TO FIRST 'TO' LINE                     
         AHI   R2,LINHDRBH-LINHDRAH                                             
         GOTO1 ERROR                                                            
         EJECT                                                                  
*===============================================================                
* CREATE/UPDATE 'TO' BUYLINES                                                   
*===============================================================                
         SPACE                                                                  
B60      LA    R5,TODATA                                                        
         USING TODATAD,R5                                                       
*                                                                               
B62      CLC   TOOLDSKD(14),TONEWSKD  ANY NPW CHANGES HERE ?                    
         BE    B72                    NO - SKIP                                 
*                                                                               
         OC    TODA,TODA           TEST LINE EXISTS                             
         BNZ   B65                                                              
*                                                                               
         BRAS  RE,NEWBUY                                                        
*                                                                               
         BRAS  RE,SETBDDTS         SET START/END DATES                          
*                                                                               
         MVC   SKED(14),TONEWSKD   SET TO ADD NPW AS INPUT                      
*                                                                               
         BRAS  RE,SETNPW           DETERMINE MOST FREQUENT NPW                  
         MVC   BDNOWK,SKDHINPW     SET SPOTS/WEEK IN NEW BUY                    
         BRAS  RE,SKEDBUY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADDREC                                                           
* UPDATE LINENUM/DISKADDR TABLE                                                 
         L     RE,TOMVBUY                                                       
         MVC   1(1,RE),BUYKEY+10                                                
         MVC   TOLINE,BUYKEY+10                                                 
         MVC   2(4,RE),KEY+14                                                   
         MVC   TODA,KEY+14                                                      
* NEED TO REDISPLAY CPE-LIN                                                     
         LR    R0,R2                                                            
         L     R2,TOTWA            POINT TO LINE IN TWA                         
         OI    LINHDRAH+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    RE,LINCPE+14                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LR    R2,R0               RESTORE CURRENT TWA POINTER                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEY+10        LINE NUMBER FROM SVMVLIST                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   1(RE),C'-'                                                       
         UNPK  2(3,RE),DUB                                                      
         B     B70                                                              
         EJECT                                                                  
*=================================================================              
* READ EXISTING 'TO' BUYLINE                                                    
*=================================================================              
         SPACE 1                                                                
B65      XC    KEY,KEY                                                          
         MVC   KEY+14(4),TODA                                                   
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SKED(14),TONEWSKD   SET INPUT NPW AND                            
         BRAS  RE,SETNPW           DETERMINE MOST FREQUENT NPW                  
* SET CHANGE IN NPW IN SKED                                                     
         SR    R6,R6               CLEAR INDEX REG                              
*                                                                               
B67      SR    RE,RE                                                            
         IC    RE,TOOLDSKD(R6)     OLD NPW                                      
         SR    RF,RF                                                            
         IC    RF,TONEWSKD(R6)     NEW NPW                                      
         SR    RF,RE                                                            
         STC   RF,SKED(R6)                                                      
         AHI   R6,1                                                             
         CHI   R6,14                                                            
         BL    B67                                                              
*                                                                               
         BRAS  RE,SKEDBUY                                                       
         BNE   BSKERR                                                           
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
B70      LR    R0,R2                                                            
         L     R2,TOTWA                                                         
         BRAS  RE,BLDSKED          REDISPLAY SKED DATA                          
         LR    R2,R0                                                            
*                                                                               
B72      AHI   R5,L'TODATA                                                      
         OC    0(4,R5),0(R5)       TEST FOR MORE 'TO' ENTRIES                   
         BNZ   B62                                                              
         EJECT                                                                  
*=================================================================              
* NOW UPDATE FROM BUY                                                           
*=================================================================              
         SPACE 1                                                                
B80      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),FROMDA                                                 
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,FROMDATA                                                      
         MVC   FROMPRD,BDMASPRD    SET PRD FOR ALLOCATION                       
*                                                                               
         MVC   SKED(14),FRNEWSKD   SET NPW TO BE ADDED                          
         OC    SKED(14),SKED                                                    
         BZ    B82                                                              
         BRAS  RE,SKEDBUY                                                       
         BNE   BSKERR                                                           
*                                                                               
         BRAS  RE,BLDFROM          INSERT 'FROM' ELEMENTS                       
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         LR    R0,R2               SAVE R2                                      
         L     R2,FROMTWA          POINT TO SKED DATA LINE                      
         BRAS  RE,BLDSKED          REDISPLAY SKED DATA                          
         LR    R2,R0               RESTORE R2                                   
         SPACE 1                                                                
*===============================================================                
* DONE WITH THIS FROM/TO SET - SEE IF ANY MORE                                  
*===============================================================                
         SPACE 1                                                                
B82      CLI   0(R4),C'F'          TEST ANOTHER 'FROM' LINE                     
         BE    B22                                                              
         MVC   BUYMSG(20),=CL20'** BUYS UPDATED **'                             
         B     EXIT                                                             
*                                                                               
BSKERR   L     R2,TOTWA            SPOTS NOT IN BUY DESC PERIOD                 
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(OUTOFBUY)                                              
         GOTO1 ERROR                                                            
         EJECT                                                                  
*===============================================================                
* BUMP TO NEXT LINE NUMBER IN SVKEY AND CONTINUE                                
*===============================================================                
         SPACE 1                                                                
B100     CLI   SVKEY+11,255                                                     
         BNE   B102                                                             
         BRAS  RE,NOMORE                                                        
         MVC   BUYMSG(40),2(R1)                                                 
         OI    MOVINP1H+6,X'40'                                                 
         J     EXIT                                                             
*                                                                               
B102     SR    R0,R0                                                            
         IC    R0,SVKEY+11         GET LAST BUYLINE PROCESSED                   
         AHI   R0,1                                                             
         STC   R0,SVKEY+11                                                      
*                                                                               
         TWAXC MOVHL2BH,MOVPFKH-1,PROT=Y CLEAR LINES BELOW INPUT LINE           
         LA    R2,MOVLAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
         B     B6                                                               
         EJECT                                                                  
*====================================================================           
* PROCESS PFKEY REQUESTS                                                        
*====================================================================           
         SPACE 1                                                                
BPF      DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         CHI   R0,2                                                             
         BNE   *+12                                                             
         BRAS  RE,GOMIS                                                         
         B     EXIT                                                             
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADPFKEY)                                              
         GOTO1 ERROR                                                            
         EJECT                                                                  
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
*====================================================================           
* READ BUYLINE AND BUILD TABLE OF NPW IN SKED                                   
*====================================================================           
         SPACE 1                                                                
GETOLD   NTR1  BASE=*,LABEL=*                                                   
         XC    SKED,SKED                                                        
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,SKED                                                          
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B' <=== DO NOT MOVE OTO'S                              
*                                                                               
GETOLD2  BRAS  RE,NEXTEL                                                        
         BNE   GETOLD4                                                          
         CLC   2(2,R6),SVMVEND     TEST AFTER PERIOD END                        
         BNL   GETOLD4                                                          
         CLC   2(2,R6),0(R1)       < WEEK START DATE                            
         BL    GETOLD2                                                          
         OC    2(2,R1),2(R1)       TEST LAST WEEK                               
         BZ    *+14                                                             
         CLC   2(2,R6),2(R1)       >= WEEK END DATE                             
         BNL   GETOLD4             YES                                          
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   GETOLD2                                                          
         BCT   R7,GETOLD2          BUMP COUNTER AND CONTINUE                    
*                                                                               
GETOLD4  LPR   R7,R7                                                            
         STC   R7,0(R4)                                                         
*                                                                               
GETOLD6  LA    R4,1(R4)            NEXT COUNTER POSN                            
         LA    R1,2(R1)            NEXT WEEK IN LIST                            
         OC    0(2,R1),0(R1)                                                    
         JZ    EXIT                                                             
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         B     GETOLD2                                                          
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* EDIT SKED DATA INPUT                                                          
* ON ENTRY, R2 POINTS TO INPUT LINE, R5 POINTS TO TODATA ENTRY                  
* ON ERROR, R4 HAS ADDRESS OF FIELD IN ERROR                                    
*====================================================================           
         SPACE 1                                                                
         USING TODATAD,R5                                                       
*                                                                               
EDITSKD  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,LINSKED          POINT TO INPUT                               
         OC    LINSKED,SPACES      MAKE X'00' INTO SPACES                       
*                                                                               
         LA    R2,SKED                                                          
         LA    R1,SVMVDTS          POINT TO LIST OF WEEK DATES                  
*                                                                               
EDSK2    CLI   0(R4),C' '                                                       
         BH    EDSK50              INPUT IS NOT POSITIONAL BY WEEK              
         CLI   1(R4),C' '                                                       
         BH    EDSK20                                                           
         CLI   2(R4),C'.'                                                       
         BE    EDSK24                                                           
         CLI   2(R4),C'X'                                                       
         BE    EDSK24                                                           
         CLI   2(R4),C' '                                                       
         BH    EDSK10                                                           
         B     EDSK24                                                           
*                                                                               
EDSK10   CLI   2(R4),C'0'          ONLY ONE CHAR                                
         BL    EDSKERR                                                          
         CLI   2(R4),C'9'                                                       
         BH    EDSKERR                                                          
         MVC   0(1,R2),2(R4)                                                    
         NI    0(R2),X'0F'                                                      
         B     EDSK22                                                           
*                                                                               
EDSK20   CLI   1(R4),C'0'                                                       
         BL    EDSKERR                                                          
         CLI   1(R4),C'9'                                                       
         BH    EDSKERR                                                          
         CLI   2(R4),C'0'                                                       
         BL    EDSKERR                                                          
         CLI   2(R4),C'9'                                                       
         BH    EDSKERR                                                          
         PACK  DUB,1(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,0(R2)                                                         
*                                                                               
EDSK22   LTR   R0,R0                                                            
         BZ    EDSK24                                                           
         CLC   0(2,R1),TOSTP       TEST WEEK IN TO EST PER                      
         BL    EDSKERR                                                          
         CLC   0(2,R1),TOENDP                                                   
         BH    EDSKERR                                                          
*                                                                               
EDSK24   LA    R1,2(R1)            NEXT WEEK IN LIST                            
         LA    R2,1(R2)            NEXT WEEK IN SKED TABLE                      
         LA    R4,3(R4)            POINT TO NEXT INPUT CHAR                     
         CLI   0(R4),C'/'                                                       
         BNE   EDSK26                                                           
         LA    R4,1(R4)            POINT TO NEXT INPUT CHAR                     
         B     EDSK50                                                           
*                                                                               
EDSK26   CLI   0(R4),C'+'                                                       
         BE    EDSK40                                                           
         LA    R0,SKED+13                                                       
         CR    R2,R0                                                            
         BH    EDSKX                                                            
         CLI   0(R4),C' '                                                       
         BE    EDSK2                                                            
*                                                                               
EDSKERR  ST    R4,FADDR            SET ADDRESS OF CHAR IN ERROR                 
         J     NEQXIT                                                           
*                                                                               
* INPUT IS A C'+'                                                               
*                                                                               
EDSK40   CLI   1(R4),C' '          SHOULD BE NO CHAR FOLLOWING                  
         BH    EDSKERR                                                          
         LA    R0,SKED+13                                                       
         CR    R2,R0                                                            
         BNL   EDSKERR                                                          
*                                                                               
         SR    RE,RE                                                            
         BCTR  R2,0                BACK UP TO LAST VALUE                        
         IC    RE,0(R2)                                                         
         AHI   R1,-2               BACK UP DATE TABLE ENTRY                     
*                                                                               
EDSK42   STC   RE,0(R2)                                                         
         LA    R2,1(R2)                                                         
         LA    R1,2(R1)            NEXT WEEK IN TABLE                           
         OC    0(2,R1),0(R1)       TEST ANY MORE WEEKS                          
         BZ    EDSKX                                                            
         CLC   0(2,R1),TOENDP      TEST PAST EST END                            
         BH    EDSKX                                                            
         LA    R0,SKED+14          POINT TO LAST WEEK                           
         CR    R2,R0                                                            
         BL    EDSK42                                                           
         B     EDSKX                                                            
*                                                                               
EDSK50   CLI   0(R4),C'/'          TEST FOR NO CHARS                            
         BNE   EDSK52                                                           
         LA    R4,1(R4)            RIGHT - SKIP THIS WEEK                       
         B     EDSK60              NO SPOTS THIS WEEK                           
*                                                                               
EDSK52   CLI   1(R4),C' '          TEST FOR 1 CHAR FIELD                        
         BE    EDSK54                                                           
         CLI   1(R4),C'/'                                                       
         BE    EDSK54                                                           
         CLI   1(R4),C'+'                                                       
         BNE   EDSK56              NO                                           
*                                                                               
EDSK54   CLI   0(R4),C'0'          THIS IS A ONE DIGIT NPW                      
         BL    EDSKERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    EDSKERR                                                          
         MVC   0(1,R2),0(R4)                                                    
         NI    0(R2),X'0F'                                                      
         CLI   1(R4),C' '          SPACE IS FINAL TERMINATOR                    
         BE    EDSKX                                                            
         LA    R4,1(R4)            POINT AT TERMINATOR                          
         CLI   0(R4),C'+'                                                       
         BNE   EDSK55                                                           
         LA    R2,1(R2)            NEXT WEEK IN SKED                            
         LA    R1,2(R1)            NEXT WEEK IN DATE LIST                       
         B     EDSK40                                                           
*                                                                               
EDSK55   LA    R4,1(R4)            POINT BEYOND C'/'                            
         B     EDSK60                                                           
*                                                                               
EDSK56   CLI   2(R4),C' '                                                       
         BE    EDSK58                                                           
         CLI   2(R4),C'+'                                                       
         BE    EDSK58                                                           
         CLI   2(R4),C'/'                                                       
         BNE   EDSKERR                                                          
*                                                                               
EDSK58   CLI   0(R4),C'0'                                                       
         BL    EDSKERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    EDSKERR                                                          
         CLI   1(R4),C'0'                                                       
         BL    EDSKERR                                                          
         CLI   1(R4),C'9'                                                       
         BH    EDSKERR                                                          
         PACK  DUB,0(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,0(R2)                                                         
         LTR   R0,R0                                                            
         BZ    EDSK59                                                           
* SEE IF WEEK IN TO EST PER                                                     
         CLC   0(2,R1),TOSTP                                                    
         BL    EDSKERR                                                          
         CLC   0(2,R1),TOENDP                                                   
         BH    EDSKERR                                                          
*                                                                               
EDSK59   CLI   2(R4),C' '          SPACE IS FINAL TERMINATOR                    
         BE    EDSKX                                                            
         LA    R4,2(R4)            POINT AT TERMINATOR                          
         CLI   0(R4),C'+'                                                       
         BE    EDSK40                                                           
         LA    R4,1(R4)            POINT TO NEXT INPUT                          
*                                                                               
EDSK60   LA    R1,2(R1)            NEXT WEEK IN TABLE                           
         LA    R2,1(R2)            NEXT WEEK                                    
         LA    R0,SKED+14                                                       
         CR    R2,R0                                                            
         BL    EDSK50                                                           
*                                                                               
EDSKX    J     EQXIT                                                            
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* READ THE BUYLINES AND DISPLAY WEEKLY SPOTS                                    
*====================================================================           
         SPACE 1                                                                
DISPLAY  NTR1  BASE=*,LABEL=*                                                   
         XC    MOVHL2B,MOVHL2B                                                  
         OI    MOVHL2BH+6,X'80'    XMT                                          
         XC    MOVHL3B,MOVHL3B                                                  
         OI    MOVHL3BH+6,X'80'    XMT                                          
*                                                                               
         XC    BUDATA,BUDATA                                                    
         XC    SVMVDTS(30),SVMVDTS                                              
*                                                                               
         MVC   WORK(6),SVSTART                                                  
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
         MVC   SVMVSDAY,0(R1)      SAVE EST START DAY NUM                       
*                                                                               
         LA    R4,BUDATA+6                                                      
         LA    R5,14               MAX WEEKS                                    
         LA    R6,SVMVDTS                                                       
*                                                                               
DSP2     GOTO1 VDATCON,DMCB,WORK,(2,(R6))  GET 2BYTE DATES                      
         GOTO1 (RF),(R1),,(4,(R4))  GET MMMDD DATES                             
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R6,2(R6)                                                         
         GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),SVEND                                                    
         BH    DSP10                                                            
         BCT   R5,DSP2                                                          
*                                                                               
DSP10    GOTO1 VDATCON,DMCB,WORK,(2,SVMVEND) AND SAVE THE DATE                  
*                                                                               
* COUNT NUMBER OF WEEKS IN EACH MONTH                                           
*                                                                               
         LA    R4,BUDATA           WEEK COUNTERS                                
*                                                                               
         LHI   R0,1                                                             
         LA    R1,BUDATA+6         FIRST MMMDD DATE                             
*                                                                               
DSP12    CLC   0(3,R1),5(R1)       NEXT WEEK IN SAME MONTH                      
         BE    DSP16                                                            
*                                                                               
DSP14    STC   R0,0(R4)            SET NUM WEEKS THIS MONTH                     
         SR    R0,R0               RESET COUNTER                                
         CLI   5(R1),0             ANY MORE WEEKS                               
         BE    DSP20               NO                                           
         LA    R4,1(R4)            NEXT COUNTER                                 
*                                                                               
DSP16    LA    R1,5(R1)                                                         
         AHI   R0,1                                                             
         B     DSP12                                                            
*                                                                               
DSP20    LA    R1,MOVHL2B          MONTH LINE                                   
*                                                                               
         LA    R4,BUDATA           POINT TO WEEKS/MONTH LIST                    
         LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
*                                                                               
DSP22    SR    RE,RE                                                            
         ICM   RE,1,0(R4)          TEST ANY WEEKS THIS MONTH                    
         BZ    DSP24               NO - DONE                                    
         BCTR  RE,0                                                             
         MHI   RE,L'DSPTAB                                                      
         LA    RE,DSPTAB(RE)       POINT TO TABLE ENTRY                         
         SR    RF,RF                                                            
         IC    RF,0(RE)            DATA LEN                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),2(RE)       MOVE IN TITLE                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(RE)            GET DSPL TO XXX                              
         AR    RF,R1                                                            
         MVC   0(3,RF),0(R5)       MOVE MONTH OVER XXX                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(RE)            GET ENTRY LENGTH FROM TABLE                  
         AR    R1,RF               NEXT POSN IN MONTH LINE                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R4)            NUMBER OF WEEKS THIS MONTH                   
         MHI   R0,5                                                             
         AR    R5,R0               FIRST MMMDD OF NEXT MONTH                    
         LA    R4,1(R4)            NEXT MONTH WEEK COUNT                        
         B     DSP22                                                            
*                                                                               
DSP24    LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
         LA    R1,MOVHL3B          POINT TO WEEK LINE                           
*                                                                               
DSP26    MVC   1(2,R1),3(R5)                                                    
         LA    R1,3(R1)                                                         
         LA    R5,5(R5)            NEXT WEEK                                    
         CLI   0(R5),0                                                          
         BNE   DSP26                                                            
         XC    BUDATA,BUDATA                                                    
         B     DSP30                                                            
*                                                                               
DSPTAB   DS    0CL17               LEN TO MOVE, DSPL TO XXX                     
         DC    AL1(3,0),CL15'XXX'                                               
         DC    AL1(6,2),CL15' -XXX-'                                            
         DC    AL1(9,3),CL15' --XXX---'                                         
         DC    AL1(12,5),CL15' ----XXX----'                                     
         DC    AL1(15,6),CL15' -----XXX------'                                  
         EJECT                                                                  
*===========================================================                    
* READ THE BUYLINES                                                             
*===========================================================                    
         SPACE 1                                                                
DSP30    LA    R2,MOVDS1AH         FIRST DISPLAY LINE (13)                      
         USING LINED,R2                                                         
*                                                                               
         MVI   LINENUM,13          SET FIRST DISPLAY LINE NUMBER                
         XC    SVMVBUYS(SVMVBUYX-SVMVBUYS),SVMVBUYS                             
         LA    R4,SVMVBUYS                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
*                                                                               
DSP32    GOTO1 HIGH                                                             
*                                                                               
DSP34    CLC   KEY(11),KEYSAVE     SAME A-M/C/P/MKT/STA/EST                     
         BNE   DSPX                REMEMBER TO CHECK NONE DISPLAYED             
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         CLC   KEY(6),KEYSAVE      MAKE SURE NOT A SPILL POINTER                
         BE    DSP36                                                            
*                                                                               
DSP35    GOTO1 SEQ                                                              
         B     DSP34                                                            
*                                                                               
DSP36    CLI   SVDSPMOD,C'C'       TEST CHECK MODE                              
         BNE   DSP37                                                            
         LA    R6,BDELEM           FIND A SPOT                                  
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
DSP36A   BRAS  RE,NEXTEL                                                        
         BNE   DSP35                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   DSP36A              IF SO, LOOP                                  
*                                                                               
DSP37    MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                                                             
         STC   R0,FRSTDAY          SAVE START DAY                               
*                                                                               
         MVC   STTIME,BDTIMST      SAVE START/END TIMES                         
         MVC   ENDTIME,BDTIMEND                                                 
*                                                                               
         CLI   SVDPTOPT,0                                                       
         BE    *+14                                                             
         CLC   BDDAYPT,SVDPTOPT                                                 
         BNE   DSP35                                                            
*                                                                               
         MVI   0(R4),C'F'          SET 'FROM' LINE                              
         MVC   1(1,R4),KEY+11      SAVE LINE NUMBER                             
         MVC   2(4,R4),KEY+14      SAVE DISK ADDRESS                            
         LA    R4,8(R4)            NEXT LINE/DISKADDR                           
*                                                                               
         OI    LINHDRAH+1,X'08'    HIGH INT                                     
         OI    LINHDRBH+1,X'08'    HIGH INT                                     
         OI    LINHDRAH+6,X'80'    XMT                                          
         OI    LINHDRBH+6,X'80'    XMT                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEY+10                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINLIN,DUB                                                       
*                                                                               
         LA    R5,DSPAREA          POINT TO BUILD AREA                          
         XC    DSPAREA,DSPAREA                                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVEOWSDY         OUT-OF-WEEK START DAY                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
         OI    BYTE,X'80'          11 BYTE OUTPUT                               
         GOTO1 VDAYUNPK,DMCB,(BYTE,BDDAY),WORK                                  
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,11                                                            
*                                                                               
DSP38    CLI   0(R1),C','          CHANGE COMMAS TO SLASHES                     
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,DSP38                                                         
         MVC   0(8,R5),WORK                                                     
         LA    R5,9(R5)                                                         
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'    GET UNTIME ADDRESS                 
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),BDTIMST,0(R5)                                          
         LA    R5,12(R5)                                                        
*                                                                               
         MVC   0(1,R5),BDDAYPT                                                  
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         EDIT  (R0),(3,1(R5)),ALIGN=LEFT                                        
         LA    R5,4(R5)                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'          SHOW DOLLARS ONLY                            
         LR    R0,R1                                                            
         EDIT  (R0),(7,(R5)),0,ALIGN=LEFT,FLOAT=$                               
         LA    R5,8(R5)                                                         
         MVC   0(16,R5),BDPROGRM                                                
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A0D'    GET SQUASHER ADDRESS               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),DSPAREA,80                                             
*                                                                               
         MVC   LINDATA,DSPAREA                                                  
*                                                                               
* NOW DISPLAY SCHEDULE DATA                                                     
*                                                                               
         LA    R5,FROMDATA                                                      
         MVC   FROMSTP,SVSTARTP                                                 
         MVC   FROMENDP,SVENDP                                                  
         BRAS  RE,BLDSKED          BUILD SCHEDULE DATA                          
*                                                                               
         CLI   SVDSPMOD,C'C'       TEST CHECK MODE                              
         BE    DSP62                                                            
         EJECT                                                                  
*===================================================================            
* NOW ADD DISPLAY LINES FOR EACH CLT/PRD/EST AND READ BUYS TO SEE               
* IF THEY HAVE MOVED TO THIS CLT/PRD/EST FROM THIS LINE BEFORE                  
* R4 POINTS TO NEXT SVMVBUY ENTRY                                               
*===================================================================            
         SPACE 1                                                                
DSP40    MVC   DUB(4),AREC1        MOVE FROM                                    
         MVC   DUB+4(4),AREC3      MOVE TO                                      
         GOTO1 MOVEREC                                                          
         MVC   AREC,AREC3          SET ADDRESS OF FROM BUY                      
*                                                                               
         LA    R7,SVMVLIST                                                      
         USING MVLISTD,R7                                                       
*                                                                               
         SR    R8,R8                                                            
         IC    R8,SVMVNUM          NUMBER OF MOVE TO CLIENTS                    
*                                                                               
DSP50    LA    R2,LINNEXT          POINT TO NEXT DISPLAY LINE                   
         SR    R0,R0                                                            
         IC    R0,LINENUM                                                       
         AHI   R0,1                                                             
         STC   R0,LINENUM                                                       
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(3),MVQCLT                                                    
         MVC   DUB+3(3),MVQPRD                                                  
         MVC   DUB+6(1),MVBEST                                                  
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'92'        'MOVE FROM' ELEMENTS                         
         MVI   ELCDHI,X'92'                                                     
         MVI   BUYKEY,0            SET NO RECORD PRESENT                        
         MVI   0(R4),C'T'          SET 'TO' LINE                                
*                                                                               
         USING MOVELEM,R6                                                       
*                                                                               
DSP52    BRAS  RE,NEXTEL                                                        
         BNE   DSP56                                                            
         CLC   DUB(7),MOVQCLT      MATCH CLT/PRD/EST                            
         BNE   DSP52                                                            
* PREVIOUSLY MOVED - READ THE OLD RECORD                                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+1(2),MVBCLT                                                  
         MVC   KEY+9(1),MOVBEST                                                 
         MVC   KEY+11(1),MOVLIN                                                 
         MVC   1(1,R4),MOVLIN      SAVE LINE NUMBER IN LIST                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   2(4,R4),=X'FFFFFFFF'  SET MISSING FLAG                           
         CLC   KEY(12),KEYSAVE                                                  
         BNE   DSP56                                                            
*                                                                               
         MVC   2(4,R4),KEY+14      ELSE SAVE DISK ADDRESS IN LIST               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
DSP56    NI    LINHDRAH+1,X'FF'-X'08'    SET NORM INT                           
         OI    LINHDRAH+6,X'80'          XMT                                    
         NI    LINHDRBH+1,X'FF'-X'08'    SET NORMAL INT                         
         NI    LINHDRBH+1,X'FF'-X'20'    SET UNPROT                             
         OI    LINHDRBH+6,X'80'          XMT                                    
*                                                                               
         MVC   LINLIN,=C'==>'                                                   
         LA    R1,LINCPE                                                        
         MVC   0(3,R1),MVQCLT                                                   
         BAS   RE,SETIT                                                         
         MVC   0(3,R1),MVQPRD                                                   
         BAS   RE,SETIT                                                         
         SR    R0,R0                                                            
         IC    R0,MVBEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         LA    R1,4(R1)                                                         
*                                                                               
         CLI   0(R6),X'92'                                                      
         BNE   DSP60                                                            
         BAS   RE,SETIT                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MOVLIN                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BCTR  R1,0                BACK UP TO /                                 
         MVI   0(R1),C'-'                                                       
         UNPK  1(3,R1),DUB                                                      
         LA    R1,5(R1)                                                         
         CLC   2(4,R4),=X'FFFFFFFF'  TEST LINE MISSING                          
         BNE   *+14                                                             
         MVC   0(3,R1),=C'!!!'                                                  
         LA    R1,4(R1)                                                         
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DIFFDAYS)                                              
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                                                             
         CLM   R0,1,FRSTDAY        RECS MUST HAVE SAME START DAY                
         BNE   DSP56ERR                                                         
         MVC   NERRCD,=Y(DIFFTIME)                                              
         CLC   STTIME(4),BDTIMST                                                
         BNE   DSP56ERR                                                         
         B     DSP60                                                            
*                                                                               
DSP56ERR MVC   4(3,R1),=C':-('                                                  
         LA    R4,LINHDRAH                                                      
         ST    R4,FADDR                                                         
         B     DSPERR                                                           
*                                                                               
DSP60    BAS   RE,DSPDEM           ON RETURN, FULL HAS TARGET VALUE             
         L     R0,FULL                                                          
         EDIT  (R0),(6,LINDEM),1                                                
*                                                                               
         LA    R5,TODATA                                                        
         USING TODATAD,R5                                                       
*                                                                               
         MVC   TOSTP,MVSTARTP      SET ESTIMATE DATES                           
         MVC   TOENDP,MVENDP                                                    
         BRAS  RE,BLDSKED          DISPLAY SCHEDULE (OR DOTS)                   
*                                                                               
         LA    R4,8(R4)            ADVANCE LIST ENTRY POINTER                   
         AHI   R7,L'MVDATA                                                      
         BCT   R8,DSP50                                                         
         DROP  R6,R7                                                            
         SPACE 1                                                                
*=================================================================              
* ALL DATA FOR ONE BUY HAS BEEN MOVED                                           
* SEE IF THERE IS ROOM FOR ANOTHER SET OF BUYS ON THE SCREEN                    
*=================================================================              
         SPACE 1                                                                
DSP62    SR    R0,R0                                                            
         IC    R0,LINENUM          GET CURRENT LINE NUMBER                      
         SR    R1,R1                                                            
         IC    R1,SVMVNUM          NUMBER OF 'MOVE TO' CLIENTS                  
         AHI   R1,1                +1 FOR MOVE FROM CLIENT                      
         AR    R0,R1                                                            
         CHI   R0,22               WILL ANOTHER LINE FIT ?                      
         BH    DSPX                NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                REREAD LAST 'MOVE FROM' LINE                 
         GOTO1 SEQ                                                              
*                                                                               
         LA    R2,LINNEXT          POINT TO NEXT DISPLAY LINE                   
         SR    R0,R0                                                            
         IC    R0,LINENUM                                                       
         AHI   R0,1                                                             
         STC   R0,LINENUM                                                       
         B     DSP34                                                            
*                                                                               
DSPX     CLI   LINENUM,13          TEST ANY MORE LINES DISPLAYED                
         BNE   DSPX2                                                            
         BRAS  RE,NOMORE                                                        
         MVC   BUYMSG(40),2(R1)                                                 
         OI    MOVINP1H+6,X'40'                                                 
         J     EQXIT                                                            
*                                                                               
DSPX2    BRAS  RE,DSPLYD                                                        
         CLI   SVDSPMOD,C'C'       TEST MODE=CHECK                              
         BNE   *+8                                                              
         BRAS  RE,CHKDSPLY         SET A BETTER MESSAGE                         
         MVC   BUYMSG(40),2(R1)                                                 
         OI    MOVDS2BH+6,X'40'    CURSOR TO FIRST UNP SKED FIELD               
         J     EQXIT                                                            
*                                                                               
SETIT    LA    R1,3(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         LA    R1,2(R1)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*===========================================================                    
* FIND TARGET DEMO VALUE IN BUYLINE IN AREC IF IT EXISTS                        
*   OR AREC3 IF IT DOES NOT                                                     
* AND RETURN IN FULL                                                            
*===========================================================                    
         SPACE 1                                                                
         USING MVLISTD,R7                                                       
DSPDEM   NTR1                                                                   
         L     R6,AREC                                                          
         AHI   R6,BDELEM-BUYREC                                                 
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         JNP   EXIT                                                             
         SRL   R0,3                                                             
         LA    R6,24(R6)                                                        
*                                                                               
DSPDEM2  CLC   MVDEMS(3),0(R6)     MATCH DEMO CODE                              
         BE    DSPDEM4                                                          
         LA    R6,8(R6)                                                         
         BCT   R0,DSPDEM2                                                       
         B     DSPDEMX                                                          
*                                                                               
DSPDEM4  SR    R0,R0                                                            
         IC    R0,3(R6)            GET SVI VALUE                                
         AR    R0,R0               SVI X 2                                      
         L     R1,4(R6)            GET DEMO VALUE                               
         N     R1,=X'7FFFFFFF'     DROP OVRD FLAG                               
         MR    R0,R0                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,FULL                                                          
*                                                                               
DSPDEMX  J     EXIT                                                             
         DROP  R7                                                               
         SPACE 1                                                                
*===========================================================                    
* ERROR DURING DISPLAY                                                          
*===========================================================                    
         SPACE 1                                                                
DSPERR   L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2               A(FLDHDR)                                    
         SR    R0,R3               R3 = A(TWA)                                  
         STH   R0,TIOBCURD                                                      
         SR    RE,RE               POINT TO START OF FIELD                      
         STC   RE,TIOBCURI                                                      
         NI    MOVINP1H+4,X'DF'    SET UNVALIDATED                              
         GOTO1 ERROR                                                            
         EJECT                                                                  
*======================================================================         
* VALIDATES THE INPUT LINE                                                      
*                                                                               
* INPUT STRING LOOKS LIKE 1MOVE,CL1/PR1/ES1,CL2/PR2/ES2                         
*                                                                               
*======================================================================         
         SPACE 1                                                                
VALIN    NTR1  BASE=*,LABEL=*                                                   
         MVI   SVDSPMOD,0          CLEAR DISPLAY MODE                           
         MVI   SVMVNUM,0           CLEAR MOVE TO NUM                            
*                                                                               
         TWAXC MOVHL2BH,MOVPFKH-1,PROT=Y CLEAR LINES BELOW INPUT LINE           
         LA    R2,MOVLAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
*                                                                               
         LA    R2,MOVDS1AH         FIRST DISPLAY LINE                           
         USING LINED,R2                                                         
*                                                                               
VLN2     OI    LINHDRBH+1,X'20'       PROTECT SKED AREA                         
         NI    LINHDRBH+1,X'FF'-X'08' NORMAL INT                                
         OI    LINHDRAH+6,X'80'       FORCE XMT                                 
         OI    LINHDRBH+6,X'80'       FORCE XMT                                 
         LA    R2,LINNEXT                                                       
         LA    R0,MOVPFKH                                                       
         CR    R2,R0                                                            
         BL    VLN2                                                             
*                                                                               
         LA    R0,SVMVDATA         CLEAR SAVED DATA                             
         LHI   R1,SVMVDATX-SVMVDATA                                             
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,SVMVLIST                                                      
         USING MVLISTD,R6                                                       
         MVC   SVMVID,=C'*SVMVDTA'                                              
*                                                                               
         LA    R2,MOVINP1H         POINT TO INPUT LINE                          
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   FSTOP,0                                                          
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'M'         READ UP TO THE WORD MOVE                     
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'M'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    VLNERR1                                                          
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    VLNERR1                                                          
         CHI   R0,255                                                           
         BH    VLNERR1                                                          
         STC   R0,SVLIN                                                         
*                                                                               
         AR    R4,R5               POINT TO 'M'                                 
         ST    R4,FADDR            AND SET INPUT POINTER                        
         XC    FLEN,FLEN                                                        
*                                                                               
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL              THIS READS WORD 'MOVE'                       
         CLC   =C'MOVE',0(R4)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   4(R4),C','          COMMA MUST FOLLOW                            
         BNE   VLNERRF                                                          
*                                                                               
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     STARTING LINE FOUND                          
         BNE   VLNERR2             ERR0=NO BUYLINES TO MOVE                     
         MVC   SVKEY(13),KEY       SAVE STARTING LINE NUMBER                    
* MAKE SURE THIS IS NOT A 'TO' BUYLINE                                          
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
* NOTE BELOW -- CAN'T USE NEXTEL BECAUSE R6 POINTS TO MVLIST                    
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOMOVEFR)                                              
         LA    RE,BDELEM                                                        
         SR    RF,RF                                                            
*                                                                               
VLN8     IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    VLN10                                                            
         CLI   0(RE),X'93'         CANNOT BE A MOVED FROM LINE                  
         BNE   VLN8                                                             
         GOTO1 ERROR                                                            
*                                                                               
VLN10    CLI   5(R4),C'*'          BUILD MOVE LIST ?                            
         BNE   VLN12                                                            
         BAS   RE,VLNBLD                                                        
*                                                                               
VLN12    CLC   =C'CHECK',5(R4)                                                  
         BE    VLN14                                                            
         CLC   =C'CHK',5(R4)                                                    
         BE    VLN14                                                            
         B     VLN16                                                            
*                                                                               
VLN14    MVI   SVDSPMOD,C'C'       SET MODE=CHECK                               
         J     EXIT                                                             
*                                                                               
VLN16    XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'/'         READ CLT/PRD/EST                             
         GOTO1 FLDVAL                                                           
*                                                                               
         CLI   FSTOP,C'/'                                                       
         BNE   VLNERR3             ERR3=INVALID CLT                             
         CHI   R5,2                                                             
         BL    VLNERR3                                                          
         CHI   R5,3                                                             
         BH    VLNERR3                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   MVQCLT(0),0(R4)                                                  
         OC    MVQCLT,=C'   '                                                   
*                                                                               
         XC    DMCB(8),DMCB        GET A(CLPACK)                                
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),MVQCLT,MVBCLT                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY                                                   
         MVC   KEY+2(2),MVBCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VLNERR4             ERR4=CLT NOT FOUND                           
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC              READ CLTHDR                                  
*                                                                               
         GOTO1 FLDVAL              GET PRODUCT CODE                             
*                                                                               
         CLI   FSTOP,C'/'                                                       
         BNE   VLNERR5             ERR5=INVALID PRODUCT                         
         CHI   R5,2                                                             
         BL    VLNERR5                                                          
         CHI   R5,3                                                             
         BH    VLNERR5                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   MVQPRD(0),0(R4) *EXECUTED*                                       
         OC    MVQPRD,=C'   '                                                   
*                                                                               
         L     RE,AREC                                                          
         USING CLTHDRD,RE                                                       
         LA    R1,CLIST                                                         
         DROP  RE                                                               
VLN20    CLC   MVQPRD,0(R1)                                                     
         BE    VLN22                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   VLN20                                                            
         B     VLNERR6             ERR6=PRD NOT FOUND                           
*                                                                               
VLN22    MVC   MVBPRD,3(R1)        MOVE BINARY PRD TO MOVLIST                   
*                                                                               
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL              GET ESTIMATE NUMBER                          
         LTR   R5,R5                                                            
         BZ    VLNERR7             ERR7=EST NOT VALID                           
         CHI   R5,3                                                             
         BH    VLNERR7                                                          
         TM    FVAL,X'08'                                                       
         BZ    VLNERR7                                                          
         CVB   R0,DUB                                                           
         STC   R0,MVBEST           SAVE EST IN MOVELIST                         
*                                                                               
         CLC   SVCLT,MVBCLT        SAME CLIENT                                  
         BNE   *+14                                                             
         CLC   SVEST,MVBEST        SAME ESTIMATE                                
         BE    VLNERRB                                                          
*                                                                               
         MVC   KEY+4(3),MVQPRD     MOVE EBCDIC PRD TO KEY                       
         MVC   KEY+7(1),MVBEST                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VLNERR8             ERR8=EST NOT FOUND                           
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AREC                                                          
         USING ESTHDRD,RE                                                       
         MVC   MVDEMS,EDEMOS       MOVE 14 DEMOS TO MOVLIST                     
*                                                                               
         CLC   ESTART,SVEND        MUST MATCH ESTIMATE DATES                    
         BH    VLNERRA                                                          
         CLC   EEND,SVSTART                                                     
         BL    VLNERRA                                                          
         MVC   WORK(12),ESTART                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,MVSTARTP)                                   
         GOTO1 (RF),(R1),WORK+6,(2,MVENDP)                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVMVNUM                                                       
         AHI   R0,1                                                             
         STC   R0,SVMVNUM                                                       
*                                                                               
         LA    R6,MVNEXT           NEXT MOVLIST ENTRY                           
         CLI   FSTOP,C','          IS THERE MORE DATA                           
         BNE   VLN30               NO                                           
         CHI   R0,6                TEST REACHED MAX                             
         BL    VLN12                                                            
         B     VLNERR9             ERR9=TOO MANY MOVE TO CODES                  
*                                                                               
VLN30    J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
VLNERR1  MVC   NERRCD,=Y(BADMVLIN) INVALID LINE NUMBER                          
         B     VLNERRX                                                          
VLNERR2  MVC   NERRCD,=Y(NOMVBUYS) NO BUYS TO MOVE                              
         B     VLNERRX                                                          
VLNERR3  MVC   NERRCD,=Y(BADMVCLT) INVALID CLIENT                               
         B     VLNERRX                                                          
VLNERR4  MVC   NERRCD,=Y(MVCLTNF)  CLIENT NOT FOUND                             
         B     VLNERRX                                                          
VLNERR5  MVC   NERRCD,=Y(BADMVPRD) INVALID PRODUCT                              
         B     VLNERRX                                                          
VLNERR6  MVC   NERRCD,=Y(MVPRDNF)  PRODUCT NOT FOUND                            
         B     VLNERRX                                                          
VLNERR7  MVC   NERRCD,=Y(BADMVEST) INVALID ESTIMATE                             
         B     VLNERRX                                                          
VLNERR8  MVC   NERRCD,=Y(MVESTNF)  ESTIMATE NOT FOUND                           
         B     VLNERRX                                                          
VLNERR9  MVC   NERRCD,=Y(MVLSTMAX) MAX 6 MOVELIST ENTRIES                       
         B     VLNERRX                                                          
VLNERRA  MVC   NERRCD,=Y(MVDTERR)  TO/FROM EST DATES MUST OVERLAP               
         B     VLNERRX                                                          
VLNERRB  MVC   NERRCD,=Y(MVCPEERR) TO/FROM CLT/EST MUST BE DIFF                 
         B     VLNERRX                                                          
VLNERRF  MVC   NERRCD,=Y(BADMVFMT) INPUT FORMAT IS MOVE,CL/PR/ES,...            
         B     VLNERRX                                                          
*                                                                               
VLNERRX  MVI   ERRCD,NEWERRS                                                    
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2               DISPLACEMENT TO FIELD IN TWA                 
         SR    R0,R3               R3 = A(TWA)                                  
         STH   R0,TIOBCURD                                                      
*                                                                               
         LA    R0,8(R2)            START OF FIELD                               
         SR    R4,R0               GIVES DSPL TO FIELD IN ERROR                 
         STC   R4,TIOBCURI         POINT TO THE PLACE IN FIELD                  
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* BUILD INPUT STRING FROM '92' ELEMS IN FIRST BUYLINE                           
*===========================================================                    
         SPACE 1                                                                
VLNBLD   NTR1                                                                   
         LA    R4,5(R4)            WRITE OVER THE *                             
         MVI   ELCDLO,X'92'                                                     
         MVI   ELCDHI,X'92'                                                     
         LA    R6,BDELEM                                                        
         LA    R7,6                MAX 6 !                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   VLNBLDX                                                          
*                                                                               
         USING MOVELEM,R6                                                       
VLNBLD2  MVC   0(3,R4),MOVQCLT                                                  
         BAS   RE,VLNUPDT                                                       
         MVC   0(3,R4),MOVQPRD                                                  
         BAS   RE,VLNUPDT                                                       
         SR    R0,R0                                                            
         IC    R0,MOVBEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,3(R4)                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   VLNBLDX                                                          
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         BCT   R7,VLNBLD2                                                       
*                                                                               
VLNBLDX  LA    R0,8(R2)            NEED TO UPDATE FIELD LEN                     
         SR    R4,R0                                                            
         STC   R4,5(R2)                                                         
         XIT1                                                                   
*                                                                               
VLNUPDT  LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
*==============================================================                 
* BUILD WEEKLY SCHEDULE AND MOVE TO DISPLAY LINE                                
* R2 IS POINTING AT DISPLAY LINE HEADER                                         
* R5 POINTS AT TODATA ENTRY                                                     
* AND BUYREC IS IN REC                                                          
*==============================================================                 
         SPACE 1                                                                
         USING TODATAD,R5                                                       
*                                                                               
BLDSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,SVMVDTS          LIST OF 2-BYTE DATES                         
         LA    R4,LINSKED                                                       
         XC    LINSKED,LINSKED                                                  
*                                                                               
BKDSK12  MVI   2(R4),C'X'                                                       
         CLI   SVDSPMOD,C'C'       TEST CHECK MODE                              
         BE    BLDSK13                                                          
*                                                                               
         CLC   0(2,R1),TOSTP       TEST WEEK IN 'TO' EST PER                    
         BL    BKDSK14                                                          
         CLC   0(2,R1),TOENDP                                                   
         BH    BKDSK14                                                          
*                                                                               
BLDSK13  MVI   2(R4),C'.'                                                       
*                                                                               
BKDSK14  LA    R1,2(R1)                                                         
         LA    R4,3(R4)                                                         
         OC    0(2,R1),0(R1)                                                    
         BNZ   BKDSK12                                                          
*                                                                               
         CLI   BUYKEY,0            TEST BUY RECORD PRESENT                      
         BE    BLDSKX              NO                                           
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,LINSKED                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
BKDSK20  CLC   0(2,R1),TOSTP       TEST WEEK PRIOR TO EST START                 
         BL    BKDSK26                                                          
         CLC   0(2,R1),TOENDP      OR AFTER EST END                             
         BH    BKDSK26                                                          
*                                                                               
BKDSK22  BRAS  RE,NEXTEL                                                        
         BNE   BKDSK24                                                          
*                                                                               
         CLC   2(2,R6),0(R1)       BEFORE WEEK START DATE                       
         BL    BKDSK22                                                          
         CLC   2(2,R6),SVMVEND     TEST AFTER PERIOD END                        
         BNL   BKDSK24                                                          
         OC    2(2,R1),2(R1)       TEST LAST WEEK                               
         BZ    *+14                                                             
         CLC   2(2,R6),2(R1)       AFTER WEEK END                               
         BNL   BKDSK24                                                          
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   BKDSK22                                                          
         BCT   R7,BKDSK22          BUMP COUNTER AND CONTINUE                    
*                                                                               
BKDSK24  TM    LINHDRBH+1,X'20'    TEST PROTECTED                               
         BO    *+10                YES -DISPLAY 0                               
         LPR   R7,R7                                                            
         BZ    BKDSK26                                                          
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         CLI   1(R4),C'0'                                                       
         BNE   *+8                                                              
         MVI   1(R4),C' '                                                       
*                                                                               
BKDSK26  LA    R4,3(R4)            NEXT OUTPUT POSN                             
         LA    R1,2(R1)            NEXT WEEK IN LIST                            
         OC    0(2,R1),0(R1)                                                    
         BZ    BLDSKX                                                           
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         B     BKDSK20                                                          
*                                                                               
BLDSKX   OC    LINSKED,SPACES                                                   
         OI    LINHDRBH+1,X'08'    HIGH INT                                     
         OI    LINHDRBH+4,X'20'    SET PREVIOUSLY VALIDATED                     
         OI    LINHDRBH+6,X'80'    XMT                                          
         J     EXIT                                                             
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* NEWBUY - CREATE NEW BUY FROM RECORD SAVED IN AREC3                            
* ON ENTRY R5 POINTS TO TODATA ENTRY                                            
*===============================================================                
         SPACE                                                                  
         USING TODATAD,R5                                                       
*                                                                               
NEWBUY   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,REC                                                           
         L     RE,AREC3                                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         GET FROM LEN                                 
         LA    R1,2(RF)            TO LEN IS +2                                 
         MVCL  R0,RE                                                            
* UPDATE CLT/PRD/EST IN (NEW) RECORD                                            
         MVC   BUYKCLT,TOCLT                                                    
         MVI   BDMASPRD,0          CLEAR MASPRD                                 
         CLI   TOPRD,X'FF'         DO NOT SET BDMASPRD=X'FF'                    
         BE    *+10                                                             
         MVC   BDMASPRD(1),TOPRD                                                
         MVC   BUYKEST,TOEST                                                    
*                                                                               
         BRAS  RE,NXTLINE          SET NEXT AVAILABLE LINENUM IN REC            
*                                                                               
* NOW REMOVE ALL 0B/0C AND ASSOCIATED ELEMENTS                                  
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'1F'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   NEWB12                                                           
*                                                                               
NEWB10   GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         BRAS  RE,NEXTEL2                                                       
         BE    NEWB10                                                           
*                                                                               
NEWB12   MVI   ELCDLO,X'92'        DELETE ACTIVITY ELEMENTS                     
         MVI   ELCDHI,X'99'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   NEWB16                                                           
*                                                                               
NEWB14   GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         BRAS  RE,NEXTEL2                                                       
         BE    NEWB14                                                           
*                                                                               
NEWB16   L     R4,AREC5            SAVE 'FROM' DEMOS FOR BLDDEM CALL            
         MVC   0(63,R4),SVDEMLST                                                
*                                                                               
         L     RE,TOMVLIST         POINT TO MVLIST ENTRY                        
         USING MVLISTD,RE                                                       
         MVC   SVDEMLST(42),MVDEMS SET THE DEMO CODES TO BE USED                
         DROP  RE                                                               
*                                                                               
         GOTO1 VBLDDEM                                                          
         MVC   SVDEMLST,0(R4)      RESTORE 'FROM' DEMOS                         
*                                                                               
         XC    ELEM,ELEM                                                        
M        USING MOVELEM,ELEM                                                     
*                                                                               
         MVI   M.MOVELEM,MOVELTOQ  MOVE 'TO' ELEMENT                            
         MVI   M.MOVELLEN,16                                                    
         MVC   M.MOVQCLT,QCLT      SET 'FROM' CLIENT                            
         MVC   M.MOVQPRD,=C'POL'                                                
         MVC   M.MOVBEST,FROMEST                                                
         MVC   M.MOVLIN,FROMLINE                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,M.MOVDATE)                                 
         BRAS  RE,GETTIME                                                       
         MVC   M.MOVTIME,DUB                                                    
         MVC   M.MOVWHO,SVPASSWD                                                
*                                                                               
         LA    R6,REC                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,REC,ELEM,(R6)                                        
*                                                                               
         J     EQXIT                                                            
         DROP  M                                                                
         SPACE 2                                                                
GETTIME  NTR1                                                                   
         THMS  DDSTIME=YES                                                      
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DUB            CURRENT TIME PWOS                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SETNPW - DETEREMINE THE MOST FREQUENT NUMBER OF SPOTS/WEEK                    
* FOR 14 NPW ENTRIES IN SKED                                                    
*===============================================================                
         SPACE                                                                  
         USING TODATAD,R5                                                       
SETNPW   NTR1  BASE=*,LABEL=*                                                   
         OC    SKED,SKED           TEST ANY DATA                                
         BZ    SNPX                                                             
* MAKE 2 BYTE ENTRIES IN ELEM SO CAN USE QSORT                                  
         LA    R1,ELEM                                                          
         LA    RE,SKED                                                          
         LHI   RF,16                                                            
*                                                                               
SNP2     SR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         STH   R0,0(R1)                                                         
         LA    R1,2(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,SNP2                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 VCALLOV,DMCB,0                                                   
         ICM   RF,15,0(R1)                                                      
         BP    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(X'80',ELEM),14,2,2,0 SORT DESCENDING                  
*                                                                               
         LA    R1,ELEM                                                          
         LHI   R0,1                INITIALIZE COUNTER                           
         MVI   SKDHICNT,0          NUM TIMES SKDHINPW OCCURS                    
         MVI   SKDHINPW,0          MOST FREQUENT NPW                            
*                                                                               
SNP10    CLC   0(2,R1),2(R1)       THIS COUNT MATCH NEXT COUNT                  
         BNE   SNP12               NO                                           
         LA    R1,2(R1)                                                         
         AHI   R0,1                                                             
         B     SNP10                                                            
*                                                                               
SNP12    CLM   R0,1,SKDHICNT       NEW COUNTER TO HIGH SO FAR                   
         BNH   SNP14               NOT MORE                                     
*                                                                               
         LTR   R0,R0               DO NOT ALLOW 0 NPW TO WIN !                  
         BZ    SNP14                                                            
         MVC   SKDHINPW,1(R1)      SAVE THE WINNING NPW                         
         STC   R0,SKDHICNT         AND HOW MANY TIMES IT OCCURRED               
*                                                                               
SNP14    LHI   R0,1                INITIALIZE COUNTER                           
         LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)                                                    
         BNZ   SNP10                                                            
*                                                                               
SNPX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SKEDBUY - SKED CONTAINS INCREMENTAL SPOTS/WEEK                                
* FOR BUYREC IN AREC1                                                           
* R5 POINTS TO TODATA ENTRY                                                     
*===============================================================                
         SPACE 1                                                                
         USING TODATAD,R5                                                       
SKEDBUY  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2         CLEAR TEMP DATE TABLE                        
         MVC   WORK2(30),SVMVDTS                                                
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                GET BUY DAY NUMBER                           
         CLM   R0,1,SVMVSDAY       TEST SAME AS EST START                       
         BE    SKB10                                                            
* BUILD TABLE OF DATES ON CORRECT DAY IN WORK2                                  
         SR    R1,R1                                                            
         IC    R1,SVMVSDAY                                                      
         SR    R0,R1                                                            
         BNM   *+8                                                              
         AHI   R0,7                GIVES NUMBER OF DAYS TO ADVANCE              
*                                                                               
         LA    R6,SVMVDTS                                                       
         LA    R7,WORK2                                                         
*                                                                               
SKB2     GOTO1 VDATCON,DMCB,(2,(R6)),WORK  GET YYMMDD                           
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,(R7))                                     
         LA    R6,2(R6)                                                         
         LA    R7,2(R7)                                                         
         CLI   0(R6),0             TEST EOL                                     
         BNE   SKB2                                                             
*                                                                               
SKB10    LA    R7,WORK2                                                         
         LA    R2,SKED                                                          
*                                                                               
SKB12    LA    R6,BDELEM                                                        
*                                                                               
SKB14    CLI   0(R2),0             TEST NPW CHANGES                             
         BE    SKB50               NO                                           
*                                                                               
         MVI   ELCDLO,X'0B'        FIND FIRST SPOT THIS WEEK                    
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
SKB16    BRAS  RE,NEXTEL                                                        
         BNE   SKB20                                                            
         CLC   2(2,R6),0(R7)                                                    
         BL    SKB16                                                            
*                                                                               
SKB20    SR    R0,R0                                                            
         ICM   R0,1,0(R2)          GET NPW TO ADD TO THIS WEEK                  
         BP    SKB30                                                            
         BM    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* TAKE SPOTS OUT OF THIS WEEK                                                   
*                                                                               
         ICM   R0,8,0(R2)          GET COUNT IN LEFTMOST BYTE                   
         SRA   R0,24                                                            
         LPR   R0,R0               AND SET AS LOOP COUNTER                      
*                                                                               
SKB24    CLI   0(R6),0                                                          
         BE    SKB50                                                            
         CLC   2(2,R6),0(R7)       RIGHT DATE ?                                 
         BNE   SKB50               NO                                           
*                                                                               
SKB26    GOTO1 VRECUP,DMCB,(C'S',AREC1),(R6)  DELETE THIS ELEM                  
         AHI   R0,-1                                                            
         BZ    SKB50                                                            
         CLI   0(R6),0                                                          
         BE    SKB50                                                            
         CLI   0(R6),X'0B'                                                      
         BE    SKB24                                                            
         CLI   0(R6),X'0C'                                                      
         BE    SKB24                                                            
         CLI   0(R6),X'1F'                                                      
         BNH   SKB26                                                            
         B     SKB50                                                            
*                                                                               
* ADD SPOTS TO THIS WEEK IF IT'S IN THE BUY DESC PERIOD                         
*                                                                               
SKB30    GOTO1 VDATCON,DMCB,(2,0(R7)),(3,FULL)                                  
         CLC   FULL(3),BDSTART                                                  
         JL    NEQXIT                                                           
         CLC   FULL(3),BDEND                                                    
         JH    NEQXIT                                                           
*                                                                               
         CLI   0(R6),0             TEST POINTING AT EOR                         
         BNE   SKB44                                                            
* FIND INSERTION POINT                                                          
         LA    R6,BDELEM                                                        
         SR    RE,RE                                                            
SKB40    SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),0                                                          
         BE    SKB44                                                            
         CLI   0(R6),X'0B'                                                      
         BL    SKB40                                                            
         CLI   0(R6),X'0C'                                                      
         BH    SKB44                                                            
         CLC   0(2,R7),2(R6)       NEW DATE TO ELEM DATE                        
         BH    SKB40                                                            
*                                                                               
SKB42    IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),0                                                          
         BE    SKB44                                                            
         CLI   0(R6),X'0B'                                                      
         BE    SKB44                                                            
         CLI   0(R6),X'0C'                                                      
         BE    SKB44                                                            
         CLI   0(R6),X'20'         ADD AFTER ASSOCIATED ELEMS                   
         BL    SKB42                                                            
*                                                                               
SKB44    XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,14                                                        
         CLI   TOPRD,X'FF'         TEST NO MASPRD                               
         BNE   *+8                                                              
         MVI   ELEM+1,10           THEN LEAVE ELEM UNALLOCATED                  
         MVC   ELEM+2(2),0(R7)                                                  
         MVC   ELEM+10(1),TOPRD                                                 
* POINT TO SAVED FROM LINE                                                      
         L     RE,AREC3                                                         
         CLI   ELEM+11,0                                                        
         BNE   *+10                                                             
         MVC   ELEM+11(1),BDSEC-BUYREC(RE)                                      
*                                                                               
SKB46    GOTO1 VRECUP,DMCB,(C'S',AREC),ELEM,(R6)                                
         BCT   R0,SKB46                                                         
*                                                                               
SKB50    LA    R7,2(R7)            NEXT WEEK ENTRY                              
         OC    0(2,R7),0(R7)                                                    
         JZ    EQXIT                                                            
         LA    R2,1(R2)            NEXT SKED ENTRY                              
         B     SKB12                                                            
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SETBDDTS - SET BDSTART/BDEND DATES                                            
* START WITH COPIED BUY DATES AND ADJUST TO MAKE SURE IN PERIOD                 
* R5 POINTS TO TODATA ENTRY                                                     
* BUYREC IS IN AREC1                                                            
*===============================================================                
         SPACE 1                                                                
         USING TODATAD,R5                                                       
SETBDDTS NTR1  BASE=*,LABEL=*                                                   
                                                                                
* COUNT WEEKS IN OLD BUY. CAN'T USE BDWKS BECAUSE IT ISN'T SET                  
* PROPERLY IF BDWKIND IS NOT C'O' (EVERY WEEK)                                  
                                                                                
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK   GET EBCDIC START                 
         GOTO1 (RF),(R1),(3,BDEND),WORK+12     GET EBCDIC END                   
         LHI   R0,1                INITIALIZE COUNTER                           
*                                                                               
SETBD1   GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),WORK+12     TEST PAST BUY END DATE                       
         BH    *+8                                                              
         BCT   R0,SETBD1                                                        
*                                                                               
         LPR   R0,R0                                                            
         STC   R0,BDWKS                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,TOSTP),(3,WORK)  GET YMD EST START               
         CLC   BDSTART(3),WORK          TEST START DATE IN PERIOD               
         BNL   SETBD10                  YES                                     
* ADVANCE START DATE TILL IN ESTIMATE PERIOD                                    
         GOTO1 VDATCON,DMCB,(2,TOSTP),WORK+12 GET YYMMDD EST START              
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK  GET YYMMDD BUY START              
*                                                                               
SETBD2   GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),WORK+12     TEST REACHED ESTIMATE START                  
         BL    SETBD2                                                           
         GOTO1 VDATCON,DMCB,WORK,(3,BDSTART)  SET NEW START DATE                
*                                                                               
SETBD10  SR    R0,R0                                                            
         IC    R0,BDWKS            NUMBER OF WEEKS IN OLD BUY                   
*                                                                               
         GOTO1 VDATCON,DMCB,(2,TOENDP),WORK+12 GET YYMMDD EST END               
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK   GET YYMMDD BUY START             
*                                                                               
SETBD12  GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         CLC   WORK+6(6),WORK+12               TEST PAST EST END                
         BH    SETBD20                                                          
         AHI   R0,-1                                                            
         BZ    SETBD20                                                          
         MVC   WORK(6),WORK+6                                                   
         B     SETBD12                                                          
*                                                                               
SETBD20  GOTO1 VDATCON,DMCB,WORK,(3,BDEND)     USE PREVIOUS DATE                
         SR    R1,R1                                                            
         IC    R1,BDWKS                                                         
         SR    R1,R0                                                            
         STC   R1,BDWKS            SET ACTUAL NUMBER OF WEEKS                   
         MVI   BDWKIND,C'O'        SET EVERY WEEK (UGH)                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* INSERT/UPDATE MOVE TO ELEMENTS IN FROM BUYLINE                                
*==============================================================                 
         SPACE 1                                                                
BLDFROM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,TODATA                                                        
         USING TODATAD,R5                                                       
*                                                                               
M        USING MOVELEM,ELEM                                                     
*                                                                               
BLDFR2   L     RE,TOMVLIST         POINT TO MOVELIST ENTRY                      
         USING MVLISTD,RE                                                       
*                                                                               
         CLI   TOLINE,0            TEST ANY LINE ACTUALLY THERE                 
         BE    BLDFR10                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   M.MOVELCOD,X'92'    MOVELFRQ                                     
         MVI   M.MOVELLEN,MOVLENQ                                               
         MVC   M.MOVQCLT,MVQCLT                                                 
         MVC   M.MOVQPRD,MVQPRD                                                 
         MVC   M.MOVBEST,MVBEST                                                 
         MVC   M.MOVLIN,TOLINE                                                  
         DROP  RE                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,M.MOVDATE)                                 
         BRAS  RE,GETTIME                                                       
         MVC   M.MOVTIME,DUB                                                    
         MVC   M.MOVWHO,SVPASSWD                                                
         DROP  M                                                                
*                                                                               
         MVI   ELCDLO,X'92'        LOOK FOR MATCH                               
         MVI   ELCDHI,X'92'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLDFR4   BRAS  RE,NEXTEL                                                        
         BNE   BLDFR6                                                           
         CLC   0(10,R6),ELEM       MATCH CD/LN/CLT/PRD/EST/LIN                  
         BNE   BLDFR4                                                           
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  DELETE EXISTING ELEMENT                 
*                                                                               
BLDFR6   LA    R6,REC                   ADD NEW ELEMENT AT END                  
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,REC,ELEM,(R6)                                        
*                                                                               
BLDFR10  LA    R5,L'TODATA(R5)                                                  
         OC    TOTWA,TOTWA         TEST ANOTHER ENTRY                           
         BNZ   BLDFR2                                                           
         XIT1                                                                   
         LTORG                                                                  
*===============================================================                
* GET NEXT LINE NUMBER                                                          
*===============================================================                
         SPACE 1                                                                
NXTLINE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),BUYKEY      A-M/CLT/PRD/MKT/STA/EST                      
         GOTO1 HIGH                                                             
*                                                                               
NXTL2    CLC   KEY(10),KEYSAVE     WHEN KEYS NOT EQUAL, DONE                    
         BNE   NXTL4                                                            
         MVC   KEYSAVE,KEY         SAVE CURRENT KEY                             
         GOTO1 SEQ                                                              
         B     NXTL2                                                            
*                                                                               
NXTL4    CLI   KEYSAVE+11,255      LINE NUMBER IN SPTDIR KEY                    
         BNE   NXTL10                                                           
         MVI   ERRCD,MAXLINES                                                   
         GOTO1 ERROR                                                            
*                                                                               
NXTL10   SR    R0,R0                                                            
         IC    R0,KEYSAVE+11                                                    
         AHI   R0,1                                                             
         STC   R0,BUYKEY+10        LINE NUMBER IN BUYREC                        
         MVI   BUYKEY+11,1                                                      
*                                                                               
NXTLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* CHECK ALL SPOTS ON FROM LINE ARE IN BUY DESC PERIOD                           
*===========================================================                    
         SPACE 1                                                                
CHKINPER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BDSTART),(2,FULL)                                
         GOTO1 (RF),(R1),(3,BDEND),(2,FULL+2)                                   
*                                                                               
         MVI   ELCDLO,X'0B'        CHECK REGELS ONLY                            
         MVC   ELCDHI,ELCDLO                                                    
         LA    R6,BDELEM                                                        
*                                                                               
CHKIN2   BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         CLC   2(2,R6),FULL        TEST PRIOR TO BDSTART                        
         BL    CHKINERR                                                         
         CLC   2(2,R6),FULL+2      TEST AFTER BDEND                             
         BH    CHKINERR                                                         
         B     CHKIN2                                                           
*                                                                               
CHKINERR MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(OUTOFBUY)                                              
         NI    MOVINP1H+4,X'DF'    SET INPUT LINE UNVALIDATED                   
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=============================================================                  
* DETERMINE WHICH INPUT LINE CURSOR IS POSITIONED IN AND                        
* SET GLOBALS FOR MIS CALL TO CLT/PRD/EST ON THIS INPUT LINE                    
*=============================================================                  
         SPACE 1                                                                
GOMIS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         SR    R7,R7                                                            
         ICM   R7,3,TIOBCURD                                                    
         AR    R7,R3               FIELD ADDRESS IN TWA                         
         DROP  R1                                                               
* IF ONLY ONE MOVE TO CLIENT, CURSOR POSITION DOESN'T MATTER                    
         LA    R4,SVMVLIST                                                      
         USING MVLISTD,R4                                                       
         LA    RE,MVNEXT           POINT TO SECOND ENTRY                        
         OC    0(3,RE),0(RE)       ANYTHING THERE                               
         BZ    GOMIS10             NO - USE FIRST ENTRY                         
*                                                                               
         LA    R2,MOVDS1AH         POINT TO FIRST DISPLAY LINE                  
         USING LINED,R2                                                         
*                                                                               
         CR    R2,R7                                                            
         BH    GOMISERR                                                         
*                                                                               
GOMIS2   LA    R0,LINNEXT                                                       
         CR    R7,R0               CURSOR SHOULD NOT BE IN FROM LINE            
         BL    GOMISERR                                                         
*                                                                               
         LA    R2,LINNEXT                                                       
         LA    R4,SVMVLIST         POINT TO LIST OF MOVE 'TO' DATA              
*                                                                               
GOMIS6   LA    R0,LINNEXT                                                       
         CR    R7,R0               IF LOW, CURSOR IS IN THIS LINE               
         BL    GOMIS10                                                          
         LA    R2,LINNEXT          NEXT INPUT LINE                              
         LA    R4,MVNEXT                                                        
         OC    MVQCLT,MVQCLT       TEST THERE IS ANOTHER ENTRY                  
         BNZ   GOMIS6                                                           
         B     GOMIS2                                                           
*                                                                               
GOMIS10  GOTO1 VGLOBBER,DMCB,=C'PUTD',MVQCLT,3,GLVSPCLT                         
         BAS   RE,GLBTEST                                                       
*                                                                               
         GOTO1 (RF),(R1),,MVQPRD,3,GLVSPPRD                                     
         BAS   RE,GLBTEST                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MVBEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         GOTO1 (RF),(R1),,WORK,3,GLVSPEST                                       
         BAS   RE,GLBTEST                                                       
*                                                                               
         MVC   WORK(8),=CL8'ALL     '  ALWAYS GET MARKET TOTALS                 
         GOTO1 (RF),(R1),,WORK,8,GLVSPSTA                                       
*                                                                               
         MVC   WORK(5),=C'GVP-W'       AND DO GVP-W                             
         GOTO1 (RF),(R1),,WORK,5,GLVSPFMT                                       
* DELETE DAYPART VALUE IF ANY                                                   
         GOTO1 (RF),(R1),=C'DELE',,,GLVSPDPT (IGNORE ERRORS)                    
*                                                                               
         CLI   SVDPTOPT,0          ANY DAYPART FILTER ACTIVE                    
         BE    GOMIS20             NO                                           
         GOTO1 (RF),(R1),=C'PUTD',SVDPTOPT,1,GLVSPDPT                           
         BAS   RE,GLBTEST                                                       
* BUILD TRANSFER CONTROL ELEMENT AND EXIT !                                     
GOMIS20  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'MIS'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         MVC   BUYMSG(22),=C'** BACK TO SPOT BUY **'                            
         J     EXIT                                                             
         DROP  R1                                                               
*                                                                               
GOMISERR MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADCRSR)                                               
         GOTO1 ERROR                                                            
*                                                                               
GLBTEST  CLI   DMCB+8,0                                                         
         BER   RE                                                               
         DC    H'0'                                                             
         LTORG                                                                  
         SPACE 2                                                                
* INCLUDE LOWERCASE MESSAGES                                                    
       ++INCLUDE SPBUY37MSG                                                     
         EJECT                                                                  
OVWORKD  DSECT                                                                  
SKED     DS    XL16                                                             
SKDHICNT DS    X                                                                
SKDHINPW DS    X                                                                
SKDCOUNT DS    X                                                                
FRSTDAY  DS    X                                                                
STTIME   DS    XL2                                                              
ENDTIME  DS    XL2                                                              
*                                                                               
         DS    0D                                                               
FROMDATA DS    0XL56                                                            
FROMTWA  DS    A                   FLDHDR ADDR OF FROM LINE                     
         DS    A                                                                
         DS    A                                                                
FROMCLT  DS    XL2                                                              
FROMPRD  DS    X                                                                
FROMEST  DS    X                                                                
FROMLINE DS    X                                                                
FROMSTP  DS    XL2                                                              
FROMENDP DS    XL2                 SPARE                                        
         DS    XL3                                                              
FROMDA   DS    XL4                                                              
FROLDSKD DS    XL14                                                             
FRNEWSKD DS    XL14                                                             
*                                                                               
         DS    0D                                                               
TODATA   DS    7XL56                                                            
TODATAX  EQU   *                                                                
*                                                                               
TODATAD  DSECT                                                                  
TOTWA    DS    A                   FLDHDR ADDR OF 'TO' LINE                     
TOMVLIST DS    A                   A(SVMVLIST ENTRY)                            
TOMVBUY  DS    A                   A(SVMVBUYS ENTRY)                            
TOCLT    DS    XL2                                                              
TOPRD    DS    X                                                                
TOEST    DS    X                                                                
TOLINE   DS    X                                                                
TOSTP    DS    XL2                                                              
TOENDP   DS    XL2                                                              
         DS    XL3                                                              
TODA     DS    XL4                                                              
TOOLDSKD DS    XL14                                                             
TONEWSKD DS    XL14                                                             
*                                                                               
MVLISTD  DSECT                                                                  
MVDATA   DS    0XL64                                                            
MVQCLT   DS    CL3                                                              
MVBCLT   DS    XL2                                                              
MVQPRD   DS    CL3                                                              
MVBPRD   DS    XL1                                                              
MVBEST   DS    XL1                                                              
MVSTARTP DS    XL2                 EST START - 2 BYTE                           
MVENDP   DS    XL2                 EST END - 2 BYTE                             
         DS    XL8                 SPARE                                        
MVDEMS   DS    XL42                                                             
MVNEXT   EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
         PRINT ON                                                               
         EJECT                                                                  
T211FFD  DSECT                                                                  
         ORG   BUYHL1H                                                          
       ++INCLUDE SPBUYF7D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOREQUS                                                      
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
LINED    DSECT                                                                  
*                                                                               
LINHDRAH DS    XL8                                                              
LINLIN   DS    CL3                                                              
         DS    CL1                                                              
LINDATA  DS    0CL32               NEW SQUASHED DATA                            
LINDAYS  DS    CL7                                                              
         DS    CL1                                                              
LINTIME  DS    CL11                                                             
         DS    CL1                                                              
LINPROG  DS    CL12                                                             
*                                                                               
         ORG   LINDAYS                                                          
LINCPE   DS    CL15                MAX CCC/PPP/EEE-LLL                          
         DS    CL1                                                              
LINDEM   DS    CL6                                                              
         ORG                                                                    
*                                                                               
LINHDRBH DS    XL8                                                              
LINSKED  DS    CL42                                                             
*                                                                               
LINNEXT  EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPBUY37   04/02/13'                                      
         END                                                                    
