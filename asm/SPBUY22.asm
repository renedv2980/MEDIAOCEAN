*          DATA SET SPBUY22    AT LEVEL 026 AS OF 11/13/19                      
*PHASE T21122C                                                                  
*INCLUDE SPXBHIST                                                               
         TITLE 'T21122 - SPOTPAK BUY - LINE DISPLAY III'                        
T21122   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21122,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21122+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO SR    RE,RE                                                            
         IC    RE,RCLOPT                                                        
         SLL   RE,25                                                            
         SRL   RE,23                                                            
         L     RF,RCLTAB(RE)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
* NO RETURN HERE VIA RE                                                         
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
* IF ANY UNPROTECTED FIELDS DISPLAYED, ADD TAB FIELD                            
*                                                                               
LDX      LA    R2,BUYOUTH                                                       
         BRAS  RE,FNDUF                                                         
         JNE   EXIT                                                             
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'01010000'                                          
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
RCLTAB   DS    0F                                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLROT),AL3(0)                                               
         DC    AL1(RCLPAY),AL3(0)                                               
         DC    AL1(RCLPAYDT),AL3(0)                                             
         DC    AL1(RCLINV),AL3(0)                                               
         DC    AL1(RCLREF),AL3(LD300)                                           
         DC    AL1(RCLCOM),AL3(LD400)                                           
         DC    AL1(RCLDEM),AL3(LD500)                                           
         DC    AL1(RCLACTV),AL3(LDACTV)                                         
         DC    AL1(RCLORB),AL3(LD700)                                           
         DC    AL1(RCLSPILL),AL3(LD750)                                         
         DC    AL1(RCLSTA),AL3(LD775)                                           
         DC    AL1(RCLHUT),AL3(0)                                               
         DC    AL1(RCLPCD),AL3(0)                                               
         DC    AL1(RCLINT),AL3(0)                                               
         DC    AL1(RCLINTX),AL3(0)                                              
         DC    AL1(RCLFLM),AL3(0)                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLSCH),AL3(0)                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLRSVP),AL3(0)                                              
         DC    AL1(RCLDEMS),AL3(LD550)                                          
         DC    AL1(RCLPDEM),AL3(LD580)                                          
         DC    AL1(RCLPDEMX),AL3(LD580)                                         
         DC    AL1(RCLXCH),AL3(0)                                               
         DC    AL1(RCLDSK),AL3(0)                                               
         DC    AL1(RCLNET),AL3(LDNET)                                           
         DC    AL1(RCLDT),AL3(0)                                                
         DC    AL1(RCLCUT),AL3(0)                                               
         DC    AL1(RCLCLST),AL3(LDNET)                                          
         DC    AL1(RCLNLST),AL3(LDNL)                                           
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLHIST),AL3(LD800)                                          
         DC    AL1(RCLLKIN),AL3(LD820)                                          
*                                                                               
LDERR    MVI   SVRCLOPT,0                                                       
         ICM   R2,15,ABUYINPH                                                   
         BNZ   *+8                                                              
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR                                                            
         EJECT                                                                  
* REFERENCES                                                                    
*                                                                               
LD300    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         XC    ELEM+256(256),ELEM+256   NNEED A LOT OF ROOM                     
         XC    BLDLIST,BLDLIST                                                  
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    LD302                                                            
         MVC   ELEM(27),=C'** NOT A REFERENCED LINE **'                         
         MVI   BLDLIST+1,27                                                     
         MVI   BLDLIST+2,X'20'     PROTECTED                                    
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
         BRAS  RE,GOBLDFLD                                                      
         B     LDX                                                              
*                                                                               
LD302    MVC   ELEM(3),=C'PKG'                                                  
         MVC   BYTE,2(R6)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,2                                                           
         BNH   LD304                                                            
         MVC   ELEM(3),=C'REV'                                                  
         CLI   BYTE,6                                                           
         BNH   LD304                                                            
         DC    H'0'                                                             
*                                                                               
LD304    MVC   BYTE,2(R6)          SAVE IND AGAIN                               
*                                                                               
         LA    R4,ELEM+4                                                        
         TM    2(R6),X'01'         TEST MASTER                                  
         BZ    *+14                                                             
         MVC   0(5,R4),=C'LINES'                                                
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'='                                                       
         LA    R4,2(R4)                                                         
* DISPLAY LINE NUMBERS                                                          
         ZIC   R0,1(R6)                                                         
         AHI   R0,-3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         TM    BYTE,X'10'          TEST 2-BYTE LINES                            
         BZ    *+8                                                              
         SRL   R0,1                                                             
         LA    R7,3(R6)                                                         
*                                                                               
LD306    LLC   RE,0(R7)                                                         
         TM    BYTE,X'10'                                                       
         BZ    *+8                                                              
         ICM   RE,3,0(R7)                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
         LA    R7,1(R7)                                                         
         TM    BYTE,X'10'                                                       
         BZ    *+8                                                              
         LA    R7,1(R7)                                                         
         BCT   R0,LD306                                                         
*                                                                               
LD307    MVC   BLDLIST(4),=X'004C2000'                                          
         LA    R6,ELEM                                                          
*                                                                               
LD308    ST    R6,BLDLIST+4                                                     
         BRAS  RE,GOBLDFLD                                                      
         LA    R6,76(R6)                                                        
         CR    R6,R4                                                            
         BL    LD308                                                            
         B     LDX                                                              
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
LD400    DS    0H                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'004C0000'                                          
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
         MVC   ELEM(80),SPACES                                                  
*                                                                               
         ZAP   HALF,=P'5'                                                       
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    LD402                                                            
         MVC   ELEM(17),=C'** NO COMMENTS **'                                   
         B     LD420                                                            
*                                                                               
LD402    ZIC   R0,2(R6)            COMMENT NUMBER                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM(1),DUB                                                      
         MVI   ELEM+1,C'-'                                                      
         ZIC   RE,1(R6)                                                         
         SHI   RE,4                GET DATA LEN - 1                             
         BM    LD420                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),3(R6) *EXECUTED*                                       
*                                                                               
LD420    BRAS  RE,GOBLDFLD                                                      
         MVC   ELEM(80),SPACES                                                  
         SP    HALF,=P'1'                                                       
         BNP   LDX                                                              
         CLI   0(R6),0                                                          
         BE    LD420                                                            
         BRAS  RE,NEXTEL                                                        
         BE    LD402               GO DISPLAY NEXT COMMENT                      
         B     LD420                                                            
         EJECT                                                                  
* DEMOGRAPHICS                                                                  
*                                                                               
LD500    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCDLO,X'24'        DEMO LOOKUP OVERRIDE ELEM                    
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD500A                                                           
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+64(0),2(R6)    SAVE BKTYPE/ALPHAMKT/STA OVRD                
*                                                                               
LD500A   MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'00192001'                                          
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
*                                                                               
         LR    R7,R6               SAVE 02 EL ADDR                              
*                                                                               
         CLI   BUYMD,C'R'          TEST RADIO                                   
         BNE   LD501                                                            
*                                                                               
         OC    SVSPLMKT,SVSPLMKT   SKIP IF SPILL DISPLAY                        
         BNZ   LD501                                                            
*                                                                               
         MVC   ELEM(5),=C'MKT ='                                                
         MVC   ELEM+6(3),=C'???'    INDICATE UNKNOWN                            
         CLI   SVMKTMKT,C' '        TEST MKT REC HAS ALPHA MARKET               
         BNH   *+16                                                             
         MVC   ELEM+6(3),SVMKTMKT   USE MARKET ALPHAMKT AS DEFAULT              
         MVC   ELEM+10(3),=C'(M)'   INDICATE FROM MARKET                        
*                                                                               
         CLC   SVSTAMKT,=C'   '     TEST STATION OVERRIDE                       
         BNH   *+14                                                             
         MVC   ELEM+6(3),SVSTAMKT   ELSE USE IT                                 
         MVI   ELEM+11,C'S'         INDICATE FROM STATION                       
*                                                                               
X        USING DLUBKTYP,ELEM+64                                                 
         CLI   X.DLUBAMKT,0         TEST MARKET OVRD IN DLU ELEM                
         BE    *+14                 NO                                          
         MVC   ELEM+6(3),X.DLUBAMKT                                             
         MVI   ELEM+17,C'*'                                                     
         DROP  X                                                                
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD501    MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+8                                                              
         ST    R6,ELEM+128         SAVE X'50' ELEM ADDRESS (IF ANY)             
*                                                                               
         LR    R6,R7               RESTORE 02 ELEM POINTER                      
*                                                                               
         OC    SVSPLMKT,SVSPLMKT   TEST SPILL REQUEST                           
         BZ    LD503                                                            
*                                                                               
* FIND SPILL ELEM FOR REQ MKT                                                   
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         MVI   ERRCD,NOSPLL                                                     
*                                                                               
         CLC   SVSPLMKT,=X'FFFF'   IF AGENCY MARKET KNOWN                       
         BNE   LD501M                                                           
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   LDERR                                                            
*                                                                               
         USING NDELEM,R6                                                        
         CLC   SVSPLALP,NDMKTALF   COMPARE TO ALPHA MKT NUM                     
         BNE   *-14                                                             
*                                                                               
         MVC   SVSPLMKT,NDAGYMKT   SAVE AGENCY MARKET NUMBER                    
         B     LD501X                                                           
*                                                                               
LD501M   BRAS  RE,NEXTEL                                                        
         BNE   LDERR                                                            
         CLC   SVSPLMKT,NDAGYMKT   COMPARE TO AGY MKT NUM                       
         BNE   *-14                                                             
*                                                                               
LD501X   MVC   ELEM+64(16),NDBKTYPE  SAVE DATA FROM ELEMENT                     
         MVC   NDPROG,4(R7)          AND OVERWRITE WITH PROGRAM NAME            
         DROP  R6                                                               
*                                                                               
         MVC   ELEM(25),=CL25'*SPILL MARKET= XXXX*'                             
*                                                                               
         LH    R0,SVSPLMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+15(4),DUB                                                   
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD503    XC    ELEM(32),ELEM                                                    
         OC    2(2,R6),2(R6)         TEST NO BOOK IN REC                        
         BZ    LD503X                                                           
         GOTO1 VDATCON,DMCB,(3,2(R6)),(9,ELEM)    BOOK                          
*                                                                               
         LA    R1,ELEM+64            POINT TO BOOKTYPE                          
         CLI   0(R1),0                                                          
         BE    LD503A                                                           
         BRAS  RE,GETBKTYP                                                      
         MVC   ELEM+6(2),HALF                                                   
*                                                                               
LD503A   MVC   ELEM+9(16),4(R6)      PROGRAM                                    
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD503X   CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   LD506                                                            
*                                                                               
         XC    ELEM(32),ELEM                                                    
         MVC   ELEM(12),=C'DEMO SOURCE='                                        
*                                                                               
         OC    SVSPLMKT,SVSPLMKT   TEST SPILL                                   
         BNZ   LD504               YES                                          
*                                                                               
X        USING DLUBKTYP,ELEM+64    NON-SPILL USES DEMO LOOKUP ELEM              
*                                                                               
         TM    X.DLUBFLGS,X'01'    TEST BBM                                     
         BZ    *+10                                                             
         MVC   ELEM+12(4),=C'BBM/'                                              
*                                                                               
         TM    X.DLUBFLGS,X'02'    TEST NSI                                     
         BZ    *+10                                                             
         MVC   ELEM+12(4),=C'NSI/'                                              
*                                                                               
         MVC   ELEM+16(3),X.DLUBAMKT  ALPHA MKT OVRD                            
         MVI   ELEM+19,C'/'                                                     
         MVC   ELEM+20(4),X.DLUBSTOV  STATION OVRD                              
         B     LD505                                                            
         DROP  X                                                                
*                                                                               
X        USING NDBKTYPE,ELEM+64                                                 
*                                                                               
LD504    CLI   X.NDRTGSVC,C'0'                                                  
         BNE   *+10                                                             
         MVC   ELEM+12(4),=C'NSI/'                                              
         CLI   X.NDRTGSVC,C'1'                                                  
         BNE   *+10                                                             
         MVC   ELEM+12(4),=C'BBM/'                                              
         MVC   ELEM+16(3),X.NDMKTALF                                            
         MVI   ELEM+19,C'/'                                                     
         MVC   ELEM+20(4),X.NDSTA                                               
*                                                                               
LD505    BRAS  RE,GOBLDFLD                                                      
         DROP  X                                                                
*                                                                               
LD506    LH    RE,BLDROW                                                        
         AHI   RE,1                                                             
         STH   RE,BLDROW                                                        
         XC    BLDCOL,BLDCOL                                                    
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'00072000'    DEMO NAME (PROT)                         
         LA    RE,ELEM                                                          
         ST    RE,4(R1)                                                         
         LA    R1,8(R1)                                                         
         MVC   0(4,R1),=X'00070000'  DEMO VALUE (UNP)                           
         LA    RE,ELEM+10                                                       
         ST    RE,4(R1)                                                         
*                                                                               
         LA    R1,8(R1)                                                         
         MVC   0(4,R1),=X'00012002'  HUT VALUE (PROT)                           
         LA    RE,ELEM+17                                                       
         ST    RE,4(R1)                                                         
*                                                                               
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
*                                                                               
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   BUYMD,C'R'                                                       
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     LD507                                                            
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   LD507                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    LD507                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
LD507    XC    ELEM(32),ELEM       CLEAR OUTPUT AREA                            
         BRAS  RE,GETDEMNM         GET NAME IN DUB                              
         MVC   ELEM(7),DUB                                                      
*                                                                               
         CLI   BUYMD,C'R'          TEST RADIO                                   
         BE    LD510                                                            
         CLI   0(R6),3             TEST SPILL                                   
         BNE   LD510                                                            
         CLI   DUB,C'R'            TEST RATING                                  
         BE    LD510                                                            
         CLI   DUB,C'E'            TEST EXTENDED RATING                         
         JNE   LD519                                                            
                                                                                
* FIND DEMO IN BUYREC *                                                         
                                                                                
LD510    LLC   R0,1(R6)                                                         
         SHI   R0,24                                                            
         BNP   LDX                                                              
         SRL   R0,3                                                             
         LA    R7,24(R6)                                                        
*                                                                               
LD513    CLC   0(3,R4),0(R7)                                                    
         BE    LD515                                                            
*                                                                               
LD514    LA    R7,8(R7)                                                         
         BCT   R0,LD513                                                         
         B     LD517                                                            
*                                                                               
LD515    CLI   2(R4),0             TEST NONT DEMO                               
         JNE   LD516                                                            
         ICM   RE,15,ELEM+128      GET A(X'50 ELEM)                             
         BZ    LD516                                                            
         LLC   R1,1(R4)            GET NONT DEMO INDEX                          
         BCTR  R1,0                                                             
         MHI   R1,9                                                             
         LA    RE,2(R1,RE)         POINT TO DEMO NAME IN X'50' EL               
         CLC   0(7,RE),DUB         COMPARE DEMO NAMES                           
         JNE   LD514                                                            
*                                                                               
LD516    L     R0,4(R7)                                                         
         N     R0,=X'3FFFFFFF'                                                  
*                                                                               
         BAS   RE,LDEDT                                                         
*                                                                               
         TM    4(R7),X'80'                                                      
         BZ    *+8                                                              
         MVI   ELEM+17,C'*'                                                     
*                                                                               
LD517    BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD519    LA    R4,3(R4)                                                         
         CLI   1(R4),0             TEST EOL                                     
         BNE   LD507                                                            
         B     LD530                                                            
         EJECT                                                                  
                                                                                
*==============================================================                 
* SUBROUTINE TO FORMAT DEMO AND HUT VALUES *                                    
*==============================================================                 
                                                                                
LDEDT    SR    R1,R1                                                            
         IC    R1,3(R7)             GET HUT VALUE                               
         MR    R0,R0                                                            
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         TM    4(R7),X'40'         TEST 2 DECIMALS                              
         BO    LDEDT2                                                           
         EDIT  (R0),(7,ELEM+10),1,ALIGN=LEFT                                    
         B     LDEDT4                                                           
*                                                                               
LDEDT2   EDIT  (R0),(7,ELEM+10),2,ALIGN=LEFT                                    
                                                                                
* HUT                                                                           
LDEDT4   DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
*=======================================================                        
* FORMAT DEMO UPGRADE ELEMENT DATA *                                            
*=======================================================                        
                                                                                
LD530    LA    R6,BDELEM                                                        
         MVI   BUHUTADJ,0          CLEAR BKTYPE SAVE AREA                       
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   BUHUTADJ,DLUBKTYP-DLUELEM(R6)                                    
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'62'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   LDX                                                              
*                                                                               
         USING UPELEM,R6                                                        
*                                                                               
         MVC   DSPAREA,SPACES                                                   
         LA    R4,DSPAREA                                                       
         MVC   0(13,R4),=C'** UPGRADE **'                                       
         LA    R4,14(R4)                                                        
*                                                                               
         MVC   0(4,R4),=C'UPX='                                                 
         MVC   2(1,R4),UPFILE                                                   
*                                                                               
*        LA    R1,UPTAB                                                         
         L     R1,=A(UPTAB)                                                     
         A     R1,RELO                                                          
LD532    CLI   0(R1),0             TEST E-O-T                                   
         BE    LD534               YES - MUST BE DEMO UPGRADE                   
         CLC   0(2,R1),UPTYPE      MATCH SPUPTYPE/SPUPSTYP                      
         BE    *+12                                                             
         LA    R1,L'UPTAB(R1)                                                   
         B     LD532                                                            
         MVC   4(6,R4),2(R1)       MOVE UPGRADE TYPE NAME                       
         B     LD536                                                            
*                                                                               
LD534    XC    DUB,DUB             DEMO UPGRADE                                 
         MVC   DUB+1(2),UPTYPE                                                  
         MVI   DUB+3,X'FF'                                                      
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'   GET DEMOCON ADDRESS                  
         L     RF,DMCB                     AND SAVE IN RF                       
         LHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         STCM  RE,15,DMCB+16                                                    
         GOTO1 (RF),DMCB,(1,DUB),(6,4(R4)),(C'S',ADBLOCK),SVUSRNMS              
*                                                                               
LD536    LA    R4,10(R4)           FIND END OF UPGRADE NAME                     
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'          ATTACH DELIMITER                             
         LA    R4,2(R4)                                                         
*                                                                               
         LA    R7,UPTYPE+2         POINT TO FIRST UPGRADE VALUE                 
         LA    R8,3                SET NUMBER OF VALUES                         
*                                                                               
LD540    OC    0(2,R7),0(R7)       TEST VALUE PRESENT                           
         BZ    LD544                                                            
         CLC   0(2,R7),=H'500'     TEST BOOK OR VALUE                           
         BNH   LD542                                                            
         CLC   0(2,R7),=X'C100'    SOME VALUES START WITH LETTERS               
         BL    LD540C                                                           
         MVI   0(R7),0             REMOVE THE LETTER                            
         B     LD542                                                            
*                                                                               
LD540C   BAS   RE,EDITBK           R4 POINTS TO OUTPUT                          
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         B     LD544                                                            
*                                                                               
LD542    BCTR  R4,0                BACK UP TO INSERT DELIMITER                  
         MVI   0(R4),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,0(R7)                                                       
         EDIT  (R0),(4,1(R4)),ALIGN=LEFT                                        
         LA    R4,6(R4)                                                         
*                                                                               
LD544    LA    R7,2(R7)            BUMP TO NEXT VALUE                           
         BCT   R8,LD540            DO FOR MAX VALUES                            
         BCTR  R4,0                OVERWRITE LAST DELIMITER                     
         MVI   0(R4),C' '                                                       
*                                                                               
         OC    UPFBK,UPFBK                                                      
         BZ    LD546                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(3,R4),=C'BK='                                                  
         LA    R4,3(R4)                                                         
         LA    R7,UPFBK                                                         
         BAS   RE,EDITBK                                                        
*                                                                               
         CLI   BUHUTADJ,0          TEST FOR BOOKTYPE                            
         BE    LD544B                                                           
         LA    R1,BUHUTADJ                                                      
         BRAS  RE,GETBKTYP                                                      
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C'('                                                       
         MVC   1(2,R4),HALF                                                     
         CLI   HALF+1,C' '         TWO-CHAR BOOKTYPE?                           
         BH    LD544A              YES                                          
         MVI   2(R4),C')'                                                       
         LA    R4,4(R4)            POINT PAST (X)                               
         B     LD544B                                                           
LD544A   MVI   3(R4),C')'                                                       
         LA    R4,5(R4)            POINT PAST (XX)                              
*                                                                               
LD544B   CLI   UPELEM+1,49                                                      
         BL    *+14                                                             
         OC    UPFBKLST,UPFBKLST   TEST BOOK LIST                               
         BNZ   *+12                                                             
         LA    R4,12(R4)                                                        
         B     LD546                                                            
*                                                                               
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R0,L'UPFBKLST/2                                                  
         LA    R7,UPFBKLST                                                      
*                                                                               
LD545    OC    0(2,R7),0(R7)                                                    
         BZ    LD545A                                                           
         BAS   RE,EDITBK                                                        
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R7,2(R7)                                                         
         BCT   R0,LD545                                                         
*                                                                               
LD545A   BCTR  R4,0                BACK UP OVER LAST DELIMITER                  
         MVI   0(R4),C' '                                                       
         LA    R4,2(R4)                                                         
*                                                                               
LD546    OC    UPDAYTIM,UPDAYTIM                                                
         BZ    LD547                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(3,R4),=C'DT='                                                  
         GOTO1 VCALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),UPDAYTIM,3(R4)                                         
         LA    R4,11(R4)                                                        
         BAS   RE,LDCOMMA2                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),UPDAYTIM+1,1(R4)                                       
*                                                                               
         OC    UPSTA,UPSTA         TEST STATION OVERRIDE                        
         BZ    LD547                                                            
         LA    R4,14(R4)                                                        
         BAS   RE,LDCOMMA2                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         MVC   1(4,R4),UPSTA                                                    
         LA    R4,5(R4)                                                         
*                                                                               
LD547    CLI   1(R6),41            TEST ELEMENT HAS OLD LENGTH                  
         BNH   LD549               YES                                          
         CLI   UP2YRP,C'Y'         NO-PUT/SHR 2 YEAR AVERAGING                  
         BNE   LD548                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(5,R4),=C'PUT=2'                                                
         LA    R4,5(R4)                                                         
*                                                                               
LD548    CLI   UP2YRS,C'Y'                                                      
         BNE   LD549                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(5,R4),=C'SHR=2'                                                
*                                                                               
LD549    LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'004F2000'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
         XC    8(4,R1),8(R1)                                                    
         BRAS  RE,GOBLDFLD                                                      
         B     LDX                                                              
         DROP  R6                                                               
         SPACE 2                                                                
*================================================================*              
* R7 POINTS TO BOOK. PLACE OUTPUT AT 0(R4).                      *              
* R4 POINTS TO NEXT OUTPUT ADDRESS ON EXIT                       *              
*================================================================*              
         SPACE 1                                                                
EDITBK   NTR1                                                                   
         MVC   DUB(2),0(R7)                                                     
         MVI   DUB+2,1                                                          
         NI    DUB+1,X'7F'          TURN OFF OLYM EXCL                          
         GOTO1 VDATCON,DMCB,(3,DUB),(6,WORK)                                    
         MVC   WORK+3(2),WORK+4     MOVE YEAR LEFT                              
         MVC   WORK+5(5),SPACES                                                 
         TM    1(R7),X'80'                                                      
         BZ    *+10                                                             
         MVC   WORK+5(3),=C'(O)'                                                
*                                                                               
         MVC   0(10,R4),WORK                                                    
         LA    R4,10(R4)                                                        
*                                                                               
         CLI   0(R4),C' '          FIND END OF EDITED VALUE                     
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C' '                                                       
         LA    R4,2(R4)                                                         
         XIT1 REGS=(R4)                                                         
         EJECT                                                                  
*============================================================                   
* SPILL DEMOS                                                                   
*============================================================                   
                                                                                
LD550    L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   LD552                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    LD552                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
LD552    MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         MVI   ERRCD,NOSPLL                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   LDERR                                                            
         XC    BLDLIST,BLDLIST     BUILD TITLE LINE                             
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'004F2000'                                             
         LA    RE,ELEM                                                          
         ST    RE,4(R1)                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(6),=C'MARKET'                                               
         LA    R4,SVDEMOS          GET DEMO NAMES                               
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         CLI   1(R4),0             IF NO DEMOS                                  
         JE    EXIT                                                             
*                                                                               
         LA    R0,8                MAX 8 DEMOS                                  
         LA    R1,WORK2            LIST OF DEMO CODES USED                      
         L     R7,AOVWORK          BUILD DEMO NAMES IN OVWORK                   
         XC    0(64,R7),0(R7)                                                   
*                                                                               
LD554    BRAS  RE,GETDEMNM         GET THE DEMO NAME IN DUB                     
*                                                                               
         CLI   BUYMD,C'R'          KEEP ALL DEMOS FOR RADIO                     
         BE    LD556                                                            
         CLI   DUB,C'R'           TEST RATING                                   
         BE    LD556                                                            
         CLI   DUB,C'E'           TEST EXTENDED RATING                          
         BE    LD556                                                            
         B     LD558                                                            
*                                                                               
LD556    MVC   0(3,R1),0(R4)       MOVE DEMO CODE                               
         LA    R1,3(R1)                                                         
         MVC   0(7,R7),DUB         MOVE DEMO NAME                               
         LA    R7,7(R7)                                                         
*                                                                               
LD558    LA    R4,3(R4)                                                         
         CLI   1(R4),0                                                          
         JNE   LD554                                                            
*                                                                               
         LA    R0,7                                                             
         LA    R1,ELEM+8           SET ADDRESS OF TITLE LINE                    
         L     R7,AOVWORK          DEMO NAME LIST                               
*                                                                               
LD559    OC    0(7,R7),0(R7)                                                    
         JZ    LD559A                                                           
         MVC   0(7,R1),0(R7)                                                    
         LA    R1,9(R1)                                                         
         LA    R7,7(R7)                                                         
         JCT   R0,LD559                                                         
*                                                                               
LD559A   LA    R7,7                                                             
         SR    R7,R0               GIVE NUMBER OF DEMOS                         
*                                                                               
LD559X   BRAS  RE,GOBLDFLD         WRITE TITLE LINE                             
*                                                                               
         LA    R8,8                MAX 8 SPILL MARKETS                          
         CLI   BUYMD,C'R'          RADIO ALLOWS 20 MARKETS                      
         BNE   *+8                                                              
         LA    R8,20                                                            
         B     LD564                                                            
*                                                                               
LD563    BRAS  RE,NEXTEL           NEXT SPILL DEMO ELEMENT                      
         BNE   LDX                                                              
*                                                                               
LD564    XC    ELEM,ELEM                                                        
         USING NDELEM,R6                                                        
         EDIT  (2,NDPROG),(4,ELEM),FILL=0                                       
         XC    BLDLIST,BLDLIST     BUILD LIST FOR MARKET NUMBER                 
         MVC   BLDLIST(4),=X'00072000'                                          
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
         BRAS  RE,GOBLDFLD         DISPLAY MARKET NUMBER                        
         MVC   BLDLIST(4),=X'00070000' BUILD LIST FOR DEMO VALUE                
*                                                                               
         LA    R5,WORK2            R5=A(DEMO LIST)                              
         LR    RF,R7               RF=N'DEMOS                                   
*                                                                               
LD565    LLC   R1,NDLEN                                                         
         SHI   R1,(NDEMNO-NDELEM)                                               
         BNP   LD572                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         SRL   R1,3                                                             
         LA    RE,NDEMNO                                                        
*                                                                               
LD566    CLC   0(3,R5),0(RE)                                                    
         BE    LD568                                                            
         LA    RE,8(RE)                                                         
         BCT   R1,LD566                                                         
         B     LD570                                                            
*                                                                               
LD568    L     R0,4(RE)                                                         
         N     R0,=X'3FFFFFFF'                                                  
         LLC   R1,3(RE)            GET HUT VALUE                                
         MR    R0,R0                                                            
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         TM    4(RE),X'40'         TEST 2-DEC                                   
         BO    LD568A                                                           
         EDIT  (R1),(6,ELEM),1,ALIGN=LEFT                                       
         B     LD568B                                                           
LD568A   EDIT  (R1),(6,ELEM),2,ALIGN=LEFT                                       
*                                                                               
LD568B   TM    4(RE),X'80'         TEST OVERRIDE                                
         BZ    LD570                                                            
         LA    R1,ELEM                                                          
         AR    R1,R0                                                            
         MVI   0(R1),C'*'                                                       
*                                                                               
LD570    BRAS  RE,GOBLDFLD         DISPLAY DEMO VALUE                           
         LA    R5,3(R5)            NEXT DEMO                                    
         BCT   RF,LD565                                                         
*                                                                               
LD572    BCT   R8,*+8              NEXT SPILL MARKET                            
         B     LDX                                                              
         LH    R1,BLDROW           NEXT DISPLAY LINE                            
         LA    R1,1(R1)                                                         
         STH   R1,BLDROW                                                        
         XC    BLDCOL,BLDCOL                                                    
         B     LD563                                                            
         DROP  R6                                                               
         EJECT                                                                  
* POST BUY DEMOS                                                                
*                                                                               
LD580    XC    BLDLIST,BLDLIST     BUILD TITLE LINE                             
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'00162000'                                             
         LA    RE,ELEM                                                          
         ST    RE,4(R1)                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(22),=C'*** POST BUY DEMOS ***'                              
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
         XC    ELEM,ELEM           BUILD SECOND TITLE LINE                      
         MVC   ELEM(6),=C'MARKET'                                               
         MVC   0(4,R1),=X'004F2000'                                             
         LA    R4,SVDEMOS          GET DEMO NAMES                               
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
*                                                                               
         CLI   RCLOPT,RCLPDEMX     TEST RECALL 2ND SET OF DEMOS                 
         BNE   LD581                                                            
*                                                                               
         LA    R4,24(R4)           YES-START AT 9TH DEMO                        
         CLI   1(R4),0             TEST THERE ARE MORE THAN 8 DEMOS             
         BNE   LD581                                                            
         MVI   ERRCD,NOMDEMOS      NO-NO MORE DEMOS TO DISPLAY                  
         B     LDERR                                                            
*                                                                               
LD581    XC    WORK2,WORK2         BUILD RATING INDICATORS IN WORK2             
         LA    R0,8                MAX 8 DEMOS                                  
         LA    R1,WORK2                                                         
         SR    R7,R7               R7=DEMO COUNT                                
         LR    RE,R4                                                            
*                                                                               
LD582    CLI   1(RE),0             TEST E-O-L                                   
         BE    LD586                                                            
         CLI   1(RE),C'R'          TEST RATING                                  
         BE    LD585                                                            
         CLI   1(RE),C'E'          TEST EXTENDED RATING                         
         BE    LD585                                                            
*                                                                               
         CLI   1(RE),X'21'         TEST USER DEMO                               
         BNE   LD582A              NO                                           
         LLC   RF,2(RE)            GET DEMO NUMBER                              
         BCTR  RF,0                                                             
         MHI   RF,7                                                             
         LA    RF,SVUSRNMS(RF)     USER-DEF DEMO LIST                           
         B     LD583                                                            
*                                                                               
LD582A   CLI   2(RE),0             TEST NON-T DEMO                              
         JNE   LD584                                                            
         LLC   RF,1(RE)            GET NONT SEQNUM                              
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         LAY   RF,SVNTDMS(RF)      NON-TRAD INDEX DEMO LIST                     
*                                                                               
LD583    CLI   0(RF),C'R'          RATING?                                      
         BE    LD585                                                            
         CLI   0(RF),C'E'          OR EXTENDED?                                 
         BE    LD585                                                            
*                                                                               
LD584    MVI   0(R1),X'20'         INDICATE THIS DEMO'S NOT A RATING            
*                                                                               
LD585    LA    R1,1(R1)                                                         
         LA    R7,1(R7)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,LD582                                                         
*                                                                               
LD586    LTR   R7,R7               R7=N'DEMOS                                   
         BZ    EXIT                                                             
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   LD588                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    LD588                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
LD588    XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'   GET DEMOCON ADDRESS                  
         L     RF,DMCB                     AND SAVE IN RF                       
         L     R5,AOVWORK                                                       
         LHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         STCM  RE,15,DMCB+16                                                    
         GOTO1 (RF),DMCB,((R7),(R4)),(2,(R5)),(C'S',ADBLOCK),SVUSRNMS           
         LR    R0,R7                                                            
         LA    R1,ELEM+8           MOVE DEMO NAMES TO TITLE LINE                
         MVC   0(7,R1),0(R5)                                                    
         LA    R1,9(R1)                                                         
         LA    R5,7(R5)                                                         
         BCT   R0,*-14                                                          
         BRAS  RE,GOBLDFLD         WRITE TITLE LINE                             
*                                                                               
         MVC   HALF,STSPLMKT       STARTING SPILL MKT                           
**       LA    R8,7                MAX 7 MARKETS                                
         LA    R8,0                MAX 7 MARKETS (20 FOR RADIO)                 
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         USING NDELEM,R6                                                        
*                                                                               
LD590    DS    0H                                                               
         CLI   BUYMD,C'R'          RADIO ALLOWS 20 MARKETS                      
         BNE   *+12                                                             
         CHI   R8,20                                                            
         B     *+8                                                              
         CH    R8,=H'7'            MAX 7 MARKETS                                
         BE    LD596                                                            
         BRAS  RE,NEXTEL           NEXT DEMO ELEMENT                            
         BNE   LD596                                                            
         LH    R1,BLDROW           NEXT DISPLAY LINE                            
         LA    R1,1(R1)                                                         
         STH   R1,BLDROW                                                        
         XC    BLDCOL,BLDCOL                                                    
         XC    ELEM,ELEM                                                        
         CLI   NDCODE,2            TEST ORIGINATING MARKET                      
         BNE   LD590A                                                           
         MVC   ELEM(6),=C'*ORIG*'  YES                                          
         B     LD591                                                            
*                                                                               
LD590A   OC    HALF,HALF           STARTING SPILL MKT                           
         BZ    LD590C                                                           
         CLC   NDPROG(2),HALF                                                   
         BE    LD590C              KEEP GOING                                   
         BRAS  RE,NEXTEL                                                        
         BNE   LD596               UNTIL FIND STARTING SPILL MKT                
         B     LD590A                                                           
*                                                                               
LD590C   EDIT  (2,NDPROG),(4,ELEM),FILL=0   NO-SPILL MARKET NUMBER              
         XC    HALF,HALF                                                        
*                                                                               
LD591    XC    BLDLIST,BLDLIST     BUILD LIST FOR MARKET                        
         MVC   BLDLIST(4),=X'00072000'                                          
         LA    RE,ELEM                                                          
         ST    RE,BLDLIST+4                                                     
         BRAS  RE,GOBLDFLD         DISPLAY MARKET                               
*                                                                               
         BAS   RE,LDGETDM          GET THE DEMO VALUES                          
         LA    R1,WORK2            R1=A(RATING INDICATORS)                      
         LA    R5,WORK2+8          R5=A(DEMO VALUE LIST)                        
         LR    RF,R7               RF=N'DEMOS                                   
*                                                                               
LD592    MVC   BLDLIST(4),=X'00060001' BUILD LIST FOR DEMO VALUE                
         XC    ELEM,ELEM                                                        
         CLI   NDCODE,3            TEST SPILL MARKET                            
         BNE   *+18                                                             
         CLI   BUYMD,C'R'          RADIO TREATS ALL DEMOS THE SAME              
         BE    *+10                                                             
         MVC   BLDLIST+2(1),0(R1)  YES-MAKE NON-RATING PROTECTED FIELD          
         CLI   BLDLIST+2,X'20'     TEST PROTECTED                               
         BNE   *+12                                                             
         MVI   BLDLIST,1           YES-LEADING SPACE                            
         MVI   BLDLIST+3,2             AND ANOTHER TRAILING SPACE               
         TM    0(R5),X'80'         TEST OVERRIDE                                
         BZ    LD594               NO-LEAVE BLANK                               
         SR    R0,R0                                                            
         ICM   R0,7,0(R5)                                                       
         N     R0,=X'003FFFFF'                                                  
         TM    0(R5),X'40'         TEST 2-DEC VALUE                             
         BO    LD593                                                            
         EDIT  (R0),(6,ELEM),1,ALIGN=LEFT                                       
         B     LD594                                                            
*                                                                               
LD593    N     R0,=X'3FFFFFFF'                                                  
         EDIT  (R0),(6,ELEM),2,ALIGN=LEFT                                       
*                                                                               
LD594    BRAS  RE,GOBLDFLD         DISPLAY DEMO VALUE                           
         LA    R1,1(R1)                                                         
         LA    R5,3(R5)            NEXT DEMO                                    
         BCT   RF,LD592                                                         
*                                                                               
         MVI   ELCDLO,3            SPILL MARKET DEMO ELEMENTS                   
         MVI   ELCDHI,3                                                         
         LA    R8,1(R8)            MAX 7 MARKETS                                
         B     LD590               NEXT MARKET                                  
*****    BCT   R8,LD590            NEXT MARKET                                  
*                                                                               
LD596    B     LDX                                                              
         DROP  R6                                                               
         SPACE 1                                                                
* BUILD DEMOS VALUES IN WORK2+8                                                 
*                                                                               
LDGETDM  NTR1                                                                   
         XC    WORK2+8(L'WORK2-8),WORK2+8                                       
         LR    R8,R6                                                            
         USING NDELEM,R8                                                        
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'22'        GET POST BUY DEMO ELEMENT                    
         MVI   ELCDHI,X'22'                                                     
         CLI   NDCODE,3            TEST SPILL                                   
         BNE   LDG2                                                             
         MVI   ELCDLO,X'23'        YES-GET POST BUY DEMO SPILL ELEMENT          
         MVI   ELCDHI,X'23'                                                     
*                                                                               
LDG2     BRAS  RE,NEXTEL                                                        
         BNE   LDGX                                                             
         CLI   NDCODE,3            TEST SPILL                                   
         BNE   *+14                                                             
         CLC   NDPROG(2),SDAGYMKT-SDELEM(R6)  YES-MATCH SPILL MARKET            
         BNE   LDG2                                                             
         LA    R2,WORK2+8                                                       
*                                                                               
LDG4     ZIC   R1,NDLEN                                                         
         SHI   R1,24                                                            
         BNP   LDGX                                                             
         SRL   R1,3                R1=N'DEMOS IN DEMO ELEMENT                   
         LA    R3,NDEMNO                                                        
         LA    R5,PDEMO-PDELEM(R6) SET REGS FOR BXLE                            
         CLI   NDCODE,3                                                         
         BNE   *+8                                                              
         LA    R5,SDEMO-SDELEM(R6)                                              
         LA    RE,3                                                             
         ZIC   RF,1(R6)                                                         
         AR    RF,R6                                                            
         BCTR  RF,0                                                             
*                                                                               
LDG6     CLC   0(3,R4),0(R3)       FIND DEMO IN DEMO ELEMENT                    
         BNE   *+14                                                             
         MVC   0(3,R2),0(R5)       MOVE VALUE FROM POST BUY DEMO ELE            
         B     LDG8                                                             
         LA    R3,8(R3)                                                         
         BCT   R1,*+8                                                           
         B     *+8                                                              
         BXLE  R5,RE,LDG6                                                       
*                                                                               
LDG8     LA    R2,3(R2)            NEXT DEMO                                    
         LA    R4,3(R4)                                                         
         BCT   R7,LDG4                                                          
*                                                                               
LDGX     B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
* ACTIVITY                                                                      
*                                                                               
LDACTV   BRAS  RE,LD600                                                         
         B     EXIT                                                             
         EJECT                                                                  
* ORBIT                                                                         
*                                                                               
LD700    DS    0H                                                               
         MVI   ELCDLO,X'67'                                                     
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
* SET UP FOR PROGRAM ENTRIES                                                    
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'00102000'                                             
         LA    RE,=CL16'DAY,TIM,PRG,DEM'                                        
         ST    RE,4(R1)                                                         
         LA    R1,8(R1)            AS OF 6/28/96 DO 2-UP                        
         MVC   0(4,R1),=X'001D0001'                                             
         LA    R1,8(R1)                                                         
         MVC   0(4,R1),=X'001D0000'                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         CLI   0(R6),X'67'                                                      
         BNE   LD718                                                            
*                                                                               
         ZIC   R7,1(R6)                                                         
         SHI   R7,4                                                             
         SRL   R7,4                SET R7 FOR NUM SHOWS PRESENT                 
         LA    R6,4(R6)            POINT TO PROGRAM EL                          
         USING ORBDAY,R6                                                        
         LA    R4,ELEM                                                          
*                                                                               
LD710    ST    R4,FULL                                                          
         GOTO1 VCALLOV,DMCB,0,X'D9000A0F'   GET DAYUNPK ADDRESS                 
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),ORBDAY,(R4)                                            
         BAS   RE,LDCOMMA                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'   GET UNTIME ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),ORBTIME,(R4)                                           
         BAS   RE,LDCOMMA                                                       
         MVC   0(7,R4),ORBDESC     PROGRAM                                      
         BAS   RE,LDCOMMA          DEMO                                         
         SR    R0,R0                                                            
         ICM   R0,3,ORBDEM                                                      
         N     R0,=X'00003FFF'     DROP FLAGS                                   
         TM    ORBDEM,X'40'        TEST 2-DEC                                   
         BO    LD712                                                            
         EDIT  (R0),(5,(R4)),1,ALIGN=LEFT                                       
         B     LD712X                                                           
*                                                                               
LD712    EDIT  (R0),(5,(R4)),2,ALIGN=LEFT                                       
*                                                                               
LD712X   L     R4,FULL                                                          
         LA    R4,40(R4)                                                        
         LA    R6,16(R6)                                                        
         BCT   R7,LD710                                                         
         DROP  R6                                                               
*                                                                               
LD718    LA    R0,9                                                             
         LA    R4,ELEM                                                          
*                                                                               
LD720    ST    R4,BLDLIST+12                                                    
         BCT   R0,LD722                                                         
         XC    BLDLIST+16(8),BLDLIST+16 CLEAR 10TH ENTRY                        
         B     LD724                                                            
*                                                                               
LD722    LA    R4,40(R4)                                                        
         ST    R4,BLDLIST+20                                                    
*                                                                               
LD724    BRAS  RE,GOBLDFLD                                                      
         LA    R4,40(R4)                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   R0,LD720                                                         
*                                                                               
         B     LDX                                                              
         EJECT                                                                  
LDCOMMA  DS    0H                                                               
         LA    R4,8(R4)            POINT TO END (MAX LEN IS 8)                  
*                                                                               
LDCOMMA2 OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,LDCOMMA2                                                      
*                                                                               
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
* SPILL MARKETS                                                                 
*                                                                               
LD750    MVI   ERRCD,NOSPLL                                                     
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LDERR                                                            
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'01262000'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
*                                                                               
B752     MVC   DSPAREA,SPACES                                                   
         LH    R0,4(R6)            AGENCY SPILL MARKET NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPAREA(4),DUB      DISPLAY AGENCY MARKET NUMBER                 
*                                                                               
         CLI   BUYMD,C'R'          SKIP IF NOT RADIO                            
         BNE   B753                                                             
*                                                                               
         CLC   4+13(3,R6),SPACES   IF MARKET ALPHA CODE KNOWN                   
         BNH   *+18                                                             
         MVI   DSPAREA+26,C'('        DISPLAY IT                                
         MVC   DSPAREA+27(3),4+13(R6)                                           
         MVI   DSPAREA+30,C')'                                                  
*                                                                               
B753     DS    0H                                                               
         SPACE 1                                                                
* TEST ALL DEMO VALUES ZERO *                                                   
         SPACE 1                                                                
         ZIC   R0,1(R6)                                                         
         SHI   R0,24                                                            
         BNP   B754X                                                            
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,24(R6)           POINT TO FIRST DEMO VALUE                    
B754     L     RE,4(R1)                                                         
         N     RE,=X'3FFFFFFF'                                                  
         LTR   RE,RE                                                            
         BNZ   B756                                                             
         LA    R1,8(R1)                                                         
         BCT   R0,B754                                                          
B754X    MVI   DSPAREA+5,C'Z'      SET IND FOR ZERO SPILL                       
         SPACE 1                                                                
* READ MKT REC FOR NAME                                                         
         SPACE 1                                                                
B756     BRAS  RE,GETMKT                                                        
*                                                                               
         L     R8,AREC2                                                         
         MVC   DSPAREA+7(13),=C'** UNKNOWN **'                                  
*                                                                               
         CLC   KEY(15),0(R8)       MARKET NOT ON FILE                           
         BNE   B760                                                             
*                                                                               
         MVC   DSPAREA+7(19),18(R8)   DISPLAY NAME                              
*                                                                               
         CLC   DSPAREA+26(5),SPACES   IF NO MARKET ALPHA CODE                   
         BH    *+10                                                             
         MVC   DSPAREA+26(5),37(R8)      DISPLAY REST OF NAME                   
*                                                                               
B760     BRAS  RE,GOBLDFLD                                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    B752                                                             
         B     LDX                                                              
         EJECT                                                                  
*=================================================================*             
* STATIONS BOUGHT FOR CANAD NTWK BUY                              *             
*=================================================================*             
         SPACE 1                                                                
LD775    MVI   ERRCD,NOTNETWK                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BZ    LDERR                                                            
         MVC   DSPAREA,SPACES                                                   
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LDERR                                                            
* NOTE START STATION DISPLAY AFTER FIRST INPUT LINE                             
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'01042000'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
         LA    R1,8(R1)                                                         
         MVC   0(4,R1),=X'01060000'                                             
         LA    RE,DSPAREA+5                                                     
         ST    RE,4(R1)                                                         
*                                                                               
         CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         BNE   LD777                                                            
* TEST ANY DEMO OVERRIDE ERRORS                                                 
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
LD776    TM    SVNPSHR,X'C0'                                                    
         BNZ   LD780                                                            
         LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   LD776                                                            
         DROP  R7                                                               
*                                                                               
LD777    OC    SVRCLSTA,SVRCLSTA   TEST STARTING STATION                        
         BZ    LD777B              NO                                           
*                                                                               
* FIND 68 ELEM FOR STARTING STATION                                             
*                                                                               
LD777A   GOTO1 STAPACK,DMCB,(C'U',2(R6)),DUB,(X'80',WORK)                       
         CLC   SVRCLSTA,WORK                                                    
         BE    LD777B                                                           
         BRAS  RE,NEXTEL                                                        
         BE    LD777A                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NORCLSTA)                                              
         B     LDERR                                                            
*                                                                               
LD777B   GOTO1 STAPACK,DMCB,(C'U',2(R6)),DUB,(X'80',WORK)                       
         MVC   DSPAREA(4),WORK                                                  
*                                                                               
         CLI   SVNETBTS,X'01'      TEST CABLE                                   
         BNE   LD777C                                                           
         MVC   DSPAREA(4),SPACES                                                
         MVC   DSPAREA(2),WORK+5                                                
*                                                                               
LD777C   MVC   FULL,7(R6)                                                       
         BAS   RE,LD775EDT                                                      
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    LD777B                                                           
* DISPLAY TOTAL                                                                 
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'010D2002'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
*                                                                               
         LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,20(R6)        GET RGN TOT IF ANY                           
         BNZ   LD778B                                                           
*                                                                               
         SR    R8,R8                                                            
LD778A   BRAS  RE,NEXTEL                                                        
         BNE   LD778B                                                           
         MVC   FULL,7(R6)                                                       
         A     R8,FULL                                                          
         B     LD778A                                                           
LD778B   MVC   DSPAREA(4),=C'TOT='                                              
         ST    R8,FULL                                                          
         BAS   RE,LD775EDT                                                      
         BRAS  RE,GOBLDFLD                                                      
         B     LDX                                                              
*                                                                               
LD775EDT L     R0,FULL                                                          
         LA    R4,DSPAREA+5                                                     
         EDIT  (R0),(7,(R4)),3,ALIGN=LEFT,ZERO=NOBLANK                          
         BR    RE                                                               
         EJECT                                                                  
* DISPLAY NETWORK PERCENTAGES                                                   
*                                                                               
LDNET    LA    R2,BUYINP2H         DISPLAY OVER SECOND <<INPUT>> LINE           
         XC    0(3,R2),0(R2)       SET E-O-S FLAG                               
         MVC   BLDROW(4),=X'000B0000' DISPLAY STARTS IN ROW 11                  
         MVI   SVSCR,X'F1'            TELL BLDFLD WHAT'S UP                     
*                                                                               
         MVI   ERRCD,NOTNETWK                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BZ    LDERR                                                            
         MVC   DSPAREA,SPACES                                                   
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
*                                                                               
LDNET4   DS    0H                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'01042000'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
         LA    R1,8(R1)                                                         
         MVC   0(4,R1),=X'00060000'                                             
         LA    RE,DSPAREA+5                                                     
         ST    RE,4(R1)                                                         
*                                                                               
         LA    R7,SVNDEF                                                        
         SR    R8,R8               CLEAR TOTAL ACCUM                            
*                                                                               
LDNET6   GOTO1 STAPACK,DMCB,(C'U',SVNDMKST),DUB,(X'80',WORK)                    
         MVC   DSPAREA(4),WORK                                                  
*                                                                               
         CLI   SVNETBTS,X'01'      TEST CABLE                                   
         BNE   LDNET7                                                           
         MVC   DSPAREA(4),SPACES                                                
         MVC   DSPAREA(2),WORK+5                                                
*                                                                               
LDNET7   CLI   RCLOPT,RCLCLST      TEST CUT-IN LIST DISPLAY                     
         BNE   LDNET8                                                           
         TM    SVNDSTAT,SVNDCUTQ                                                
         BZ    *+8                                                              
         MVI   DSPAREA+5,C'X'                                                   
         B     LDNET10                                                          
*                                                                               
LDNET8   MVC   FULL,SVNDPCT                                                     
         MVC   DSPAREA+5(2),=C'NB'                                              
         CLC   =C'NDIS=NB',BUYINP1   TEST FORCE TO NB                           
         BE    LDNET10                                                          
         ICM   R0,15,FULL                                                       
         C     R0,=F'-1'           TEST NOT BOUGHT                              
         BE    LDNET10                                                          
         AR    R8,R0                                                            
         BAS   RE,LD775EDT                                                      
*                                                                               
LDNET10  BRAS  RE,GOBLDFLD                                                      
         XC    DSPAREA,DSPAREA                                                  
*                                                                               
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKT,SVNDMKT                                                  
         BNZ   LDNET6                                                           
         EJECT                                                                  
         CLI   RCLOPT,RCLCLST                                                   
         BE    LDNET20                                                          
* DISPLAY TOTAL                                                                 
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'010D2002'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
         MVC   DSPAREA(4),=C'TOT='                                              
         ST    R8,FULL                                                          
         OC    SVNPCTG,SVNPCTG                                                  
         BZ    *+10                                                             
         MVC   FULL,SVNPCTG                                                     
         BAS   RE,LD775EDT                                                      
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
         L     RE,=A(NETWKMSG)                                                  
         B     LDNET30                                                          
*                                                                               
LDNET20  L     RE,=A(CUTINMSG)                                                  
*                                                                               
LDNET30  A     RE,RELO                                                          
         MVC   BUYMSG(33),0(RE)                                                 
         B     LDX                                                              
         EJECT                                                                  
* DISPLAY STATION DEMO OVERRIDE ERRORS                                          
*                                                                               
LD780    XC    BUYMSG,BUYMSG                                                    
         L     RE,=A(DEMOVMSG)                                                  
         A     RE,RELO                                                          
         MVC   BUYMSG(38),0(RE)                                                 
*                                                                               
         L     R7,AREC3                                                         
         USING SVNPGMD,R7                                                       
LD782    TM    SVNPSHR,X'C0'                                                    
         BZ    LD784                                                            
*                                                                               
         GOTO1 STAPACK,DMCB,(C'U',SVNPMKST),DUB,(X'80',WORK)                    
         MVC   DSPAREA(4),WORK                                                  
*                                                                               
         CLI   SVNETBTS,X'01'      TEST CABLE                                   
         BNE   LD783                                                            
         MVC   DSPAREA(4),SPACES                                                
         MVC   DSPAREA(2),WORK+5                                                
*                                                                               
LD783    TM    SVNPSHR,X'80'       TEST STATION OVRD ERR                        
         BO    *+10                                                             
         MVC   DSPAREA+5(7),=C'(SPILL)'                                         
         BRAS  RE,GOBLDFLD                                                      
LD784    LA    R7,SVNPLEN(R7)                                                   
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   LD782                                                            
         B     LDX                                                              
         DROP  R7                                                               
         EJECT                                                                  
*======================================================                         
* DISPLAY CANADIAN LOCAL STATION NETWORK LIST                                   
* NETWORK LIST IS BY DISPLACEMENT IN AREC3                                      
*======================================================                         
                                                                                
LDNL     BRAS  RE,DSPLDNL                                                       
         B     EXIT                                                             
                                                                                
*=====================================================                          
* DISPLAY BUY HISTORY VIA SPXBHIST                                              
*=====================================================                          
                                                                                
LD800    BRAS  RE,DSPHIST                                                       
         B     EXIT                                                             
                                                                                
*=====================================================                          
* DISPLAY SPECIAL LCI LOCKIN INPUT                                              
*=====================================================                          
                                                                                
LD820    BRAS  RE,DSPLKIN                                                       
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    FNDUFX                                                           
         TM    1(R2),X'20'                                                      
         JO    FNDUF                                                            
         CLI   0(R2),9                                                          
         JNH   FNDUF                                                            
         CR    RB,RB                                                            
         BR    RE                                                               
FNDUFX   LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(4),DSPAREA                                                 
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R0,AREC                                                          
         L     R8,AREC2                                                         
         ST    R8,AREC                                                          
         GOTO1 STA                                                              
         ST    R0,AREC                                                          
         XIT1                                                                   
         LTORG                                                                  
* DISPLAY AMFM TRACE ELEMENT                                                    
         USING BTRCELEM,R6                                                      
AMFMTRC  NTR1  BASE=*,LABEL=*                                                   
         CLI   1(R6),20            IGNORE IF NOT 20 LONG (AM/FM)                
         BNE   AMFMX                                                            
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(7,R4),=C'SENT BY'                                              
         MVC   8(8,R4),BTRCLUID                                                 
         MVC   17(2,R4),=C'ON'                                                  
         GOTO1 VDATCON,DMCB,(3,BTRCDATE),(5,20(R4))                             
         LA    R4,29(R4)                                                        
*                                                                               
         MVC   0(11,R4),=C'AT HH.MM.SS'                                         
         LA    R4,3(R4)                                                         
         LA    R1,BTRCTIME                                                      
         LA    R0,3                                                             
AMFM2    BRAS  RE,CVD                                                           
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,AMFM2                                                         
*                                                                               
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),=C'SEQ='                                                 
         OC    BTRCSEQN(2),BTRCSEQN   TEST NEW STYLE                            
         BNZ   AMFM4                                                            
         ICM   R0,15,BTRCSEQN                                                   
         CVD   R0,DUB                                                           
         EDIT  (R0),(5,4(R4)),ALIGN=LEFT                                        
         B     AMFMX                                                            
*                                                                               
AMFM4    L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         MVC   0(5,R4),=C'SEQX='                                                
         GOTO1 (RF),DMCB,BTRCSEQN,5(R4),4                                       
AMFMX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* DISPLAY DARE TRACE ELEMENT                                                    
*===========================================================                    
         SPACE 1                                                                
         USING BDARELEM,R6                                                      
DARETRC  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(23,R4),=C'DARE MAKEGOOD FOR ORDER'                             
         MVC   24(8,R4),BDARORD                                                 
         MVC   33(2,R4),=C'ON'                                                  
         GOTO1 VDATCON,DMCB,(3,BDARDATE),(5,36(R4))                             
         LA    R4,45(R4)                                                        
*                                                                               
         MVC   0(11,R4),=C'AT HH.MM.SS'                                         
         LA    R4,3(R4)                                                         
         LA    R1,BDARTIME                                                      
         LA    R0,3                                                             
DARE2    BRAS  RE,CVD                                                           
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DARE2                                                         
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(6,R4),=C'BUYER='                                               
         MVC   6(3,R4),BDARBYR                                                  
         LA    R4,10(R4)                                                        
         MVC   0(4,R4),=C'GRP='                                                 
         MVC   4(3,R4),BDARMKGP                                                 
         LA    R4,8(R4)                                                         
         CLI   BDARFLT,0                                                        
         BE    DARE4                                                            
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         SR    R0,R0                                                            
         IC    R0,BDARFLT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         LA    R4,4(R4)                                                         
*                                                                               
DARE4    LA    RE,BDARSEQN                                                      
         LA    RF,3                                                             
*                                                                               
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   DAREX               NO, DON'T SHOW IT                            
*                                                                               
DARE6    SR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C'/'                                                       
         LA    R4,3(R4)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DARE6                                                         
*                                                                               
         BCTR  R4,0                BACK UP TO LAST /                            
         MVI   0(R4),0                                                          
*                                                                               
DAREX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* DISPLAY NETWORK LIST                                                          
*===========================================================                    
         SPACE 1                                                                
DSPLDNL  NTR1  BASE=*,LABEL=*                                                   
         XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'01042001'                                             
         LA    RE,DSPAREA                                                       
         ST    RE,4(R1)                                                         
*                                                                               
         L     R4,AREC3                                                         
         LA    R5,256                                                           
*                                                                               
LDNL2    MVC   DSPAREA(4),0(R4)                                                 
         OC    0(4,R4),0(R4)                                                    
         BZ    *+8                                                              
         BRAS  RE,GOBLDFLD                                                      
         LA    R4,4(R4)                                                         
         BCT   R5,LDNL2                                                         
         MVC   BUYMSG(32),=C'* NETWORKS WITH BUYS DISPLAYED *'                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* DISPLAY BUY HISTORY DATA                                                      
*===========================================================                    
         SPACE 1                                                                
DSPHIST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVRCLOPT,RCLHIST    SET CURRENT RECALL OTPION                    
*                                                                               
         LA    R2,BUYINP1H                                                      
         XC    ELEM,ELEM                                                        
         ZIC   R1,5(R2)            SAVE CURRENT INPUT LINE IN ELEM              
         STC   R1,ELEM             SAVE REAL LEN (THANKS MEL)                   
         LTR   R1,R1                                                            
         BZ    DSP02                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+1(0),8(R2)                                                  
*                                                                               
DSP02    DS    0H                                                               
         CLI   SVSCR,X'F6'                                                      
         BE    DSP03                                                            
*        MVC   ELEM(80),BUYINP1                                                 
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90211F6'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SVSCR,X'F6'                                                      
*                                                                               
         LA    R2,HSTINP1H         RESTORE INPUT DATA                           
         CLI   ELEM,0                                                           
         BE    DSP03                                                            
         ZIC   R1,ELEM                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ELEM+1                                                   
*                                                                               
DSP03    DS    0H                                                               
*                                                                               
         CLI   PFKEY,X'06'                                                      
         BE    DSP04                                                            
         CLC   SVHFLAGS+3(2),SVKEY+11         TEST BUY LINE NUMBER              
         BE    DSP05                                                            
DSP04    DS    0H                                                               
         XC    SVHFLAGS,SVHFLAGS                                                
DSP05    DS    0H                                                               
         MVC   SVHFLAGS+3(2),SVKEY+11         SAVE BUYLINE NUMBER               
*                                                                               
DSP06    DS    0H                                                               
*                                                                               
         LA    R2,HSTOUTH          R2 IS SCREEN POINTER NOW                     
         XC    WORK,WORK                                                        
X        USING XBHISTD,WORK                                                     
* BUILD INITIAL PARAMETER LIST                                                  
         L     RE,AREC2            A(HISTORY REC)                               
         ST    RE,X.XBAHIST                                                     
         L     RE,VCOMFACS                                                      
         ST    RE,X.XBACOMF                                                     
         SR    RE,RE                                                            
         ICM   RE,3,SVSECAGY                                                    
         ST    RE,X.XBSECAGY                                                    
         MVC   X.XBFLAGS,SVHFLAGS                                               
         MVC   X.XBELEM,SVHFLAGS+1                                              
         MVC   X.XBCOUNT,SVHFLAGS+2                                             
*                                                                               
DSP10    DS    0H                                                               
*                                                                               
         LA    R8,8(R2)                                                         
         ST    R8,X.XBAOUT1        A(OUTPUT LINE)                               
         GOTO1 =V(XBHIST),DMCB,WORK,RR=Y                                        
         OI    6(R2),X'80'                                                      
         TM    X.XBFLAGS,XBENDQ    END OF HISTORY RECORD?                       
         BNO   DSP15                                                            
         XC    SVHFLAGS(3),SVHFLAGS    CLEAR FLAGS                              
         B     DSPXIT                                                           
*                                                                               
DSP15    DS    0H                                                               
         TM    X.XBFLAGS,XBNOPRTQ   NO OUTPUT FROM XBHIST?                      
         BO    DSP10        THEN PROCEED WITHOUT ADVANCING SCREEN PTR           
         BAS   RE,ADVSCR                                                        
         BE    DSP10      IF NOT END OF SCREEN, PROCEED                         
*                                                                               
         MVC   SVHFLAGS(1),X.XBFLAGS                                            
         MVC   SVHFLAGS+1(1),X.XBELEM                                           
         MVC   SVHFLAGS+2(1),X.XBCOUNT                                          
DSPXIT   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
ADVSCR   DS    0H                                                               
* R2 IS EXPECTED TO ADDRESS OUTPUT LINE ON F6 SCREEN                            
* UNEQUAL EXIT CONDITION IF YOU TRY TO ADVANCE R2 BEYOND THE LAST LINE          
         LA    R0,HSTOUTXH                                                      
         CR    R0,R2                                                            
         BE    ANEQXIT                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     AEQXIT                                                           
*                                                                               
AEQXIT   CR    RB,RB                                                            
         BR    RE                                                               
ANEQXIT  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
* PROVIDE LINKAGE TO BLDFLD WITH RETURN TO ORIGINAL CALLER                      
*                                                                               
GOBLDFLD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VBLDFLD                                                          
         XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
UPTAB    DS    0XL8                SPUPTYPE/SPUTSTYP/UPGRADE NAME               
         DC    AL1(SPUPTRTG),X'0',C'RATING'                                     
         DC    AL1(SPUPTHUT),X'0',C'HUT   '                                     
         DC    AL1(SPUPTPUT),C'P',C'PUT   '                                     
         DC    AL1(SPUPTNDX),X'0',C'INDEX '                                     
         DC    AL1(SPUPTHPT),X'0',C'HPT   '                                     
         DC    AL1(SPUPTPIN),C'N',C'PIN   '                                     
         DC    AL1(SPUPTPIM),C'M',C'PIM   '                                     
         DC    AL1(SPUPTPIY),C'Y',C'PIY   '                                     
         DC    AL1(SPUPTPIQ),C'Q',C'PIQ   '                                     
         DC    AL1(SPUPTPIB),C'B',C'PIB   '                                     
         DC    AL1(SPUPTSIN),C'N',C'SIN   '                                     
         DC    AL1(SPUPTSIM),C'M',C'SIM   '                                     
         DC    AL1(SPUPTSIY),C'Y',C'SIY   '                                     
         DC    AL1(SPUPTSIQ),C'Q',C'SIQ   '                                     
         DC    AL1(SPUPTSIB),C'B',C'SIB   '                                     
         DC    AL1(SPUPTPAV),C'N',C'PAVG  '                                     
         DC    AL1(SPUPTPAY),C'Y',C'PAY   '                                     
         DC    AL1(SPUPTPAQ),C'Q',C'PAQ   '                                     
         DC    AL1(SPUPTPAB),C'B',C'PAB   '                                     
         DC    AL1(SPUPTSAV),C'N',C'SAVG  '                                     
         DC    AL1(SPUPTSAY),C'Y',C'SAY   '                                     
         DC    AL1(SPUPTSAQ),C'Q',C'SAQ   '                                     
         DC    AL1(SPUPTSAB),C'B',C'SAB   '                                     
         DC    X'00'                                                            
         SPACE 1                                                                
DEMOVMSG DC    CL38'** DEMO OVERRIDE ERRORS - SEE BELOW **'                     
NETWKMSG DC    CL33'* NETWORK PERCENTAGES DISPLAYED *'                          
CUTINMSG DC    CL33'* CUT-IN STATIONS DISPLAYED *'                              
         LTORG                                                                  
*==============================================================                 
* DETACH ACTIVITY DISPLAY FOR ADDRESSABILITY PROBLEMS                           
*==============================================================                 
         SPACE 1                                                                
LD600    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,BLDLIST                                                       
         MVC   0(4,R1),=X'004F2000'                                             
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         ST    RE,4(R1)                                                         
         XC    8(4,R1),8(R1)                                                    
         BRAS  RE,GOBLDFLD                                                      
         MVC   ELEM(13),=C'LAST ACTIVITY'                                       
         GOTO1 VDATCON,DMCB,(3,BDCHG),(5,ELEM+14)                               
*                                                                               
         LA    R4,ELEM+25                                                       
         CLI   BDWHY,0                                                          
         BE    LD624                                                            
         TM    BDWHY,X'80'                                                      
         BZ    LD602                                                            
         MVC   0(8,R4),=C'NEW BUY/'                                             
         LA    R4,8(R4)                                                         
*                                                                               
LD602    TM    BDWHY,X'40'                                                      
         BZ    LD604                                                            
         MVC   0(11,R4),=C'BUY CHANGE/'                                         
         LA    R4,11(R4)                                                        
*                                                                               
LD604    TM    BDWHY,X'20'                                                      
         BZ    LD606                                                            
         MVC   0(11,R4),=C'POOL ALLOC/'                                         
         LA    R4,11(R4)                                                        
*                                                                               
LD606    TM    BDWHY,X'10'                                                      
         BZ    LD608                                                            
         MVC   0(4,R4),=C'OTO/'                                                 
         LA    R4,4(R4)                                                         
*                                                                               
LD608    TM    BDWHY,X'08'                                                      
         BZ    LD610                                                            
         MVC   0(15,R4),=C'COMMENT CHANGE/'                                     
         LA    R4,15(R4)                                                        
*                                                                               
LD610    TM    BDWHY,X'04'                                                      
         BZ    LD612                                                            
         MVC   0(9,R4),=C'BUY COPY/'                                            
         LA    R4,9(R4)                                                         
*                                                                               
LD612    TM    BDWHY,X'02'                                                      
         BZ    LD614                                                            
         MVC   0(12,R4),=C'DEMO LOOKUP/'                                        
         LA    R4,12(R4)                                                        
*                                                                               
LD614    TM    BDWHY,X'01'                                                      
         BZ    LD620                                                            
         MVC   0(11,R4),=C'PKG CHANGE/'                                         
         LA    R4,11(R4)                                                        
*                                                                               
LD620    DS    0H                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD622    CLI   BDWHY2,0                                                         
         BE    LD641                                                            
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+25                                                       
LD624    TM    BDWHY2,X'80'                                                     
         BZ    LD626                                                            
         MVC   0(11,R4),=C'AFFIDAVITS/'                                         
         LA    R4,11(R4)                                                        
LD626    TM    BDWHY2,X'40'                                                     
         BZ    LD628                                                            
         MVC   0(8,R4),=C'DAYPART/'                                             
         LA    R4,8(R4)                                                         
LD628    TM    BDWHY2,X'20'                                                     
         BZ    LD630                                                            
         MVC   0(3,R4),=C'ID/'                                                  
         LA    R4,4(R4)                                                         
         LHI   RE,SVB0PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   9(RE),C'O'          OPTIONAL PURPOSE CODE                        
         JE    *+12                                                             
         CLI   9(RE),C'Y'          TEST PURPOSE CODE REQD                       
         BNE   LD630                                                            
         AHI   R4,-4               BACK UP                                      
         MVC   0(10,R4),=C'PURP CODE/'                                          
         LA    R4,10(R4)                                                        
LD630    DS    0H                                                               
         TM    BDWHY2,X'10'                                                     
         BZ    LD632                                                            
         MVC   0(12,R4),=C'???????????/'                                        
         LA    R4,12(R4)                                                        
LD632    DS    0H                                                               
         TM    BDWHY2,X'08'                                                     
         BZ    LD634                                                            
         MVC   0(9,R4),=C'DEMO CHG/'                                            
         LA    R4,9(R4)                                                         
LD634    DS    0H                                                               
         TM    BDWHY2,X'04'                                                     
         BZ    LD636                                                            
         MVC   0(8,R4),=C'HUT CHG/'                                             
         LA    R4,8(R4)                                                         
LD636    TM    BDWHY2,X'02'                                                     
         BZ    LD638                                                            
         MVC   0(10,R4),=C'ORBIT CHG/'                                          
         LA    R4,10(R4)                                                        
LD638    DS    0H                                                               
         TM    BDWHY2,X'01'                                                     
         BZ    LD640                                                            
         MVC   0(9,R4),=C'FILM CHG/'                                            
         LA    R4,9(R4)                                                         
*                                                                               
LD640    DS    0H                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
         EJECT                                                                  
LD641    DS    0H                                                               
         CLI   BDWHY3,0                                                         
         BE    LD643                                                            
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+25                                                       
         TM    BDWHY3,X'80'                                                     
         BZ    LD642                                                            
         MVC   0(12,R4),=C'SKED CHANGE/'                                        
         LA    R4,12(R4)                                                        
*                                                                               
LD642    TM    BDWHY3,X'40'                                                     
         BZ    LD642A                                                           
         MVC   0(9,R4),=C'BUY MOVE/'                                            
         LA    R4,9(R4)                                                         
*                                                                               
LD642A   TM    BDWHY3,X'20'                                                     
         BZ    LD642X                                                           
         MVC   0(15,R4),=C'-OTO''S BY $MAT'                                     
         LA    R4,15(R4)                                                        
*                                                                               
LD642X   BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD643    MVI   ELCDLO,BUPCODEQ    LOOK FOR BUY UPLOAD ELEMENT                   
         MVI   ELCDHI,BUPCODEQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD643C                                                           
         USING BUPELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(12,R4),=C'BUY UPLOAD, '                                        
         GOTO1 VDATCON,DMCB,(3,BUPDAT),(5,12(R4))                               
         CLI   BUPLEN,14           TEST TIME IS IN THE ELEMENT                  
         BNH   LD643B                                                           
         MVC   20(4,R4),=C' AT '   YES-                                         
         LA    R1,BUPTIME                                                       
         LA    RE,24(R4)                                                        
         SR    RF,RF                                                            
         LA    R0,3                                                             
*                                                                               
LD643A   IC    RF,0(R1)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,RE),DUB                                                      
         MVI   2(RE),C':'                                                       
         LA    RE,3(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,LD643A                                                        
         LA    R4,12(R4)                                                        
*                                                                               
LD643B   MVC   20(5,R4),=C', ID='                                               
         MVC   25(L'BUPUID,R4),BUPUID                                           
         TM    BUPIND,BUPIUPD                                                   
         BZ    *+10                                                             
         MVC   25+L'BUPUID(14,R4),=C', UPDATES ONLY'                            
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD643C   DS    0H                 BWS TRANSFER INFORMATION                      
         TM    BDSTAT,X'04'       BWS TRANSFER ELEMENT?                         
         BNO   LD645              NONE                                          
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(13,R4),=C'BWS TRANSFER '                                       
         LA    R4,14(R4)                                                        
         MVI   ELCDLO,X'97'                                                     
         MVI   ELCDHI,X'97'                                                     
         LA    R6,BDELEM          LOOK FOR BWS TRANSFER ELEMENT                 
         USING BWSELEM,R6                                                       
         BRAS  RE,NEXTEL                                                        
         BNE   LD644              NONE -DISPLAY WHAT WE HAVE SO FAR             
*                                                                               
         MVC   0(6,R4),=C'BUYER='   DISPLAY BWS BUYER                           
         LA    R4,6(R4)                                                         
         MVC   0(L'BWSBYR,R4),BWSBYR                                            
         LA    R4,L'BWSBYR(R4)                                                  
         MVI   0(R4),C','                                                       
         LA    R4,2(R4)                                                         
         MVC   0(9,R4),=C'CAMPAIGN='   DISPLAY CAMPAIGN NUMBER                  
         LA    R4,10(R4)                                                        
         EDIT  BWSCAM,(5,0(R4)),ALIGN=LEFT                                      
         CLI   BWSLEN,7            TEST ELEM CONTAINS DATE                      
         BNH   LD644                                                            
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         GOTO1 VDATCON,DMCB,(3,BWSDATE),(8,2(R4))                               
*                                                                               
LD644    BRAS  RE,GOBLDFLD                                                      
         EJECT                                                                  
* DISPLAY X'98' OR X'99' OR X'9E' IF PRESENT                                    
*                                                                               
LD645    DS    0H                                                               
         MVI   ELCDLO,X'98'                                                     
         MVI   ELCDHI,X'9E'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
LD645A   BRAS  RE,NEXTEL                                                        
         BNE   LD647                                                            
*                                                                               
         CLI   0(R6),X'98'         AMFM                                         
         BE    LD646                                                            
         CLI   0(R6),X'99'         SECURITY ACTIVITY                            
         BE    LD645D                                                           
         CLI   0(R6),X'9E'                                                      
         BNE   LD645A                                                           
*                                                                               
         LR    R7,R6               SAVE 9E ELEM POINTER                         
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'99'                                                     
         MVI   ELCDHI,X'99'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   LD645C                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),2(R6)        MOVE PID                                     
         LA    R5,DUB                                                           
         BAS   RE,LDAUTH                                                        
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
LD645C   LR    R6,R7                                                            
         BRAS  RE,DARETRC                                                       
         BRAS  RE,GOBLDFLD                                                      
         B     LD647                                                            
*                                                                               
LD645D   XC    ELEM,ELEM                                                        
         MVC   ELEM(7),=C'CREATED'                                              
         TM    BDSTAT3,BDST3_DSKADD                                             
         BZ    LD645E                                                           
         MVC   ELEM+46(12),=C'BY CDN DSKTP'                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    *+10                 YES                                         
         MVC   ELEM+46(12),=C'BY DESKTOP  '                                     
LD645E   LA    R5,2(R6)                                                         
         BAS   RE,LDAUTH                                                        
         BRAS  RE,GOBLDFLD                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(7),=C'CHANGED'                                              
         TM    BDSTAT3,BDST3_DSKCHG                                             
         BZ    LD645F                                                           
         MVC   ELEM+46(12),=C'BY CDN DSKTP'                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    *+10                 YES                                         
         MVC   ELEM+46(12),=C'BY DESKTOP  '                                     
LD645F   LA    R5,7(R6)                                                         
         OC    0(3,R5),0(R5)                                                    
         BZ    LD645A                                                           
         BAS   RE,LDAUTH                                                        
         BRAS  RE,GOBLDFLD                                                      
         B     LD645A                                                           
*                                                                               
LD646    BRAS  RE,AMFMTRC                                                       
         BRAS  RE,GOBLDFLD                                                      
         B     LD645A                                                           
*                                                                               
CVD      ZIC   RF,0(R1)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
LD647    MVI   ELCDLO,MFXCODEQ     LOOK FOR MKTFIX ELEMENT                      
         MVI   ELCDHI,MFXCODEQ                                                  
         LA    R6,BDELEM                                                        
*                                                                               
LD647A   BRAS  RE,NEXTEL                                                        
         BNE   LD648                                                            
*                                                                               
         USING MFXELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         GOTO1 VDATCON,DMCB,(2,MFXDATE),(8,(R4))                                
         LA    R4,10(R4)                                                        
         MVC   0(17,R4),=C'MOVED FROM MARKET'                                   
         SR    R0,R0                                                            
         ICM   R0,3,MFXMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  19(4,R4),DUB                                                     
         BRAS  RE,GOBLDFLD                                                      
         B     LD647A              GO CHECK FOR MORE ELEMENTS                   
         DROP  R6                                                               
*                                                                               
LD648    MVI   ELCDLO,X'9D'          LOOK FOR STATION CHANGE ELEMENT            
         MVI   ELCDHI,X'9F'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
LD648A   BRAS  RE,NEXTEL                                                        
         BNE   LD650                                                            
         CLI   0(R6),X'9E'         WANT 9D/9F BUT NOT 9E                        
         BE    LD648A                                                           
*                                                                               
LD648B   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING SFXELEM,R6                                                       
         GOTO1 VDATCON,DMCB,(2,SFXDATE),(8,(R4))                                
         LA    R4,10(R4)                                                        
*                                                                               
         MVC   0(31,R4),=C'STATION CALL LETTER CHANGE FROM'                     
         LA    R4,33(R4)                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SFXSTA                                                  
         GOTO1 STAPACK,DMCB,(C'U',DUB),WORK,(X'80',(R4))                        
         BRAS  RE,GOBLDFLD                                                      
         B     LD648A              GO CHECK FOR MORE ELEMENTS                   
         DROP  R6                                                               
*                                                                               
LD650    MVI   ELCDLO,X'FE'          LOOK FOR ADWARE CONV ELEM                  
         MVI   ELCDHI,X'FE'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD651                                                            
         XC    ELEM,ELEM                                                        
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),2(R6)                                                    
         BRAS  RE,GOBLDFLD                                                      
         J     LDX                                                              
*                                                                               
LD651    MVI   ELCDLO,MOVELFRQ    LOOK FOR BUY MOVE TO/FROM ELEM                
         MVI   ELCDHI,MOVELTOQ                                                  
         LA    R6,BDELEM                                                        
*                                                                               
LD651A   BRAS  RE,NEXTEL                                                        
         BNE   LD652                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         BAS   RE,DOMOVEL                                                       
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    LD651B                                                           
         BRAS  RE,GOBLDFLD                                                      
         B     LD652                                                            
*                                                                               
LD651B   LA    R4,40(R4)                                                        
         BAS   RE,DOMOVEL                                                       
         BRAS  RE,GOBLDFLD                                                      
         B     LD651A                                                           
*                                                                               
LD652    MVI   ELCDLO,X'94'        LOOK FOR A MC/GM MOVE ELEMENT                
         MVI   ELCDHI,X'94'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LD653                                                            
         MVC   ELEM(80),SPACES                                                  
*                                                                               
         MVC   ELEM(29),=C'SPECIAL MOVE ON MAR01/01 FROM'                       
*                                                                               
         USING BMVELEM,R6                                                       
         GOTO1 VDATCON,DMCB,(2,BMVDATE),(5,ELEM+16)                             
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A15'  GET CLUNPK ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(SVCPROF+6,BMVBUYCL),ELEM+31                           
*                                                                               
         MVC   ELEM+35(3),BMVBUYPR                                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BMVBUYES                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+39(3),DUB                                                   
         MVI   ELEM+42,C'-'                                                     
         IC    R0,BMVBUYLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+43(3),DUB                                                   
         DROP  R6                                                               
         BAS   RE,GOBLDFLD                                                      
LD653    J     LDX                                                              
*                                                                               
         USING MOVELEM,R6                                                       
DOMOVEL  NTR1                                                                   
         MVC   0(7,R4),=C'MOVE TO'                                              
         CLI   0(R6),X'92'   TEST MOVED FROM THIS LINE                          
         BE    *+10                                                             
         MVC   4(4,R4),=C'FROM'                                                 
         LA    R4,9(R4)                                                         
*                                                                               
         MVC   0(3,R4),MOVQCLT                                                  
         MVC   4(3,R4),MOVQPRD                                                  
         SR    R0,R0                                                            
         IC    R0,MOVBEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,R4),DUB                                                      
         MVI   11(R4),C'-'                                                      
         SR    R0,R0                                                            
         IC    R0,MOVLIN                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  12(3,R4),DUB                                                     
         LA    R4,16(R4)                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(2,MOVDATE),(8,(R4))                                
         LA    R4,9(R4)                                                         
*                                                                               
         UNPK  DUB,MOVTIME(3)                                                   
         MVC   0(2,R4),DUB+3                                                    
         MVI   2(R4),C'.'                                                       
         MVC   3(2,R4),DUB+5                                                    
         J     EXIT                                                             
         SPACE 2                                                                
LDAUTH   NTR1                                                                   
         OC    0(2,R5),0(R5)                                                    
         JZ    EXIT                                                             
         XC    WORK(25),WORK                                                    
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(2),SVSECAGY  SET SECURITY AGENCY                          
         CLC   SVSECAGY,SPACES                                                  
         BH    *+10                                                             
         MVC   WORK+1(2),AGYALPHA                                               
         MVC   WORK+23(2),0(R5)                                                 
*                                                                               
LDAUTH1  GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC2                   
         L     R8,AREC2                                                         
         CLC   WORK(25),0(R8)                                                   
         BNE   LDAUTH4                                                          
         LA    RE,28(R8)                                                        
*                                                                               
LDAUTH2  CLC   =X'0318',0(RE)                                                   
         BE    LDAUTH6                                                          
         CLC   =X'C30A',0(RE)       - NEW SECURITY - PERSON ELEMENT             
         BE    LDAUTH3                                                          
****     CLC   =X'030C',0(RE)      WE DON'T NEED TO DO IT THIS WAY              
****     BE    LDAUTH2A               ANYMORE AS C3 ELEM WILL BE THERE          
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   LDAUTH2                                                          
         B     LDAUTH4                                                          
*                                                                               
LDAUTH2A XC    WORK(25),WORK                                                    
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(2),SVSECAGY  SET SECURITY AGENCY                          
         CLC   SVSECAGY,SPACES                                                  
         BH    *+10                                                             
         MVC   WORK+1(2),AGYALPHA                                               
         MVC   WORK+15(10),2(RE)                                                
         B     LDAUTH1                                                          
*                                                                               
LDAUTH3  MVC   ELEM+23(8),2(RE)    SIGN ON ID                                   
         XC    WORK(25),WORK                                                    
         MVI   WORK,C'F'           GET PERSON RECORD                            
         MVI   WORK+1,X'04'                                                     
         MVC   WORK+13(2),SVSECAGY SET SECURITY AGENCY                          
         CLC   SVSECAGY,SPACES                                                  
         BH    *+10                                                             
         MVC   WORK+13(2),AGYALPHA                                              
         MVC   WORK+15(8),2(RE)                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC2                   
         L     R8,AREC2                                                         
         CLC   WORK(23),0(R8)                                                   
         BNE   LDAUTH8                                                          
         LA    RE,28(R8)                                                        
*                                                                               
LDAUTH3A CLI   0(RE),X'C6'                                                      
         BE    LDAUTH3B                                                         
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   LDAUTH3A                                                         
         B     LDAUTH8                                                          
*                                                                               
LDAUTH3B MVC   ELEM+20(2),2(RE)    OFFICE                                       
         B     LDAUTH8                                                          
*                                                                               
LDAUTH4  MVC   ELEM+20(13),=C'** UNKNOWN **'                                    
         B     LDAUTH8                                                          
*                                                                               
LDAUTH6  MVC   ELEM+20(2),2(RE)    OFFICE                                       
         MVC   ELEM+23(18),4(RE)   LAST NAME                                    
         MVC   ELEM+42(1),22(RE)   FIRST INIT                                   
         MVC   ELEM+44(1),23(RE)                                                
*                                                                               
LDAUTH8  DS    0H                                                               
         OC    2(3,R5),2(R5)                                                    
         BNZ   LDAUTH10                                                         
         MVC   ELEM(7),=C'APPR BY'                                              
         B     LDAUTH12                                                         
*                                                                               
LDAUTH10 GOTO1 VDATCON,DMCB,(3,2(R5)),(5,ELEM+10)                               
*                                                                               
LDAUTH12 MVC   DMCB+4(4),=X'D9000A0D'  GET SQUASHER ADDRESS                     
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),ELEM,78                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* DISPLAY LCI LOCKIN VALUES                                                     
*===========================================================                    
         SPACE 1                                                                
DSPLKIN  NTR1  BASE=*,LABEL=*                                                   
         MVC   BLDLIST(4),=X'004C2000'                                          
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         ST    R4,BLDLIST+4                                                     
*                                                                               
         L     R8,AREC                                                          
         USING SLKRECD,R8                                                       
*                                                                               
         LA    R6,SLKFSTEL                                                      
         USING SLKEL,R6                                                         
*                                                                               
         MVC   0(7,R4),=C'UPDATED'                                              
         LA    R5,SLKUPDT                                                       
         OC    0(3,R5),0(R5)                                                    
         BNZ   *+14                                                             
         MVC   0(7,R4),=CL7'ADDED  '                                            
         LA    R5,SLKCRDT                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(3,(R5)),(5,8(R4))                                  
         AHI   R4,20                                                            
*                                                                               
DSPLK2   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),3                                                          
         BNE   DSPLKX                                                           
         USING LOKEL,R6                                                         
*                                                                               
         LA    R4,ELEM+20                                                       
         MVC   0(5,R4),=C'LOCK='                                                
         AHI   R4,5                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(2,LOKWEEK),(3,DUB)                                 
         SR    R0,R0                                                            
         IC    R0,DUB+1            GET MONTH FROM DATE OF WEEK                  
         CLI   DUB+2,15            TEST DAY > 15                                
         BL    DSPLK4              NO - THIS IS THE BRD MON                     
         CHI   R0,12                                                            
         BNE   *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                THEN BRD MON IS NEXT MONTH                   
*                                                                               
DSPLK4   CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C','                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(1,R4),SLKKDPT                                                  
         MVI   1(R4),C','                                                       
         AHI   R4,2                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SLKKLEN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C','                                                       
         AHI   R4,3                                                             
*                                                                               
         ICM   R0,15,LOKDOLS                                                    
         EDIT  (R0),(8,0(R4)),2,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R4,R0                                                            
         AHI   R4,-3               BACK UP TO TO DECIMAL POINT                  
         CLC   =C'.00',0(R4)                                                    
         BE    *+8                                                              
         AHI   R4,3                ELSE LEAVE IT THERE                          
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
         LA    RE,LOKDEM                                                        
         LHI   RF,4                                                             
*                                                                               
DSPLK10  ICM   R0,15,0(RE)                                                      
         EDIT  (R0),(6,0(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         AR    R4,R0                                                            
         AHI   R4,-2               BACK UP TO DECIMAL POINT                     
         CLC   =C'.0',0(R4)                                                     
         BE    *+8                                                              
         AHI   R4,2                                                             
         XC    0(3,R4),0(R4)       CLEAR ANY EXTRA CHARS                        
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
         AHI   RE,4                NEXT DEMO                                    
         BCT   RF,DSPLK10                                                       
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
*                                                                               
         BRAS  RE,GOBLDFLD                                                      
         XC    ELEM,ELEM                                                        
         B     DSPLK2              SEE IF MORE ELEMENTS TO DISPLAY              
*                                                                               
DSPLKX   XIT1                                                                   
         DROP  R6,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* TRANSLATE 1 CHAR BOOKTYPE TO ALPHA                                            
*==================================================================             
                                                                                
GETBKTYP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               SAVE POINTER TO VALUE                        
         MVC   HALF,=C'??'                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
*                                                                               
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
EDITBT4  CLI   0(RF),X'FF'                                                      
         BE    EDITBTX                                                          
*                                                                               
EDITBT6  CLC   0(1,R4),SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     EDITBT4                                                          
*                                                                               
         MVC   HALF(2),SPBKTYPA                                                 
*                                                                               
EDITBTX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* CALL DEMOCON TO GET DEMO NAME FOR 3-BYTE DEMO CODE AT 0(R4)                   
* RETURN DEMO NAME IN DUB                                                       
*=================================================================              
                                                                                
GETDEMNM NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB             CLEAR OUTPUT                                 
*                                                                               
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BE    GETDEMUSR                                                        
         CLI   2(R4),0             TEST NONT DEMO                               
         BE    GETDEMNT                                                         
                                                                                
* GET TRADITIONAL DEMO NAME                                                     
                                                                                
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,0,X'D9000AE0'  GET DEMOCON ADDRESS                  
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(1,0(R4)),(6,DUB),(C'S',ADBLOCK)                       
         J     GETDEMNX                                                         
*                                                                               
GETDEMUSR LLC  RE,2(R4)            GET USER DEMO SEQNUM                         
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         LA    RE,SVUSRNMS(RE)                                                  
         MVC   DUB(7),0(RE)                                                     
         J     GETDEMNX                                                         
*                                                                               
GETDEMNT LLC   RE,1(R4)            GET NONT SEQNUM                              
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         MVC   DUB(7),0(RE)                                                     
*                                                                               
GETDEMNX J   EXIT                                                               
         LTORG                                                                  
         EJECT                                                                  
* DEDBLOCK                                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE SPXBHISTD                                                      
       ++INCLUDE SPGENXLK                                                       
T211FFD  DSECT                                                                  
         ORG   BUYHL1H                                                          
       ++INCLUDE SPBUYF6D                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPBUY22   11/13/19'                                      
         END                                                                    
