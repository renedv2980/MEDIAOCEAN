*          DATA SET SPBUY34    AT LEVEL 009 AS OF 04/10/13                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041861.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T21134A                                                                  
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*=========== THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
         TITLE 'T21134 - NETPAK BUY - NEW BUYS'                                 
T21134   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21134                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21134+4096,R9                                                   
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
* BUILD SKELETON DEMO ELEM                                                      
         XC    BUDEMS,BUDEMS                                                    
         LA    R6,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R6,SVBRDEMS                                                      
         LA    R1,BUDEMS+24                                                     
B10      CLI   1(R6),0                                                          
         BE    B12                                                              
         MVC   0(3,R1),0(R6)                                                    
         LA    R1,8(R1)                                                         
         LA    R6,3(R6)                                                         
         B     B10                                                              
*                                                                               
B12      LA    R0,BUDEMS                                                        
         SR    R1,R0                                                            
         STC   R1,BUDEMS+1         SET ELEM LENGTH                              
         MVI   BUDEMS,2            AND ELEM CODE                                
*                                                                               
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         CLI   BUTRCODE,C'B'       BUY                                          
         BE    B200                                                             
         MVI   ERRCD,TRCDERR                                                    
         B     BUYERR                                                           
         EJECT                                                                  
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
B200     DS    0H                                                               
         MVI   ERRCD,INVERR                                                     
         LA    R0,BUYINP2H                                                      
         CR    R2,R0                                                            
         BNL   BUYERR                                                           
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,3                                                         
         BNE   B300                                                             
         CLC   =C'PKG',0(R4)                                                    
         BNE   B300                                                             
* PACKAGE MASTER BUY                                                            
         MVI   BUSTAT,X'03'        SET NETPAK PKG IND                           
         MVI   EDTVAL,DPTEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,PGMEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,COSTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,C','                                                       
         BE    BUYERR                                                           
* CLEAR BUYREC                                                                  
         LA    R0,8                                                             
         LA    R1,BUYREC                                                        
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
* FIND LINE NUMBER                                                              
         BAS   RE,NXTBUYLN                                                      
* BUILD BUY REC                                                                 
         MVC   BUYRLEN,=H'94'                                                   
         MVC   BUYALPHA,AGYALPHA                                                
*                                                                               
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,70                                                         
         MVC   BDSTART(6),SVSTARTB                                              
         MVI   BDDAY,X'7F'                                                      
         MVI   BDWKIND,C'O'                                                     
         MVC   BDDAYPT,BUDPT                                                    
         MVI   BDSEC,30                                                         
         MVC   BDPROGRM,BUPROG                                                  
* NOTE COST IS IN NTWK SPEC ELEM                                                
         MVI   BDCIND,X'20'        SET GROSS COST                               
*                                                                               
         MVI   BDWHY,X'80'                                                      
         MVI   BDSTAT,X'03'                                                     
         GOTO1 SETCHGDT                                                         
* CREATE X'94' ELEM                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'94'                                                       
         MVI   ELEM+1,36                                                        
         MVC   ELEM+16(4),BUCOST   SET COST (4 BYTES)                           
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *-10                                                             
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDR                               
*                                                                               
         MVC   BUYMSG(33),=C'** BUY ADDED. NOTE LINE NUMBER **'                 
         MVI   RCLOPT,0                                                         
*                                                                               
B250     GOTO1 CALLDSP                                                          
*                                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* NETPAK BUY                                                                    
*                                                                               
B300     DS    0H                                                               
*                                                                               
* FIRST MUST CHECK FOR REF ON NEXT LINE                                         
*                                                                               
         XC    FLEN,FLEN           SET TO RE-EDIT NEXT TIME                     
         ST    R2,WORK2            SAVE R2                                      
         MVC   WORK2+4(18),FADDR   AND FLDVAL TABLE                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   ERRCD,NOPKGREF                                                   
         CLI   5(R2),0                                                          
         BE    BUYERR                                                           
* SET UP TO EDIT REF                                                            
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         MVI   FSTOPS+1,C'='                                                    
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'C'                                                       
         BNE   BUYERR                                                           
         GOTO1 FLDVAL              SET UP FOR REFEDT                            
*                                                                               
         MVI   EDTVAL,REFEDT                                                    
         GOTO1 CALLEDT                                                          
* TEST MASTER LINE ON FILE                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+11(1),BUREFMAS+1                                             
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         LA    R5,REC                                                           
         ST    R5,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   BUADJ,BDDAYPT       SAVE DAYPART CODE                            
         MVC   BUCIND,BDCIND       SAVE COST TYPE                               
*                                                                               
         CLI   BUREFTYP,8          TEST MAKE-GOOD                               
         BE    B301                                                             
         CLI   BUREFTYP,2          TEST PKG=                                    
         BNE   B304                                                             
         MVI   ERRCD,NNETMSTR                                                   
         TM    BDSTAT,X'02'        TEST MSTR                                    
         BZ    BUYERR                                                           
         B     B304                                                             
         EJECT                                                                  
* MAKE SURE MADE-GOOD SPOT EXISTS                                               
*                                                                               
B301     MVI   ERRCD,BADSPOT                                                    
         XC    ELEMDT,ELEMDT                                                    
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
B302     BAS   RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
*                                                                               
         CLC   ELEMDT,BUMGDATE                                                  
         BNE   B302                                                             
         CLC   ELEMNO,BUMGSPOT                                                  
         BNE   B302                                                             
         MVI   ERRCD,NOMGUNAL                                                   
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    BUYERR                                                           
         MVI   ERRCD,NO2FOR1                                                    
         TM    6(R6),X'02'         TEST MAKE-GOOD ON NEW LINE                   
         BO    BUYERR                                                           
*                                                                               
B304     DS    0H                                                               
         LA    R6,BDELEM                                                        
B305     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'94'                                                      
         BNE   B305                                                             
         MVI   ERRCD,PKGLOCK                                                    
         TM    2(R6),X'20'                                                      
         BO    BUYERR                                                           
         MVC   BUEXPDTA(36),0(R6)  SAVE X'94' ELEMENT                           
         EJECT                                                                  
* RESTORE FLDVAL TABLE AND REG                                                  
         L     R2,WORK2                                                         
         MVC   FADDR(18),WORK2+4                                                
* EDIT NETPAK BUY INPUT                                                         
         MVI   BUSTAT,X'01'                                                     
         MVI   EDTVAL,PEREDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,NPWEDT                                                    
         GOTO1 CALLEDT                                                          
         CLI   BUNPW,0                                                          
         BNE   *+16                                                             
         MVI   ERRCD,INVERR                                                     
         CLI   BUWKIND,C'O'                                                     
         BNE   BUYERR                                                           
         MVI   EDTVAL,SLNEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   EDTVAL,PCDEDT                                                    
         GOTO1 CALLEDT                                                          
         CLI   FSTOP,C','                                                       
         BNE   B318                                                             
* COST IS OPTIONAL                                                              
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLC   =C'M=',1(R4)                                                     
         BE    B316                                                             
         MVI   EDTVAL,COSTEDT                                                   
         GOTO1 CALLEDT                                                          
* CHECK FOR OPTIONAL MASPRD                                                     
B316     CLI   FSTOP,C','                                                       
         BNE   B318                                                             
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADCOMMA                                                   
         CLC   =C'M=',0(R4)                                                     
         BNE   BUYERR                                                           
* EDIT MASPRD DATA                                                              
         XC    BUELDATA,BUELDATA                                                
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,BADMAS                                                     
         TM    BUELPRSW,X'E7'      TEST ANY BITS BUT FREE RIDER                 
         BNZ   BUYERR                                                           
         CLI   BUELPRD,0      M=UNALL NOT VALID                                 
         BE    BUYERR                                                           
*                                                                               
B318     MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,C','                                                       
         BE    BUYERR                                                           
* CLEAR BUYREC                                                                  
         LA    R0,8                                                             
         LA    R1,BUYREC                                                        
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
* FIND LINE NUMBER                                                              
         BAS   RE,NXTBUYLN                                                      
* BUILD BUY REC                                                                 
         MVC   BUYRLEN,=H'94'                                                   
         MVC   BUYALPHA,AGYALPHA                                                
*                                                                               
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,70                                                         
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDSTART(6),BUSTARTB                                              
         MVC   BDWKIND,BUWKIND                                                  
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDNOWK,BUNPW                                                     
         CLI   BDNOWK,0                                                         
         BNE   *+10                                                             
         MVC   BDWKS,BUWKS         SET NUMBER OF WEEKS                          
         MVC   BDDAYPT,BUADJ       DAYPART SAVED HERE                           
         MVC   BDSEC,BUSLN                                                      
         MVC   BDTIMST(4),BUTIME                                                
         MVC   BDCOST(4),BUCOST                                                 
         L     RE,AREC3            SAVE AREA FOR PGM ELEM                       
         MVC   BDPROGRM(16),7(RE)      MOVE PGM NAME                            
         MVC   7(16,RE),BUPROG     AND SET PCD IN SAVED ELEM                    
         MVC   BDCIND,BUCIND                                                    
         MVI   BUWHY,X'80'                                                      
         GOTO1 SETCHGDT                                                         
         MVC   BDSTAT,BUSTAT                                                    
*                                                                               
         MVC   BDMASPRD,BUELPRD                                                 
         MVI   BDELLEN,10                                                       
         CLI   BDMASPRD,0                                                       
         BE    B320                                                             
         MVI   BDELLEN,14                                                       
         CLI   BDMASPRD+1,0                                                     
         BE    B320                                                             
         MVI   BDELLEN,18                                                       
* INSERT DEMO ELEM                                                              
B320     LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,BUYREC,BUDEMS,(R6)                                   
         AR    R6,R0                                                            
*                                                                               
         GOTO1 VBLDEL                                                           
         BAS   RE,CALEND                                                        
         BAS   RE,CHKMAXEL                                                      
         EJECT                                                                  
* ADD PROGRAM/HUT ELEMS (SAVED IN REC 3)                                        
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *-10                                                             
         L     R4,AREC3                                                         
         CLI   80(R4),X'90'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,BUYREC,80(R4),(R6)                                   
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R4),X'92'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,(R4),(R6)                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
* ADD X'94' ELEM                                                                
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'94'                                                       
         MVI   ELEM+1,36                                                        
         TM    BUEXPDTA+2,X'80'    TEST PKG FROZEN                              
         BZ    *+8                                                              
         MVI   ELEM+2,X'C0'        SET PFB/FRZN INDS                            
         TM    BUEXPDTA+2,X'04'    TEST INTG RATE TYPE                          
         BZ    *+8                                                              
         OI    ELEM+2,X'04'                                                     
         MVC   ELEM+5(3),BUEXPDTA+5  SET INTG COST                              
         IC    RE,BUEXPDTA+1       USE EX CODE TO HANDLE                        
         SH    RE,=H'9'             OLD-STYLE, SHORTER ELEMENTS                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),BUEXPDTA+8                                             
         GOTO1 (RF),(R1),,ELEM,(R6)                                             
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDRESS                            
         MVC   BUYMSG(33),=C'** BUY ADDED. NOTE LINE NUMBER **'                 
*                                                                               
         MVI   RCLOPT,0                                                         
         CLI   BDNOWK,0            IF NPW=0, DON'T DISPLAY ROTATION             
         BE    B330                                                             
         TM    SVOPTS,X'02'        DID USER SAY NO ROT                          
         BO    B330                                                             
         MVI   RCLOPT,RCLROT       DISPLAY ROTATION                             
*                                                                               
B330     B     B250                                                             
         EJECT                                                                  
* FIND NEXT AVAILABLE LINE NUMBER.                                              
* SVKEY HAS A-M/CLT/PRD/MKT-STA/EST                                             
* RETURN NEW KEY IN SVKEY AND SET KEY IN BUYREC                                 
*                                                                               
NXTBUYLN NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(11),SVKEY       USE HIGHEST LINE SO FAR IF ANY               
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         B     NXTBUY4                                                          
*                                                                               
NXTBUY2   MVC   KEYSAVE,KEY                                                     
         GOTO1 SEQ                                                              
*                                                                               
NXTBUY4  TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(11),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    NXTBUY2                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         IC    RE,KEY+11                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+11                                                        
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
*                                                                               
         MVI   ERRCD,MAXLINES                                                   
         CLI   KEYSAVE+11,X'FF'                                                 
         BE    BUYERR                                                           
*                                                                               
         MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(1),KEY+11                                              
         MVI   BUYKEY+11,1                                                      
         NI    DMINBTS,X'F7'       UNSET 'PASS DELETES'                         
         B     EXIT                                                             
         EJECT                                                                  
CHKMAXEL NTR1                                                                   
         MVI   ERRCD,MAXELEMS                                                   
         MVI   ELCDLO,X'0B'                                                     
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         MVI   ELCDLO,X'06'                                                     
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         LA    R7,75               MAX 75 REGELS                                
         BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         BCT   R7,*-8                                                           
         B     BUYERR                                                           
         EJECT                                                                  
* CREATE REGELEMS FOR ALL WEEKS IN BUY PERIOD                                   
*                                                                               
CALEND   NTR1                                                                   
         CLI   BDNOWK,0                                                         
         BE    EXIT                                                             
* FIND LAST ELEM                                                                
         LA    R6,BDELEM                                                        
         SR    R7,R7               CLEAR COUNTER                                
CAL4     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BNE   CAL4                                                             
* GET EBCDIC START/END DATES                                                    
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
*                                                                               
         LA    RE,7                                                             
         CLI   BDWKIND,C'O'                                                     
         BE    CAL6                                                             
         LA    RE,14                                                            
         CLI   BDWKIND,C'A'                                                     
         BE    CAL6                                                             
         LA    RE,21                                                            
         CLI   BDWKIND,C'T'                                                     
         BE    CAL6                                                             
         LA    RE,28                                                            
         CLI   BDWKIND,C'F'                                                     
         BE    CAL6                                                             
         DC    H'0'                                                             
CAL6     ST    RE,FULL             SAVE NUMBER OF DAYS BETWEEN SPOTS            
*                                                                               
CAL8     GOTO1 VDATCON,DMCB,WORK,(2,ELEM+2)                                     
         LA    R7,1(R7)                                                         
*                                                                               
         LA    R0,1                NON-POL GETS 1 ELEM/WEEK                     
         CLI   ELEM,X'06'                                                       
         BE    *+8                                                              
         IC    R0,BDNOWK           POL GETS 1 ELEM PER SPOT                     
*                                                                               
CAL10    GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         BCT   R0,CAL10            LOOP FOR NUMBER OF ELEMENTS                  
*                                                                               
CAL11    L     R0,FULL                                                          
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         CLC   WORK+6(6),WORK+12   TEST PAST END                                
         BH    CAL12                                                            
         MVC   WORK(6),WORK+6                                                   
         TM    BDSTAT,X'20'        TEST NETPAK AVG DATA                         
         BZ    CAL8                                                             
         LA    R7,1(R7)            BUMP COUNTER                                 
         B     CAL11               GEN FIRST WEEK ONLY                          
*                                                                               
CAL12    STC   R7,BDWKS            SET NUMBER OF WEEKS IN BDELEM                
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
 END                                                                            
