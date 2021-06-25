*          DATA SET SPREQ04    AT LEVEL 253 AS OF 02/26/20                      
*PHASE T20804A                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE PERVAL                                                                 
         TITLE 'SPREQ04 - REQUEST - VALIDATE DATA FIELDS - PART 2'              
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-34460  04/03/19 DO NOT FLAG Z5 AS COMSCORE                *         
***********************************************************************         
T20804   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,T20804,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LA    RA,T20804+4095                                                   
         LA    RA,1(RA)                                                         
         USING T20804+4096,RA         NOTE:2 BASE REGISTERS RB,RA               
         EJECT                                                                  
         SR    RF,RF                                                            
         IC    RF,ROUTNUM                    RF=ROUTINE NUMBER                  
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO                       RF=A(VALIDATION ROUTINE)           
         BASR  RE,RF                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
REPVAL   NTR1                                                                   
         BRAS  RE,REPVL                                                         
         J     XIT                                                              
*                                                                               
DTEVAL   NTR1                                                                   
         BRAS  RE,DTEVL                                                         
         J     XIT                                                              
*                                                                               
BOKVAL   NTR1                                BOOK-HUT - FIND BITS               
         MVI   ROUTSUB,1                     04=YYMM 08=YYMM-XX                 
         GOTO1 AINITV                        EXTRACT 1ST PART                   
         MVC   TEMP(4),IFLD                                                     
         CLI   5(R4),0                                                          
         BE    BOKVO                         BOOK-HUT NOT INPUT                 
                                                                                
                                                                                
         CLC   =C'PUT/',IFLD     'PUT/MMMYY,BK=MMMYY' ?                         
         BNE   BOKVAL00                                                         
         CLC   =C'D8',RNUM       ONLY VALID FOR D8                              
         BE    BKVV                                                             
         CLC   =C'PM',RNUM       ONLY VALID FOR PR                              
         BE    BKVV                                                             
         CLC   =C'M2',RNUM       ONLY VALID FOR M2                              
         BNE   BOKVE                                                            
* CHECK IF INDEX VALUE ENTERED 'PUT/MMMYY/NN,BK=MMMYY'                          
BKVV     CLI   IFLDH+5,9                                                        
         BNH   NOINDEX                                                          
         GOTO1 SCANNER,PLIST,IFLDH,(5,SPTREC+500),C',=//'                       
         LLC   R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BZ    NOINDEX                                                          
         LA    R1,SPTREC+500                                                    
         LA    R1,64(R1)           BUMP TO INDEX VALUE                          
         TM    2(R1),X'80'         NUMERIC?                                     
         BZ    INDXERR                                                          
         CLC   4(4,R1),=X'00000041'    64 <INDEX>256                            
         BL    INDXERR                                                          
         CLC   4(4,R1),=X'000000FF'                                             
         BH    INDXERR                                                          
         MVC   RCARD2+75(1),7(R1)     SET BINARY INDEX VALUE                    
         B     INDEXOK                                                          
*                                                                               
INDXERR  MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** 64<INDEX>265 *'                                  
         B     BOKVO                                                            
                                                                                
* NOW FUDGE IFLD TO IGNORE INDEX (/NN) AT END OF IFLD                           
INDEXOK  LLC   R2,0(R1)            GET LENGTH OF INDEX                          
         LA    R2,1(R2)            +1 FOR /                                     
         LLC   R1,IFLDH+5          TOTAL IFLD LENGTH                            
         SR    R1,R2               MINUS LENGTH OF INDEX                        
         STC   R1,IFLDH+5          AND SET IN IFLD                              
                                                                                
NOINDEX  MVI   RNUM+55,C'U'        SET U FOR RERATE                             
***************************************************                             
* BUILDS HEADER + DATA IN TEMP FOR BOOKVAL                                      
* CHECK IF (N) ANT END OF PUT/ FIELDOOKVAL                                      
                                                                                
         DS    0H                                                               
         LLC   R1,IFLDH+5          GET LENGTH OF INPUT                          
         SHI   R1,4                MINUS LENGTH OF 'PUT/'                       
         BCTR  R1,0                FOR EXECUTE MOVE                             
         LTR   R1,R1                                                            
         BM    BOKVE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+8(0),IFLD+4      SET DATE TO TEMP+8 AND ADD ','             
         LA    R1,1(R1)            +1 FOR EX MOVE                               
         LA    R2,TEMP+8                                                        
         AR    R2,R1                                                            
         MVI   0(R2),C','          SET ',' AT END OF DATA                       
         LA    R1,1(R1)            +1 FOR COMMA                                 
         LR    R2,R1               R2-> LENGTH OF DATA IN TEMP                  
*                                                                               
* EXTRACT 2ND PART - MUST BE 'BK=MMMYY'                                         
         MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLC   =C'BK=',IFLD                                                     
         BNE   BOKVE                                                            
***************************************************                             
         LLC   R1,IFLDH+5          GET LENGTH OF INPUT                          
         LA    R3,IFLD             CHCK IF (A) AT END OF IFLD                   
BKVV05   CLI   0(R3),C'('                                                       
         BNE   BKVV07                                                           
         CLI   1(R3),C'O'          MUST BE O                                    
         BNE   BOKVE                                                            
         MVI   RCARD2+74,C'O'      SET TO RCARD2 COL72                          
         XC    0(3,R3),0(R3)       CLEAR FROM IFLD                              
         LLC   R1,IFLDH+5                                                       
         SHI   R1,3                AND FROM INPUT LENGTH                        
         STC   R1,IFLDH+5          ADJUST HEADER LENGTH                         
         B     BKVV10                                                           
BKVV07   LA    R3,1(R3)                                                         
         BCT   R1,BKVV05                                                        
***************************************************                             
BKVV10   LLC   R1,IFLDH+5          LENGTH OF INPUT                              
         SHI   R1,3                MINUS LENGTH OF 'BK='                        
         BCTR  R1,0                FOR EX MOVE                                  
         LTR   R1,R1                                                            
         BM    BOKVE                                                            
         LA    R3,TEMP+8           FUDGED HEADER FIELD                          
         AR    R3,R2               R2 LENGTH OF 1ST DATA                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),IFLD+3      DATES IN TEMP SEPARATED BY ','               
         LA    R1,1(R1)            REPLACE -1                                   
         AR    R2,R1               R2-> LENGTH OF DATA IN TEMP+8                
* FUDGE TEMP AS FIELD HEADER                                                    
         XC    TEMP(8),TEMP                                                     
         STC   R2,TEMP+5           SET LENGTH OF DATA                           
         LA    R2,8(R2)                                                         
         STC   R2,TEMP             HEADER + DATA                                
*                                                                               
         GOTO1 BOOKVAL,DMCB,(C'N',TEMP),(2,TEMP+20),(C'B',SCANNER),HALF         
         CLI   4(R1),2            BOTH FIELDS MUST BE VALID                     
         BNE   BOKVE                                                            
*                                                                               
         MVI   TEMP+2,1                                                         
         MVC   TEMP(2),TEMP+24                   2ND DATE                       
         GOTO1 DATCON,DMCB,(3,TEMP),(0,TEMP+3)                                  
         MVC   RCARD2+70(4),TEMP+3                                              
*                                                                               
         MVC   TEMP(2),TEMP+21                    1ST DATE                      
         GOTO1 DATCON,DMCB,(3,TEMP),(0,IFLD)                                    
         XC    IFLD+4(2),IFLD+4                                                 
         OI    FIND,X'04'                                                       
         B     BOKVO                                                            
****************************************************************                
BOKVAL00 CLI   IFLD+5,C'/'        TEST MMMDD/N                                  
         BE    BOKVE              IF YES/INVALID                                
         CLI   IFLDH,1                                                          
         BNE   BOKVE                         1ST PART MISSING                   
**       CLI   IFLDH+5,3                                                        
**       BNE   BOKV                                                             
         CLC   IFLD(3),=C'ACT'                                                  
         BNE   *+12                                                             
         CLI   IFLDH+5,3                                                        
         BNE   BOKVE                                                            
         BE    BOKV0                                                            
         CLC   IFLD(4),=C'LATE'                                                 
         BE    BOKV0                                                            
****     B     BOKVE                                                            
BOKV     GOTO1 DATVAL,PLIST,(2,IFLD),TEMP                                       
         OC    PLIST(4),PLIST                                                   
         BE    BOKVE                                                            
BOKV0    MVI   ROUTSUB,2                     EXTRACT 2ND PART                   
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BH    BOKVE                                                            
         BE    BOKV1                                                            
         CLC   TEMP(3),=C'ACT'                                                  
         BE    *+14                                                             
         CLC   TEMP(4),=C'LATE'                                                 
         BNE   BOKV0A                                                           
         OI    FIND,X'10'               ACT                                     
         MVC   IFLD(4),TEMP        RESTORE INPUT FOR BOKVO                      
         B     BOKVO                                                            
*                                                                               
BOKV0A   OI    FIND,X'04'                    BOOK-HUT = YYMM                    
         B     BOKV2                                                            
BOKV1    CLI   IFLDH+5,2                                                        
         BNE   BOKVE                                                            
         CLC   TEMP(3),=C'ACT'                                                  
         BE    *+14                                                             
         CLC   TEMP(4),=C'LATE'                                                 
         BNE   BOKV1A                                                           
         OI    FIND,X'20'               ACT-XX                                  
*                                                                               
         CLI   IFLD,C'B'           IS IT BN(N=1-4) INPUT                        
         BNE   BOKV1B                                                           
         CLI   IFLD+1,C'1'                                                      
         BL    BOKVE                                                            
         CLI   IFLD+1,C'4'                                                      
         BH    BOKVE                                                            
         OI    FIND,X'0C'                                                       
         MVC   IFLD+4(2),IFLD                                                   
         MVC   IFLD(4),TEMP                                                     
         BAS   RE,ALLOWED                                                       
         B     BOKVO                                                            
*                                                                               
*****    B     BOKV1B                                                           
BOKV1A   OI    FIND,X'08'                    BOOK-HUT = YYMM-XX                 
*                                                                               
         CLI   IFLD,C'B'           IS IT BN(N=1-4) INPUT                        
         BNE   BOKV1B                                                           
         CLI   IFLD+1,C'1'                                                      
         BL    BOKVE                                                            
         CLI   IFLD+1,C'4'                                                      
         BH    BOKVE                                                            
         OI    FIND,X'04'                                                       
         BAS   RE,ALLOWED                                                       
         B     BOKV2                                                            
*                                                                               
BOKV1B   CLC   IFLD(2),=C'NO'                                                   
         BE    BOKV2                                                            
         MVC   HALF,=C'0000'                 CHECK FOR NUMERIC 01 TO 12         
         MVZ   HALF,IFLD                                                        
         CLC   HALF,=C'0000'                                                    
         BNE   BOKVE                                                            
         CLC   IFLD(2),=C'01'                                                   
         BL    BOKVE                                                            
         CLC   IFLD(2),=C'12'                                                   
         BH    BOKVE                                                            
BOKV2    MVC   IFLD+4(2),IFLD                REBUILD INPUT                      
         MVC   IFLD(4),TEMP                                                     
         TM    CLISAVE,X'62'       IF CLIENT=ALL, CLTGRP OR OFFICE LST          
         BNZ   BOKVO               BYPASS TEST                                  
         CLC   RNUM,=C'Z5'         IF Z5 REPORT                                 
         BE    BOKVO               BYPASS TEST                                  
         CLC   RNUM,=C'Z7'         IF Z7 REPORT                                 
         BE    BOKVO               BYPASS TEST                                  
         CLC   RNUM,=C'M6'         ...IF M6 REPORT                              
         BNE   *+12                                                             
         CLI   RMED,C'R'           ...AND RADIO                                 
         BE    BOKVO               ...BYPASS TEST                               
         CLC   RNUM,=C'M4'         ...IF M4 REPORT                              
         BNE   *+12                                                             
         CLI   RMED,C'R'           ...AND RADIO                                 
         BE    BOKVO               ...BYPASS TEST                               
         CLC   RNUM,=C'm4'         ...IF m4 REPORT                              
         BNE   BOKV2B                                                           
         CLI   RMED,C'R'           ...AND RADIO                                 
         BE    BOKVO               ...BYPASS TEST                               
BOKV2B   CLI   CLIPROF+3,0        RATING SERVICE MUST BE SPECIFIED              
         BE    BOKVE              NO - ERROR                                    
*                                                                               
         TM    FIND,X'30'        ACT INPUT SO BYPASS READ                       
         BNZ   BOKVO                                                            
*                                                                               
         MVC   KEYD,KEY            SAVE KEY                                     
         MVC   TEMP+30(6),TEMP     SAVE YYMM                                    
*                                  SINCE ARSPT USES TEMP(13)                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGY        READ AGY HEADER                              
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    BOKVO               DISK ERROR                                   
         BH    *+6                                                              
         DC    H'0'                MUST FIND AGYHEADER                          
         MVC   CANAD,SPTREC+104      AGYPROF+7                                  
         MVC   KEY,KEYD            RESTORE KEY                                  
         MVC   TEMP(6),TEMP+30     RESTORE FOR DTCNV                            
         XC    KEYD,KEYD                                                        
         GOTO1 DATCON,PLIST,(0,TEMP),(3,KEYD+3)                                 
         MVI   KEYD+5,0               ZERO DAY                                  
         XC    KEYD+3(2),=X'FFFF'            COMPLEMENT OF YM                   
         MVI   KEYD,C'M'                                                        
         MVI   KEYD+1,C'R'                                                      
         TM    REQMED1,X'40'                                                    
         BNZ   BOKV2C                                                           
         MVI   KEYD+1,C'C'                                                      
         CLI   CANAD,C'C'                                                       
         BE    BOKV2C                                                           
         MVI   KEYD+1,C'T'                                                      
         B     BOKV2F                                                           
*                                                                               
BOKV2C   DS    0H                                                               
         CLI   CLIEXTRA,C'U'       SEE IF CLT USES U.S. DEMOS                   
         BNE   BOKV2F              NO                                           
         MVI   KEYD+1,C'T'         RESET TO U.S.                                
BOKV2F   DS    0H                                                               
         MVI   KEYD+2,C'N'                                                      
         CLI   CLIPROF+3,C'0'                                                   
         BE    BOKV2Z                                                           
         MVI   KEYD+2,C'A'                                                      
                                                                                
         CLI   KEYD+1,C'T'                                                      
         BNE   BOKV2Z                                                           
         CLC   =C'94',TEMP         IF 94 OR GREATER                             
         BH    BOKV2Z                                                           
         MVI   KEYD+2,C'N'         USE NIELSEN                                  
                                                                                
BOKV2Z   GOTO1 ARDEM                         READ RATING RECORD                 
         CLC   FERN,=AL2(FE)                                                    
         BL    BOKVO                         DISK ERROR                         
         BH    *+14                                                             
BOKV3    MVC   FERN,=AL2(BOKNOF)                   BOOK NOT ON FILE             
         B     BOKVO                                                            
         CLC   SPTREC(5),TEMP          TEMP HAS KEY NOW                         
         BNE   BOKV3                                                            
*                          BYPASS HUT ADJUSTMENT READ                           
BOKV4    B     BOKVO                                                            
*                                                                               
BOKVE    MVC   FERN,=AL2(BOKINV)                 BOOK-HUT INVALID               
BOKVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
*                                                                               
         MVI   COMSCORE,C'N'       SET NOT A COMSCORE REQUEST                   
         CLI   CANAD,C'C'                                                       
         BE    BOKVX                                                            
*                                                                               
         CLI   RMED,C'T'                                                        
         JNE   BOKVX                                                            
         CLC   RNUM,=C'Z5'         Z5 REPORT?                                   
         JE    BOKVX               YES - DON'T FLAG AS COMSCORE                 
*                                                                               
         GOTO1 =A(CHKCOMSC),RR=RELO   SEE IF COMSCORE SOON REQUEST              
BOKVX    B     XIT                                                              
*                                                                               
ALLOWED  CLC   RNUM,=C'D2'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'D3'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'D4'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'D5'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'D8'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'M2'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'M3'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'M4'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'm4'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'M6'                                                      
         BER   RE                                                               
         CLC   RNUM,=C'M7'                                                      
         BER   RE                                                               
         B     BOKVE                                                            
*                                                                               
CANAD    DS    CL1                 C IF CANADIAN AGENCY                         
         EJECT                                                                  
PCTVAL   BRAS  RE,PCTVL                      PERCENTAGE - FIND BITS             
         B     XIT                                                              
         EJECT                                                                  
AMTVAL   BRAS  RE,AMTVL                                                         
         B     XIT                                                              
         EJECT                                                                  
ONEVAL   NTR1                                ONE CHR INPUT - FIND BITS          
         GOTO1 AINITV                        04 = X                             
         CLI   IFLDH+5,1                                                        
         BH    ONEVE                                                            
         LH    R7,COLNUM                     FIND REQNUM/COLNUM ENTRY           
         LA    R7,1(R7)                                                         
         STC   R7,FULL+2                                                        
         MVC   FULL(2),REQNUM                                                   
         L     R7,=A(ONEVTBL)      GET ADDR OF ONEVTABLE                        
         A     R7,RELO                                                          
*                                                                               
         SR    R2,R2                                                            
ONEV1    IC    R2,0(R7)                                                         
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(3,R7),FULL                                                     
         BE    ONEV2                                                            
         AR    R7,R2                                                            
         B     ONEV1                                                            
ONEV2    LR    R5,R7                         SAVE ENTRY ADDR                    
         CLI   IFLDH,0                       SET DEFAULT IF NO INPUT            
         BNE   ONEV3                                                            
         CLI   4(R7),C'+'                                                       
         BNE   *+8                                                              
         LA    R7,1(R7)                                                         
         MVC   IFLD,4(R7)                                                       
         B     ONEVO                                                            
ONEV3    SHI   R2,4                          SEARCH VALUE LIST(S)               
ONEV4    CLI   4(R7),C'+'                                                       
         BNE   ONEV5                                                            
         TM    REQFMT,DDSFMT                 END OF STD LIST                    
         BZ    ONEVE                         ERROR NOT DDS TERMINAL             
         B     ONEV6                                                            
ONEV5    CLC   4(1,R7),IFLD                                                     
         BE    ONEV7                                                            
ONEV6    LA    R7,1(R7)                                                         
         BCT   R2,ONEV4                                                         
         B     ONEVE                                                            
ONEV7    OI    FIND,X'04'                    CHR = X                            
         B     ONEVO                                                            
ONEVE    MVC   FERN,=AL2(ONEINV)                   INVALID CHR                  
ONEVO    SR    R2,R2                                                            
         IC    R2,0(R5)                      R2=L'TBL ENTRY                     
         AR    R5,R2                         R5=A(NEXT TBL ENTRY)               
         CLI   1(R5),X'FF'                   SYNONYM ENTRY                      
         BNE   *+12                          NO                                 
         AR    R7,R2                         YES REPLACE INPUT                  
         MVC   IFLD(1),4(R7)                                                    
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
         B     XIT                                                              
         EJECT                                                                  
         DS    0H                                                               
         EJECT                                                                  
*****************************************************************               
*  NEXT TWO ROUTINES ARE DISTINCT.  FIELDS DO NOT EXSIST ANYMORE                
*  I HAVE TO KEEP THEM SO I DON'T SCREW UP TABLE SEQUENCE                       
*                                                                               
PDIVVAL  NTR1                                PROCESS BY DIVISION                
         GOTO1 AINITV                        FIND BITS 04=Y/N                   
*        MVI   RDIV,C'N'                                                        
*        CLI   IFLDH,1                                                          
*        BL    PDIVVX                        PDIV NOT INPUT                     
*        CLI   IFLDH+5,1                                                        
*        BNE   PDIVVE                                                           
*        CLI   IFLD,C'Y'                                                        
*        BNE   PDIVV2                                                           
*        TM    CLIPROF+6,X'01'               OK FOR CLIENT                      
*        BO    *+14                                                             
*        MVC   FERN,=AL2(PDVCLI)                                                
*        B     PDIVVX                                                           
*        TM    PROSAVE,X'12'                 OK FOR PRODUCT (ALL/NO)            
*        BZ    PDIVVE                                                           
PDIVV1   OI    FIND,X'04'                    PDIV = Y/N                         
         MVC   RDIV,IFLD                                                        
*        B     PDIVVX                                                           
*DIVV2   CLI   IFLD,C'N'                                                        
*        BE    PDIVV1                                                           
*DIVVE   MVC   FERN,=AL2(PDVINV)                   PDIV INVALID                 
PDIVVX   B     XIT                                                              
         EJECT                                                                  
PDISVAL  NTR1                                PROCESS BY DISTRICT                
         GOTO1 AINITV                        FIND BITS 04=Y/N                   
*        MVI   RDIST,C'N'                                                       
*        CLI   IFLDH,1                                                          
*        BL    PDISVX                        PDIS NOT INPUT                     
*        CLI   IFLDH+5,1                                                        
*        BNE   PDISVE                                                           
*        CLI   IFLD,C'Y'                                                        
*        BNE   PDISV2                                                           
*        TM    CLIPROF+6,X'02'               OK FOR CLIENT                      
*        BO    *+14                                                             
*        MVC   FERN,=AL2(PDSCLI)                                                
*        B     PDISVX                                                           
*        TM    MKTSAVE,X'08'                 OK FOR MARKET (NON SPEC)           
*        BO    PDISVE                                                           
PDISV1   OI    FIND,X'04'                    PDIS = Y/N                         
         MVC   RDIST,IFLD                                                       
*        B     PDISVX                                                           
*DISV2   CLI   IFLD,C'N'                                                        
*        BE    PDISV1                                                           
*DISVE   MVC   FERN,=AL2(PDSINV)                   PDIS INVALID                 
PDISVX   B     XIT                                                              
*                                                                               
*****************************************************************               
         EJECT                                                                  
DEMVAL   NTR1                          REPORT DEMOS                             
         XC    DEMS,DEMS                                                        
         LA    R7,DEMS                                                          
         LA    R1,1                                                             
         STC   R1,ROUTSUB                                                       
*                     FIND BITS  04= 0-2 NNN'S , 08= 0-4 NNN'S                  
         LA    R0,4                                                             
DEMV1    GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BE    DEMV2                                                            
         CLI   IFLDH+5,3                                                        
         BH    DEMVE                                                            
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    DEMVE         NON NUMERIC                                        
         PACK  DUB,TEMP+1(3)                                                    
         CP    DUB,=P'0'                                                        
         BNH   DEMVE                                                            
         CP    DUB,=P'127'                                                      
         BH    DEMVE                                                            
         CVB   R2,DUB                                                           
         STC   R2,0(R7)                                                         
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ROUTSUB                                                       
         BCT   R0,DEMV1                                                         
*                                                                               
         MVI   ROUTSUB,5                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BNE   DEMVE           NO MORE INPUT ALLOWED                            
         B     DEMV2                                                            
*                                                                               
DEMVE    MVC   FERN,=AL2(DEMINV)                                                
         B     DEMVX                                                            
*                                                                               
DEMV2    OC    DEMS(2),DEMS                                                     
         BZ    DEMVX                                                            
         OI    FIND,X'04'                                                       
         OC    DEMS+2(2),DEMS+2                                                 
         BZ    DEMV3                                                            
         MVI   FIND,X'09'              3 OR 4 DEMOS INPUT                       
         B     DEMV3                                                            
*                                                                               
DEMV3    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),DEMS                                                     
         MVI   R3037,X'FF'                                                      
         CLI   RNUM,C'0'           NEW REPORTS                                  
         BNL   DEMVX                                                            
         MVI   R3037,C'Y'                                                       
*                                                                               
DEMVX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
MSEQVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         MVI   RDIST,C'N'                                                       
         CLI   IFLDH,1                                                          
         BNE   MSEQVX                                                           
         CLI   IFLDH+5,3                                                        
         BH    MSEQV2                                                           
         BL    MSEQVE           ERROR                                           
         CLC   IFLD(3),=C'DST'                                                  
         BE    MSEQDST                                                          
         CLC   IFLD(3),=C'REG'                                                  
         BNE   MSEQVE           ERROR                                           
         TM    MKTSAVE,X'02'        MARKET MUST BE ALL                          
         BNO   MSEQVE                                                           
         OI    FIND,X'04'       MSEQ=REG                                        
         MVI   RDIST,C'G'                                                       
         B     MSEQVX                                                           
*                                                                               
MSEQDST  TM    CLIPROF+6,X'02'        SEE IF OK IN CLIENT PROFILE               
         BO    *+14                                                             
         MVC   FERN,=AL2(PDSCLI)                                                
         B     MSEQVX                                                           
         TM    MKTSAVE,X'08'           MARKET MUST BE NON-SPECIFIC              
         BO    MSEQVE        ERROR                                              
         OI    FIND,X'02'              MSEQ=DST                                 
         MVI   RDIST,C'Y'                                                       
         B     MSEQVX                                                           
*                                                                               
MSEQV2   CLI   IFLDH+5,5                                                        
         BNE   MSEQVE       ERROR                                               
         CLC   IFLD(4),=C'REG-'                                                 
         BNE   MSEQVE        ERROR                                              
         TM    MKTSAVE,X'02'                                                    
         BNO   MSEQVE                                                           
         CLI   IFLD+4,C'A'                                                      
         BL    MSEQVE                                                           
         CLI   IFLD+4,C'Z'                                                      
         BNH   MSEQV2B            A-Z  OK                                       
         CLI   IFLD+4,C'1'                                                      
         BL    MSEQVE                                                           
         CLI   IFLD+4,C'9'                                                      
         BNH   MSEQV2B             1-9  OK                                      
         B     MSEQVE        ERROR                                              
*                                                                               
MSEQV2B  MVC   RMARK+3(1),IFLD+4                                                
         OI    FIND,X'08'          MSEQ=REG-N                                   
         MVI   RDIST,C'G'                                                       
         B     MSEQVX                                                           
*                                                                               
MSEQVE   MVC   FERN,=AL2(MSEQINV)        INVVALID MAR SEQ                       
MSEQVX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
ALPVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLC   RNUM,=C'BU'         IF BU, DU or K5                              
         BE    ALPDPT2             DO 2 CHAR DAYPART                            
         CLC   RNUM,=C'K5'                                                      
         BE    ALPDPT2                                                          
         CLC   RNUM,=C'DU'                                                      
         BNE   ALPVAL1                                                          
**************************************************************                  
* 2 CHARACTER DYPART HANDLING                                                   
ALPDPT2  CLI   IFLDH+5,0                                                        
         BE    ALPVX                                                            
         LLC   R2,IFLDH+5         LENGTH OF INPUT                               
         BCTR  R2,0                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,AGYB                                                     
         OI    NDPTAGM,X'03'       ORG IN MEDIA                                 
*                                                                               
HNDPT010 DS    0H                                                               
         MVC   KEYD,KEY                                                         
         GOTO1 DATAMGR,PLIST,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                  
         B     HNDPT040                                                         
HNDPT020 DS    0H                                                               
         GOTO1 DATAMGR,PLIST,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                  
*                                                                               
HNDPT040 CLC   KEY(5),KEYD         AGY LEVEL/CLIENT LEVEL                       
         BNE   HNDPT100            NO                                           
         EX    R2,*+12                                                          
         BNE   HNDPT020                                                         
         B     *+10                                                             
         CLC   NDPTDPTA(0),IFLD        MATCH ON CODE?                           
*                                                                               
         CLC   RNUM,=C'K5'                                                      
         BNE   HNDPT060                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   RCARD2+77(0),IFLD                                                
*                                                                               
         OI    FIND,X'04'                                                       
         B     ALPVX                                                            
HNDPT060 MVC   IFLD(1),NDPTDPTE     PASS BACK EQUATE                            
         OI    FIND,X'04'                                                       
         B     ALPVO                                                            
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL-RESET KEY-MOVE CLIENT CODE IN-RESTART SEARCH                  
* IF CLIENT LEVEL-EXIT ROUTINE-DPT WAS INVALID                                  
*                                                                               
HNDPT100 OC    KEYD+3(2),KEYD+3                                                 
         BNZ   ALPVERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYD                                                      
         GOTO1 CLPACK,PLIST,RCLI,KEY+3                                          
         CLI   PLIST,0                                                          
         BNE   ALPVERR                                                          
         B     HNDPT010                                                         
*******************************************                                     
ALPVAL1  DS    0H                                                               
         CLI   IFLDH,3             3=ALL INPUT                                  
         BE    ALLVAL                                                           
         CLI   IFLDH,1             1=INPUT                                      
         BNE   ALPVX                                                            
         B     ALPVALA                                                          
*                                                                               
ALLVAL   CLC   RNUM,=C'MC'         MC,MD ACCEPT AL                              
         BE    SKP                                                              
         CLC   RNUM,=C'MD'                                                      
         BNE   A3ERR                                                            
SKP      MVI   RO7,X'FF'                                                        
         OI    FIND,X'04'                                                       
         B     ALPVX                                                            
*                                                                               
ALPVALA  CLC   RNUM,=C'Z5'         Z5/Z7 ACCEPTS UP TO 4 CHARACTERS             
         BE    *+14                                                             
         CLC   RNUM,=C'Z7'                                                      
         BNE   AL03                                                             
*                                                                               
         CLI   IFLD,C'-'           IS IT '-NNNN' ?                              
         BNE   Z510                NO                                           
         NI    IFLD+1,X'FF'-X'80'  YES                                          
         MVC   IFLD(4),IFLD+1                                                   
*                                                                               
Z510     LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         OI    FIND,X'04'                                                       
         B     ALPVX                                                            
*                                                                               
                                                                                
AL03     CLC   RNUM,=C'A3'         A3 ACCEPTS B,D,COM,NOPIG                     
         BNE   ALPV1                                                            
         XC    SPTREC+500(150),SPTREC+500                                       
         GOTO1 SCANNER,PLIST,IFLDH,(5,SPTREC+500)                               
         LLC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    ALPVX                                                            
         LA    R5,SPTREC+500                                                    
AL5      CLI   12(R5),C'A'        BILL ADJUSTMENT                               
         BNE   AL10                                                             
         MVI   RO7,C'Y'                                                         
         B     AL30                                                             
AL10     CLI   12(R5),C'D'                                                      
         BNE   AL12                                                             
         MVI   RREPNO+1,C'D'                                                    
         B     AL30                                                             
AL12     CLI   12(R5),C'B'                                                      
         BNE   AL14                                                             
         MVI   RO6,C'B'                                                         
         B     AL30                                                             
AL14     DS    0H                                                               
         CLC   12(3,R5),=C'COM'                                                 
         BNE   AL16                                                             
         MVI   RNUM+33,C'Y'                                                     
         B     AL30                                                             
*                                                                               
AL16     DS    0H                                                               
         CLC   12(5,R5),=C'NOPIG'                                               
         BNE   AL18                                                             
         MVI   RNUM+34,C'N'                                                     
         B     AL30                                                             
*                                                                               
AL18     DS    0H                                                               
         B     A3ERR                                                            
*                                                                               
AL30     LA    R5,32(R5)                                                        
         BCT   R1,AL5                                                           
         OI    FIND,X'04'                                                       
         B     ALPVX                                                            
****                                                                            
         CLI   IFLDH+5,3          A3 REPORT ACCEPTS D,B IN THIS FIELD           
         BH    A3ERR                                                            
         CLI   IFLD,C'D'                                                        
         BE    A31                                                              
         CLI   IFLD,C'B'                                                        
         BNE   A3ERR                                                            
         MVI   RO6,C'B'                                                         
         B     *+8                                                              
A31      MVI   RREPNO+1,C'D'                                                    
         CLI   IFLDH+5,1                                                        
         BE    A3X                                                              
         CLI   IFLD+1,C','                                                      
         BE    A33                                                              
         CLI   IFLD+1,C'/'                                                      
         BNE   A3ERR                                                            
A33      CLI   IFLD+2,C'D'                                                      
         BNE   A35                                                              
         MVI   RREPNO+1,C'D'                                                    
         B     A3X                                                              
A35      CLI   IFLD+2,C'B'                                                      
         BNE   A3ERR                                                            
         MVI   RO6,C'B'                                                         
A3X      OI    FIND,X'04'                                                       
         B     ALPVX                                                            
A3ERR    MVC   FERN,=AL2(FLDINV)                                                
         B     ALPVX                                                            
*                                                                               
ALPV1    CLI   IFLDH+5,1                                                        
         BH    ALPV5                                                            
*                                                                               
         CLC   RNUM,=C'A5'          A5 ACCEPTS C'*'                             
         BNE   ALPV3                                                            
         CLI   IFLD,C'*'                                                        
         BE    ALPV4                                                            
*                                                                               
ALPV3    CLI   IFLD,C'A'                                                        
         BL    ALPVE                                                            
         CLI   IFLD,C'Z'                                                        
         BH    ALPV5                                                            
ALPV4    OI    FIND,X'04'       X'04'= ALPHA                                    
         B     ALPVO                                                            
*                                                                               
ALPV5    DS    0H                                                               
         CLC   RNUM(2),=C'N5'                                                   
         BNE   *+12                                                             
         OI    FIND,X'08'          X'08' = NUMERIC                              
         B     ALPVO                                                            
         GOTO1 ARJN                MUST BE NUMERIC                              
         CLC   FERN,=AL2(FF)                                                    
         BNE   ALPVERR                                                          
         IC    R7,TEMP+2                                                        
         SLL   R7,4                                                             
         STC   R7,TEMP+2                                                        
         NI    TEMP+3,X'0F'                                                     
         OC    TEMP+2(1),TEMP+3       TURN 12 INTO X'12'                        
         MVC   IFLD(1),TEMP+2                                                   
         OI    FIND,X'08'          X'08' = NUMERIC                              
         B     ALPVO                                                            
*                                                                               
ALPVERR  MVC   FERN,=AL2(FLDINV)                                                
         B     ALPVO                                                            
ALPVE    MVC   FERN,=AL2(NOTALP)       NOT APLHA                                
ALPVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
ALPVX    B     XIT                                                              
         EJECT                                                                  
RATVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BE    RATV00                                                           
         CLC   RNUM,=C'EB'          EB REPORT DEFAULTS TO 'N'                   
         BNE   RATV0                                                            
         MVI   IFLD,C'N'                                                        
         B     *+8                                                              
RATV0    MVI   IFLD,C'A'               SET DEFAULT                              
RATV00   LA    R7,RATTAB                                                        
         LA    R2,2                                                             
RATV1    CLC   IFLD(1),0(R7)                                                    
         BE    RATV2                                                            
         LA    R7,2(R7)                                                         
         BCT   R2,RATV1                                                         
         B     RATVE          RATING SERVICE INVALID                            
*                                                                               
RATV2    TM    CLISAVE,X'12'        WAS CLIENT ALL                              
         BZ    RATV3           NO- - CHECK PROFILE                              
         MVC   CLIPROF+3(1),1(R7)        SET RATING SERVICE                     
         OI    FIND,X'04'                                                       
         B     RATVO                                                            
*                                                                               
RATV3    CLC   CLIPROF+3(1),1(R7)                                               
         BNE   RATVZ      WRONG SERVICE                                         
         OI    FIND,X'04'                                                       
         B     RATVO                                                            
*                                                                               
RATVE    MVC   FERN,=AL2(FLDINV)                                                
         B     RATVX                                                            
RATVZ    MVC   FERN,=AL2(WNGRAT)                                                
         B     RATVX                                                            
*                                                                               
RATVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
RATVX    B     XIT                                                              
*                                                                               
RATTAB   DC    C'N0A1'                                                          
         EJECT                                                                  
         SPACE 2                                                                
MSCHVAL  BRAS  RE,MSCHVL                                                        
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE NETWORK AND SET FIND BITS                                     
*        X'02'=ALL                                                              
*        X'04'=XXXX  OR BBM OR NSI                                              
*                                                                               
NETVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLC   RNUM,=C'C1'         FOR C1                                       
         BE    NET4B               ACCEPT ANYTHING(PER LISA)                    
         CLC   RNUM,=C'Z7'                                                      
         BE    NETV0                                                            
         CLC   RNUM,=C'BU'                                                      
         BE    NETBU1                                                           
         CLC   RNUM,=C'DU'                                                      
         BE    NETBU1                                                           
         CLC   RNUM,=C'N2'                                                      
         BNE   NETVD                                                            
         L     RF,ASAVE                                                         
         USING T208FFD,RF                                                       
         CLC   =C'SOON',BVROUT     FOR SOON DISALLOW                            
         BE    *+14                THIS OPTION                                  
         DROP  RF                                                               
         CLC   IFLD(4),=C'ALL,'    AND ALL,N                                    
         BE    NETVB                                                            
*                                                                               
NETV0    CLI   FIND,1                                                           
         BL    NETVX                                                            
         BH    NETVO                                                            
         B     NETBU3                                                           
NETVB    CLI   IFLD+4,C'N'                                                      
         BE    NETVC                                                            
         CLI   IFLD+4,C'S'                                                      
         BE    NETVC                                                            
         CLI   IFLD+4,C'D'                                                      
         BE    NETVC                                                            
         CLI   IFLD+4,C'O'                                                      
         BE    NETVC                                                            
         CLI   IFLD+4,C'V'                                                      
         BE    NETVC                                                            
         CLI   IFLD+4,C'C'                                                      
         BNE   NETVE                                                            
NETVC    MVC   RBOOK1+1(1),IFLD+4                                               
         OI    FIND,X'04'                                                       
         B     NETVX                                                            
*                                                                               
NETVD    CLI   FIND,1                                                           
         BL    NETVX               MISSING                                      
         BH    NETVO               ALL                                          
         B     NETV1                                                            
*                                                                               
NETBU1   DS    0H                  ***  BU (NETPAK BILLING),DU                  
         CLI   FIND,1                                                           
         BE    NETBU2A                                                          
         BL    NETVX               MISSING                                      
         OI    FIND,X'02'                                                       
         B     NETVO                                                            
NETBU2A  CLC   IFLD(4),=C'ALL,'    ALL,X                                        
         BNE   NETBU3                                                           
         CLI   IFLD+4,C'*'         ALL,* MEANS EXPAND INTO C,S,N                
         BNE   NETBU2B                                                          
         L     RF,ASAVE                                                         
         USING T208FFD,RF                                                       
         CLC   =C'SOON',BVROUT     FOR SOON DISALLOW                            
         BE    NETVE               THIS OPTION                                  
         DROP  RF                                                               
         B     NETBU2C                                                          
NETBU2B  CLI   IFLD+4,C'S'                                                      
         BE    NETBU2C                                                          
         CLI   IFLD+4,C'C'                                                      
         BE    NETBU2C                                                          
         CLI   IFLD+4,C'O'                                                      
         BE    NETBU2C                                                          
         CLI   IFLD+4,C'D'                                                      
         BE    NETBU2C                                                          
         CLI   IFLD+4,C'V'                                                      
         BE    NETBU2C                                                          
         CLI   IFLD+4,C'N'                                                      
         BNE   NETVE                                                            
NETBU2C  MVC   RO5,IFLD+4          DU,BU                                        
         OI    FIND,X'04'                                                       
         B     NETVX                                                            
NETBU3   XC    KEYS,KEYS                                                        
         MVI   KEYS,C'S'                                                        
         MVI   KEYS+1,C'N'                                                      
         MVC   KEYS+2(4),IFLD                                                   
         MVI   KEYS+6,C'N'                                                      
         MVC   KEYS+7(2),RAGY                                                   
         MVC   KEYS+9(8),=C'00000000'                                           
         GOTO1 ARSTA                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    NETVO                                                            
         BH    NETBU3A                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         B     NETVO                                                            
NETBU3A  CLC   RNUM,=C'BU'         IF BU/DU                                     
         BE    NBU3B                                                            
         CLC   RNUM,=C'N2'         OR N2 (THAT'S I2)                            
         BE    NBU3B                                                            
         CLC   RNUM,=C'DU'                                                      
         BNE   NET4B                                                            
NBU3B    LA    R4,SPTREC                                                        
         USING STAREC,R4                                                        
         CLC   RNUM,=C'N2'          FOR N2 SET MEDTYPE                          
         BNE   NBU3C                                                            
         MVC   RBOOK1+1(1),STYPE                                                
         B     NET4B                                                            
NBU3C    CLI   RO5,X'40'           5/22                                         
         BH    NET4B                                                            
         MVC   RO5,STYPE           SET MEDIA TYPE                               
         B     NET4B                                                            
         DROP  R4                                                               
*                                                                               
*                                STANDARD PROCESSING                            
*                                                                               
NETV1    JIF  RNUM,NE,=C'44',OR,RO1,NE,C'L',NETV4,JUMP=N                        
*                  RATING SERVICE FOR SPILL REQUEST                             
         CLC   IFLD(4),=C'BBM '                                                 
         BE    NETV2                                                            
         CLC   IFLD(4),=C'NSI '                                                 
         BE    NETV2                                                            
         CLC   IFLD(4),=C'ARB '                                                 
         BE    NETV2                                                            
         B     NETVE                                                            
*                                                                               
NETV2    OI    FIND,X'04'                                                       
         B     NETVO                                                            
*                                                                               
NETV4    DS    0H                                                               
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),RAGY                                                    
         MVC   KEY+4(4),IFLD                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    NETVO               DISK ERROR                                   
         BH    NET4A                                                            
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     NETVO                                                            
*                                                                               
NET4A    MVC   KEY(13),KEYS                                                     
NET4B    OI    FIND,X'04'                                                       
         B     NETVO                                                            
*                                                                               
NETVE    MVC   FERN,=AL2(FLDINV)                                                
NETVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         OC    STASAVE,FIND                                                     
*                                                                               
NETVX    B     XIT                                                              
         EJECT                                                                  
*        VALIDATE DEMO AND SET FIND BIT                                         
*        X'04'= NNN                                                             
*              BINARY OUTPUT IN REQUEST                                         
*                                                                               
DEMOVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         MVI   TEMP,0                                                           
         CLI   FIND,1                                                           
         BNE   DEMOVO              ALL OR MISSING                               
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BNE   DEMOVE                                                           
         CLI   TEMP,0                                                           
         BE    DEMOVE                                                           
         OI    FIND,X'04'                                                       
         B     DEMOVO                                                           
*                                                                               
DEMOVE   MVC   FERN,=AL2(DEMINV)                                                
         B     DEMOVX                                                           
DEMOVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),TEMP        BINARY VALUE                                 
*                                                                               
DEMOVX   B     XIT                                                              
         EJECT                                                                  
*        VALIDATE DATA TYPES AND SET FIND BIT                                   
*        X'04' = 1-9 4 POSITIONS                                                
*                                                                               
DATAVAL  NTR1                                                                   
         CLC   RNUM,=C'K4'                                                      
         BE    DATV1                                                            
         CLC   RNUM,=C'K5'                                                      
         BE    DATV1                                                            
         CLC   RNUM,=C'KL'                                                      
         BE    DATV1                                                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   DATAVO                                                           
DATAV1   LA    R7,IFLD                                                          
         MVC   TEMP(9),=C'123456789'                                            
*                                                                               
DATAV2   LA    R2,TEMP                                                          
         LA    R4,9                FOR BCT                                      
*                                                                               
DATAV3   CLC   0(1,R7),0(R2)                                                    
         BNE   DATAV4                                                           
         LA    R7,1(R7)                                                         
         MVI   0(R2),0             0 THIS ENTRY IN TEMP                         
*                                  TO STOP DUPLICATES                           
         BCT   R5,DATAV2           R5 STARTS WITH INPUT LENGTH                  
         OI    FIND,X'04'                                                       
         B     DATAVO                                                           
*                                                                               
DATAV4   LA    R2,1(R2)                                                         
         BCT   R4,DATAV3                                                        
         B     DATAVE              ERROR FOUND                                  
*                                                                               
DATAVE   MVC   FERN,=AL2(FLDINV)                                                
DATAVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
*                                                                               
DATAVX   B     XIT                                                              
         SPACE 2                                                                
DATV1    DS    0H           K4/K5/KL ACCEPTS 1 ALPHA OR 2 DIGITS                
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   DATAVOK                                                          
         CLI   IFLDH+5,2                                                        
         BH    DATAVE                                                           
         CLI   IFLDH+5,1                                                        
         BNE   DATV2                                                            
         CLI   IFLD,C'A'                                                        
         BL    DATAVE                                                           
         CLI   IFLD,C'Z'                                                        
         BH    DATAVE                                                           
         OI    FIND,X'04'                                                       
         B     DATAVOK                                                          
         SPACE                                                                  
DATV2    DS    0H                      MUST BE 2 DIGITS                         
         LA    R7,IFLD                                                          
         MVC   TEMP(10),=C'0123456789'                                          
DATV3    LA    R2,TEMP                                                          
         LA    R4,10                   FOR BCT                                  
         SPACE                                                                  
DATV4    CLC  0(1,R7),0(R2)                                                     
         BNE   DATV5                                                            
         LA    R7,1(R7)                                                         
         BCT   R5,DATV3            R5 STARTS WITH INPUT LENGTH                  
         OI    FIND,X'04'                                                       
         B     DATAVOK                                                          
         SPACE                                                                  
DATV5    LA    R2,1(R2)                                                         
         BCT   R4,DATV4                                                         
         B     DATAVE              ERROR                                        
         SPACE                                                                  
*                                                                               
DATAVOK  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),IFLD                                                     
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE REPORT AND SET FIND FORMAT BITS                               
*        X'04' XX                                                               
*                                                                               
REPRTVAL NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BNE   REPRTVX                                                          
         CLC   =C'A8',RNUM         SPOT A8 N=NETPAK UNITS ONLY                  
         BNE   REPRT2                                                           
         CLI   IFLD,C'N'                                                        
         BNE   REPRTINV                                                         
         MVI   RO6,C'N'                                                         
         OI    FIND,X'04'                                                       
         B     REPRTVX                                                          
REPRT2   CLC   =C'PM',RNUM         SPOT PM                                      
         BNE   REPRT5                                                           
         MVC   RO2(1),IFLD         FINAL OR REVISED                             
         CLI   IFLD,C'F'                                                        
         BE    *+12                                                             
         CLI   IFLD,C'R'                                                        
         BNE   REPRTINV                                                         
         OI    FIND,X'04'                                                       
         B     REPRTVX                                                          
         SPACE                                                                  
REPRT5   CLI   IFLDH+5,2                                                        
         BL    REPRTINV                                                         
         BH    REPRTVO                                                          
         MVC   TEMP+1(2),IFLD                                                   
         MVI   TEMP,C' '           INSERT LEADING SPACE                         
         MVC   IFLD(3),TEMP                                                     
*                                                                               
REPRTVO  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         OI    FIND,X'04'                    REPRT VALID =XX                    
         B     REPRTVX                                                          
REPRTINV MVC   FERN,=AL2(02)                                                    
REPRTVX  B     XIT                                                              
         EJECT                                                                  
*              PARTIAL MATCH                                                    
*        X'04' = * OR 1-4 CHARS OF  TLCPDWAF                                    
*                                                                               
PARTVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   PARTVX              INITV SET CONDITION CODE                     
         CLI   IFLD,C'*'                                                        
         BNE   PARTV2                                                           
         CLI   IFLDH+5,1           * MUST BE ALONE                              
         BNE   PARTVE                                                           
         OI    FIND,X'04'                                                       
         B     PARTVO                                                           
*                                                                               
PARTV2   LA    R7,IFLD                                                          
         MVC   TEMP(8),=C'TLCPDWAF'                                             
*                                                                               
PARTV4   LA    R2,TEMP                                                          
         LA    R4,8                FOT BCT                                      
*                                                                               
PARTV6   CLC   0(1,R7),0(R2)                                                    
         BNE   PARTV8                                                           
         LA    R7,1(R7)                                                         
         MVI   0(R2),0             TO PREVENT DUPLICATES                        
         BCT   R5,PARTV4           R5 HAS INPUT LENGTH                          
         OI    FIND,X'04'                                                       
         B     PARTVO                                                           
*                                                                               
PARTV8   LA    R2,1(R2)                                                         
         BCT   R4,PARTV6                                                        
         B     PARTVE              ERROR                                        
*                                                                               
PARTVE   MVC   FERN,=AL2(FLDINV)                                                
PARTVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
*                                                                               
PARTVX   J     XIT                                                              
         EJECT                                                                  
*        RE-ALLOCATE PRDS                                                       
*        X'04'= 1-4 PRD=N'S                                                     
*        X'08'= UNALLOCATE,DELETE                                               
*                                  PUTS BINARY VALUE AFTER PROD                 
*                                  AND VALUES MUST = 100                        
REALVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   REALVX              NO INPUT                                     
         SR    R7,R7                                                            
         LLC   R5,IFLDH+5                                                       
         BCTR  R5,0                                                             
         EX    R5,UNACOMP                                                       
         BNE   REALV3                                                           
         OI    FIND,X'08'          UNALLOCATE                                   
         B     PARTVX              LEAVE AS SPACES                              
REALV3   EX    R5,DELCOMP          DELETE                                       
         BNE   REALV5                                                           
         CLI   RMED,C'N'           IF MEDIA =N                                  
         BNE   *+14                                                             
         CLC   =C'K2',RNUM         AND IT'S K2                                  
         BE    REALVE                                                           
         MVC   RNUM+49(6),=C'DELETE'                                            
         OI    FIND,X'08'                                                       
         B     PARTVX                                                           
*                                                                               
REALV5   XC    SPTREC+500(150),SPTREC+500                                       
         MVI   TEMP+30,C' '                                                     
         MVC   TEMP+31(20),TEMP+30                                              
         GOTO1 SCANNER,PLIST,IFLDH,(5,SPTREC+500)                               
         CLI   PLIST+4,0                                                        
         BE    REALVE              ERROR                                        
         CLI   PLIST+4,4                                                        
         BH    REALVE              ERROR - TOO MANY PRDS                        
         LA    R4,4                FOR BCT                                      
         LA    R5,SPTREC+500                                                    
         LA    R6,TEMP+30                                                       
         USING SCAND,R5                                                         
REALV10  CLI   FLD1LEN,0                                                        
         BE    REALVO                                                           
         CLI   FLD2LEN,3                                                        
         BH    REALVE                                                           
         CLI   FLD2LEN,1                                                        
         BL    REALVE                                                           
         TM    FLD2VAL,X'80'       TEST NUMERIC                                 
         BZ    REALVE                                                           
         CLI   FLD1LEN,3                                                        
         BH    REALVE                                                           
         CLI   FLD1LEN,2                                                        
         BL    REALVE                                                           
         CLC   FLD1(3),=C'POL'     DON'T ACCEPT POL                             
         BE    REALVE2                                                          
*****    CLC   RPRO,FLD1           CAN'T MATCH REQUESTED PRD                    
*****    BE    REALVE                (OK TIM/5/8/91)                            
         LA    R1,TEMP+30                                                       
         LA    R2,4                                                             
REALV15  CLC   0(3,R1),FLD1                                                     
         BE    REALVE3             DUPLICATE PRDS                               
         LA    R1,4(R1)                                                         
         BCT   R2,REALV15                                                       
         MVC   KEY+4(3),FLD1      KEY(4) WILL ALREADY BE SET                    
         XC    KEY+7(5),KEY+7                                                   
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    REALVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(PRDNFND)        PRODUCT NOT FOUND                      
         B     REALVO                                                           
         MVC   0(3,R6),FLD1                                                     
*        MVC   3(1,R6),FLD2                                                     
         MVC   3(1,R6),FLD2B+3     BINARY VALUE                                 
         LLC   R1,FLD2B+3          AND TOTAL IN R7                              
         AR    R7,R1                                                            
         OI    FIND,X'04'                                                       
         LA    R6,4(R6)                                                         
         LA    R5,32(R5)           NEXT SCANNER FIELD                           
         BCT   R4,REALV10                                                       
         B     REALVO                                                           
*                                                                               
REALVE   MVC   FERN,=AL2(FLDINV)                                                
         B     REALVO                                                           
REALVE2  MVC   FERN,=AL2(NOPOL)                                                 
         B     REALVO                                                           
REALVE3  MVC   FERN,=AL2(DUPERR)                                                
REALVO   DS    0H                                                               
         LR    R1,R7                                                            
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(16,R7),TEMP+30                                                 
         C     R1,=F'100'          EQUALS 100                                   
         BE    REALVX                                                           
         C     R1,=F'0'            OR 0                                         
         BNE   REALINV             ELSE INVALID                                 
         LA    R4,4                                                             
REALOOP  CLI   0(R7),X'40'         WHEN = 0 SET BINARY TO 1 AFTER               
         BNH   REALVX                       EACH PRODUCT                        
         MVI   3(R7),1                                                          
         LA    R7,4(R7)                                                         
         BCT   R4,REALOOP                                                       
         B     REALVX                                                           
REALINV  MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
REALVX   J     XIT                                                              
*                                                                               
UNACOMP  CLC   IFLD(0),=C'UNALLOCATE'  EXECUTED                                 
DELCOMP  CLC   IFLD(0),=C'DELETE'      EXECUTED                                 
         EJECT                                                                  
*        DEMO MENU                                                              
*        X'04'=MENU INPUT                                                       
*                                                                               
MENUVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,X'01'                                                       
         BNE   MENUVX                                                           
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),KEYS+1                                                  
         MVC   KEY+3(4),IFLD                                                    
         GOTO1 ARSPT                                                            
         MVC   KEY(13),KEYS        RESTORE KEY                                  
         CLC   FERN,=AL2(FE)                                                    
         BL    MENUVO              DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     MENUVO                                                           
         OI    FIND,X'04'          VALID MENU                                   
         CLC   RNUM,=C'98'         DON'T SET ON COL 30                          
         BE    *+8                 FOR PG EXC - 98                              
         MVI   R3037,C'Y'                                                       
*                                                                               
MENUVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
*                                                                               
MENUVX   J     XIT                                                              
         EJECT                                                                  
NREPVAL  NTR1                                REP NUMBER - FIND BITS             
*                                            02=TNNN                            
         GOTO1 AINITV                        04=PNNN                            
         CLI   IFLDH,1                       08=SNNN                            
         BNE   NREPVO                        10=SALL                            
*                                            20=PALL                            
*                                            40=TALL                            
         CLI   IFLDH+5,2                                                        
         BNH   NREPVE                                                           
         CLI   IFLD,C'S'                                                        
         BNE   NREPV5                                                           
         MVI   RREPT,C'S'                                                       
         CLC   IFLD+1(3),=C'ALL'                                                
         BNE   NREPV10                                                          
         MVC   IFLD(3),IFLD+1                                                   
         MVI   IFLD+3,C' '                                                      
         OI    FIND,X'10'          SALL                                         
         B     NREPVO                                                           
*                                                                               
NREPV5   CLI   IFLD,C'P'                                                        
         BNE   NREPV7                                                           
         MVI   RREPT,C'P'                                                       
         CLC   IFLD+1(3),=C'ALL'                                                
         BNE   NREPV10                                                          
         MVC   IFLD(3),IFLD+1                                                   
         MVI   IFLD+3,C' '                                                      
         OI    FIND,X'20'          PALL                                         
         B     NREPVO                                                           
*                                                                               
NREPV7   CLI   IFLD,C'T'                                                        
         BNE   NREPVE                                                           
         MVI   RREPT,C'T'                                                       
         CLC   IFLD+1(3),=C'ALL'                                                
         BNE   NREPV10                                                          
         MVC   IFLD(3),IFLD+1                                                   
         MVI   IFLD+3,C' '                                                      
         OI    FIND,X'40'          TALL                                         
         B     NREPVO                                                           
*                                                                               
NREPV10  MVC   IFLD(3),IFLD+1                                                   
         MVI   IFLD+3,C' '                                                      
         LLC   R0,IFLDH+5                                                       
         SHI   R0,1                ADJUST LENGHT                                
         STC   R0,IFLDH+5          STORE NEW LENGHT                             
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FLDNUM)                                                
         BE    NREPVE                        REP NON NUMERIC                    
         MVC   FERN,=AL2(FF)                    IGNORE SIZE                     
         MVC   IFLD(3),TEMP+1                                                   
         MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'R'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(3),IFLD                                                   
         MVC   KEYS+5(2),AGY                                                    
         GOTO1 ARSTA                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    NREPVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(REPNOF)                  REP NOT ON FILE               
         B     NREPVO                                                           
         MVC   NAME(22),SPTREC+18                                               
         OI    FIND,X'04'                                                       
         CLI   RREPT,C'P'          PNNN                                         
         BE    NREPVO                                                           
         OI    FIND,X'08'          SNNN                                         
         CLI   RREPT,C'S'                                                       
         BE    NREPVO                                                           
         OI    FIND,X'02'          TNNN                                         
         CLI   RREPT,C'T'                                                       
         BE    NREPVO                                                           
NREPVE   MVC   FERN,=AL2(REPINV)                   REP INVALID                  
NREPVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
NREPVX   J     XIT                                                              
         EJECT                                                                  
*                                  DEMO MENU FILTER VALIDATION                  
*                                  X'04'=N,-N,* (X4)                            
*                                  X'08'=ALL                                    
*                                                                               
MENUFVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,X'01'                                                       
         BNE   MNVX                                                             
         CLI   IFLDH+5,4                                                        
         BNE   MNV2                                                             
         CLC   IFLD(4),=C'ALL '                                                 
         BNE   MNV2                                                             
         OI    FIND,X'08'                                                       
         B     MNVX                LEAVE AS BLANKS                              
*                                                                               
MNV2     CLI   IFLDH+5,4                                                        
         BL    MNVE                                                             
         CLI   IFLDH+5,8                                                        
         BH    MNVE                                                             
         LLC   R1,IFLDH+5          SAVE INPUT LENGHT                            
         LA    R4,IFLD                                                          
         LA    R5,4                FOR BCT                                      
         XC    TEMP(5),TEMP                                                     
         LA    R6,TEMP+1                                                        
MNV3C    CLI   0(R4),C'-'          SEE IF NEGATIVE FILTER                       
         BNE   MNV3E                                                            
         MVI   TEMP,1                                                           
         LA    R4,1(R4)            BUMP PAST -                                  
         BCTR  R1,0                                                             
MNV3E    MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   MNV3F                                                            
         CLI   TEMP,0                                                           
         BNE   MNVE                                                             
         B     MNV3G                                                            
*                                                                               
MNV3F    CLI   0(R6),C'A'                                                       
         BL    MNVE                                                             
         CLI   0(R6),C'9'                                                       
         BH    MNVE                                                             
         CLI   TEMP,1              CHK IF NEGATIVE FILTER                       
         BNE   *+8                                                              
         NI    0(R6),X'BF'         SET OFF X'40' BIT                            
MNV3G    LA    R6,1(R6)                                                         
         MVI   TEMP,0                                                           
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R5,MNV3C                                                         
         LTR   R1,R1                                                            
         BNZ   MNVE                                                             
         OI    FIND,X'04'          VALID FILTERS                                
         B     MNV0                                                             
*                                                                               
MNVE     MVC   FERN,=AL2(FLDINV)                                                
         B     MNVX                                                             
*                                                                               
MNV0     DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),TEMP+1                                                   
MNVX     J     XIT                                                              
         EJECT                                                                  
***********************************************                                 
*  THIS ROUTINE CHECKS TO SEE IF SOMETHING    *                                 
*  HAS BEEN INPUT IN THE COMM ID FIELD        *                                 
*  BUT IT DOES NO EDITING                     *                                 
***********************************************                                 
         SPACE                                                                  
COMIDVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   COMIDX              NO INPUT/ERROR                               
         SPACE                                                                  
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(8,R7),IFLD                                                     
         OI    FIND,X'04'                                                       
         SPACE                                                                  
COMIDX   J     XIT                                                              
         EJECT                                                                  
************************************                                            
* INCLUDE REPORT VALIDATION                                                     
*  X'04'=NN (A2,AB,AX)                                                          
*                                                                               
IRPTVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   IRPTX                                                            
         L     RF,ASAVE                                                         
         USING T208FFD,RF                                                       
         CLC   =C'B1',RNUM         IS IT B1, SOON BILLING?                      
         BNE   *+14                                                             
         CLC   =C'SOON',BVROUT     FOR UPDATIVE SOON DISALLOW                   
         BE    IRPINV              THIS OPTION                                  
         DROP  RF                                                               
         CLC   =C'A2',IFLD                                                      
         BNE   IRP5                                                             
         MVI   IFLD,C'2'                                                        
         B     IRPTOK                                                           
IRP5     CLC   =C'AB',IFLD                                                      
         BNE   IRPINV                                                           
         MVI   IFLD,C'B'                                                        
         B     IRPTOK                                                           
IRPINV   MVC   FERN,=AL2(FLDINV)                                                
         B     *+8                                                              
IRPTOK   OI    FIND,X'04'                                                       
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
IRPTX    J     XIT                                                              
         EJECT                                                                  
*                                                                               
OPTIONS  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    OPTXIT                                                           
         GOTO1 =A(OPTRTN),DMCB,RR=RELO                                          
OPTXIT   J     XIT                                                              
*                                                                               
BILLNUM  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    BILXT                                                            
         CLI   DDS,1                                                            
         BNE   BILXT                                                            
         GOTO1 =A(BILLNM),DMCB,RR=RELO                                          
BILXT    J     XIT                                                              
*                                                                               
REALLOC  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    REAX                                                             
         GOTO1 =A(REALOC),DMCB,RR=RELO                                          
REAX     J     XIT                                                              
*                                                                               
PROGRAM  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    PROX                                                             
         GOTO1 =A(PROGRM),DMCB,RR=RELO                                          
PROX     J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
DDSFMT   EQU   X'04'                                                            
FLDINV   EQU   2                    INVALID INPUT                               
FLDNUM   EQU   03                            NON NUMERIC INPUT FLD              
REPNOF   EQU   28                            REP NOT ON FILE                    
REPINV   EQU   29                            REP INVALID                        
DTEINV   EQU   20                            DATE INVALID                       
BOKNOF   EQU   30                            BOOK NOT ON FILE                   
BOKINV   EQU   31                            BOOK-HUT INVALID                   
HUTNOF   EQU   32                            HUT NOT ON FILE                    
REGINV   EQU   33                            REGION INVALID                     
PRDNFND  EQU   41                  PRODUCT NOT ON FILE                          
PCTINV   EQU   02                            PERCENTAGE INVALID                 
AMTINV   EQU   02                            AMOUNT INVALID                     
ONEINV   EQU   02                            OPTION INVALID                     
PDVCLI   EQU   34                            DIV NOT DEFINED FOR CLI            
PDVINV   EQU   35                            DIV INVALID                        
PDSCLI   EQU   36                            DIS NOT DEFINED FOR CLI            
PDSINV   EQU   37                            DIS INVALID                        
NOPOL    EQU   102                 PRD POL INVALID                              
DUPERR   EQU   245                 DUPLICATE ENTRY                              
DEMINV   EQU   111                     INVALID DEMOGRAPHIC                      
MSEQINV  EQU   2                                                                
NOTALP   EQU   4          NOT ALPHA                                             
WNGRAT   EQU   107           WRONG RATING SERVICE                               
CAERROR  EQU   207           ACCESS ERROR                                       
PBNVAL   EQU   1344          PB= OPTION NOT VALID FOR PIGGYBACK REQUEST         
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM.                           
*                                                                               
ROUTADRT DC    A(0)                          00 - N/D                           
         DC    A(REPVAL)                     01 - REP                           
         DC    A(DTEVAL)                     02 - DATE                          
         DC    A(BOKVAL)                     03 - BOOK,HUT                      
         DC    A(OPTIONS)                    04 - OPTIONS                       
         DC    A(PCTVAL)                     05 - PERCENTAGE                    
         DC    A(AMTVAL)                     06 - AMOUNT                        
         DC    A(ONEVAL)                     07 - ONE SUNDRY CHR                
         DC    A(PDIVVAL)  08 ** THESE 2 FIELDS ARE AVAILABLE                   
         DC    A(PDISVAL)  09 ** FOR FUTURE USE *************                   
         DC    A(DEMVAL)                     10 - REPORT DEMOS                  
         DC    A(MSEQVAL)              11 - MARKET SEQUENCE                     
         DC    A(ALPVAL)              12 - ONE ALPHA CHR                        
         DC    A(RATVAL)               13 - RATING SERVICE                      
         DC    A(MSCHVAL)          14 - MKT GRP SCHEME                          
         DC    A(NETVAL)           15 - NETWORK                                 
         DC    A(DEMOVAL)          16- - DEMOGRAPHIC                            
         DC    A(DATAVAL)          17 - DATA TYPES                              
         DC    A(REPRTVAL)         18 - REPORT                                  
         DC    A(PARTVAL)          19 - PARTIAL MATCH                           
         DC    A(REALVAL)          20 - RE-AL PRDS                              
         DC    A(MENUVAL)          21 - DEMO MENU                               
         DC    A(NREPVAL)          22 - NEW REP LOGIC                           
         DC    A(MENUFVAL)         23 - DEMO MENU FILTER                        
         DC    A(COMIDVAL)         24 - COMMERCIAL ID                           
         DC    A(IRPTVAL)          25 - INCLUDE REPORTS                         
         DC    A(BILLNUM)          26 - BILLING NUMBERS                         
         DC    A(REALLOC)          27 - REALLOCATE PRD FOR K1                   
         DC    A(PROGRAM)          28 - PROGRAM NAME                            
         EJECT                                                                  
*                                                                               
*        EACH ENTRY IN THE ONE CHR TABLE HAS THE FORMAT                         
*        XL1   ENTRY LEN                                                        
*        XL2   REQUEST NUM / REQUEST SUB                                        
*        XL1   COLUMN NUM                                                       
*        XLN   VALUE LIST(S) - 1ST LIST TERMINATES WITH + - ANY EXTRA           
*              VALUES FOR DDS TERMS FOLLOW * AND FORM 2ND LIST.                 
*        AN ENTRY CAN BE FOLLOWED BY A SYNONYM ENTRY (REQUEST NUM=255)          
*        OF THE SAME LENGTH GIVING THE VALUES FOR THE REQ REC.                  
*                                                                               
ONEVTBL  DS    0C                                                               
         DC    AL1(07,03,0,67),C' YN'                                           
         DC    AL1(06,04,0,56),C'PI'                                            
         DC    AL1(08,06,0,56),C' IPU'                                          
         DC    AL1(08,07,0,56),C' IPU'                                          
         DC    AL1(07,07,0,62),C' YN'                                           
         DC    AL1(08,08,0,56),C' IPA'                                          
         DC    AL1(07,02,0,63),C'NYT'                                           
         DC    AL1(06,02,0,64),C' R'                                            
         DC    AL1(06,14,0,62),C'NY'                                            
         DC    AL1(06,15,0,62),C'SD'                                            
         DC    AL1(06,15,0,63),C'NY'                                            
         DC    AL1(05,16,0,62),C'S'                                             
         DC    AL1(07,16,0,63),C' NY'                                           
         DC    AL1(07,18,0,11),C' NY'                                           
         DC    AL1(06,19,0,65),C'NY'                                            
******   DC    AL1(08,20,0,56),C'+ GN'   inactive old report?                   
******   DC    AL1(08,20,0,62),C'NY+P'   inactive old report?                   
******   DC    AL1(08,255,0,0),C' A+P'   inactive old report?                   
******   DC    AL1(06,20,0,63),C'NY'     inactive old report?                   
******   DC    AL1(06,255,0,0),C' U'     inactive old report?                   
*                                        20 is now the SE report                
         DC    AL1(06,20,0,67),C'NY'                                            
         DC    AL1(08,21,0,62),C'NY+P'                                          
         DC    AL1(08,255,0,0),C' A+P'                                          
         DC    AL1(06,21,0,63),C'NY'                                            
         DC    AL1(06,255,0,0),C' U'                                            
         DC    AL1(07,22,0,62),C'NYT'                                           
         DC    AL1(07,23,0,11),C' NY'                 I2                        
         DC    AL1(11,23,0,62),C' YNFEXO'   (ANY CHANGE TO I2 SHOULD)           
         DC    AL1(07,23,0,63),C' NY'        (BE DONE TO N2 BELOW)              
         DC    AL1(08,23,0,64),C' 123'                                          
*******  DC    AL1(24,23,0,65),C' ABDS0123456789YNFEX'                          
         DC    AL1(12,23,0,65),C' ABDS123'                                      
         DC    AL1(08,23,0,66),C' NYP'                                          
         DC    AL1(07,23,0,68),C'NSA'                                           
*******  DC    AL1(06,24,0,67),C'NY'   NO-OPED  TEST RUN FOR CI                 
         DC    AL1(06,26,0,63),C'NY'                                            
         DC    AL1(06,27,0,65),C'NY'    LO - L'OREAL INTERFACE                  
         DC    AL1(06,28,0,67),C'NY'    TD - TAB DELIMITED INTERFACE            
         DC    AL1(06,29,0,62),C'NY'    JW - JW INTERFACE DO TAPE?              
         DC    AL1(06,41,0,62),C'CP'                                            
         DC    AL1(06,42,0,67),C'NY'    CH - CHOICE HOTELS                      
         DC    AL1(06,43,0,67),C'NY'    PZ - PFIZER                             
         DC    AL1(07,44,0,62),C'NSL'                                           
         DC    AL1(08,44,0,63),C' HLA'                                          
         DC    AL1(06,47,0,62),C'NY'                                            
         DC    AL1(06,47,0,63),C'NY'                                            
         DC    AL1(06,47,0,64),C'NY'                                            
         DC    AL1(08,48,0,62),C'ARMS'                                          
         DC    AL1(08,255,0,0),C' RMS'                                          
         DC    AL1(07,48,0,63),C' NY'                                           
         DC    AL1(07,48,0,64),C' NY'                                           
         DC    AL1(07,48,0,65),C' NY'                                           
         DC    AL1(06,49,0,62),C'NY'                                            
         DC    AL1(06,49,0,63),C'NY'                                            
         DC    AL1(07,49,0,64),C'+NY'                                           
         DC    AL1(07,255,0,0),C'+ Y'                                           
******   DC    AL1(09,50,0,62),C'OPUBR'    FOR SOME OLD REQ?                    
******   DC    AL1(06,50,0,63),C'NY'                                            
******   DC    AL1(06,50,0,64),C'NY'                                            
         DC    AL1(06,50,0,67),C'NY'       NOW AI - AT&T                        
         DC    AL1(30,51,0,62),C'NYABCDEFGHIJKLMOPQRSTUVWXZ'   VL               
         DC    AL1(06,52,0,62),C'NY'                                            
*        DC    AL1(11,52,0,62),C'OPUBRNC'                                       
*        DC    AL1(06,52,0,63),C'NY'                                            
*        DC    AL1(06,52,0,64),C'NY'                                            
*        DC    AL1(06,52,0,66),C'NY'                                            
         DC    AL1(06,255,0,0),C' Y'                                            
         DC    AL1(06,53,0,62),C'NY'                                            
*        DC    AL1(06,54,0,62),C'NY'                                            
*        DC    AL1(06,54,0,63),C'NY'                                            
         DC    AL1(06,55,0,61),C'NY'                                            
         DC    AL1(06,55,0,62),C'NY'                                            
         DC    AL1(06,55,0,63),C'NY'                                            
         DC    AL1(06,55,0,64),C'NY'                                            
         DC    AL1(06,55,0,65),C'NY'                                            
         DC    AL1(12,55,0,67),C' 1234567'                                      
         DC    AL1(07,57,0,68),C'DNS'                                           
         DC    AL1(06,59,0,62),C'NY'                                            
         DC    AL1(06,59,0,63),C'NY'                                            
         DC    AL1(06,59,0,64),C'NY'                                            
         DC    AL1(06,255,0,0),C' A'                                            
         DC    AL1(06,76,0,63),C'NY'                                            
         DC    AL1(06,76,0,64),C'NY'                                            
         DC    AL1(06,80,0,62),C'NY'                                            
         DC    AL1(06,80,0,63),C'NY'                                            
         DC    AL1(06,80,0,64),C'NY'                                            
         DC    AL1(13,85,0,64),C'012345678'                                     
         DC    AL1(06,86,0,62),C'YN'                                            
         DC    AL1(07,86,0,63),C'+NY'                                           
         DC    AL1(06,90,0,62),C'NY'                                            
         DC    AL1(10,91,0,62),C' N12+3'                                        
         DC    AL1(07,91,0,63),C' NY'                                           
         DC    AL1(09,91,0,65),C' MN+S'                                         
         DC    AL1(07,91,0,66),C' NY'                                           
         DC    AL1(07,91,0,64),C' NY'                                           
         DC    AL1(07,92,0,36),C'+NY'                                           
         DC    AL1(07,255,0,0),C'+ Y'                                           
         DC    AL1(07,92,0,37),C'+NY'                                           
         DC    AL1(07,255,0,0),C'+ Y'                                           
         DC    AL1(06,96,0,65),C'NY'                                            
         DC    AL1(07,98,0,64),C'NSD'                                           
         DC    AL1(06,103,0,62),C'YN'                                           
         DC    AL1(08,104,0,62),C' NCG'                                         
         DC    AL1(06,104,0,63),C'NY'                                           
         DC    AL1(12,105,0,62),C' 1234567'                                     
**->     DC    AL1(07,106,0,62),C'N+Y'   + = DDS ONLY / DOESN'T WORK            
         DC    AL1(06,106,0,62),C'NY'    QUICK FIX /REPORT IS DDS ONLY          
*                                                   ANYWAY                      
         DC    AL1(07,107,0,11),C' NY'                                          
         DC    AL1(07,109,0,62),C' BG'                                          
         DC    AL1(14,109,0,63),C' 123456789'                                   
         DC    AL1(07,111,0,56),C'PIA'                                          
         DC    AL1(08,112,0,56),C' PIA'                                         
         DC    AL1(10,112,0,57),C'ABCDEF'                                       
         DC    AL1(08,112,0,60),C' ABC'                                         
         DC    AL1(40,112,0,61),C' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXY'         
         DC    AL1(07,112,0,62),C' DS'                                          
         DC    AL1(09,112,0,66),C' 0123'                                        
         DC    AL1(11,112,0,68),C' MCPFLB'    FILM TYPE FILTER                  
         DC    AL1(08,113,0,56),C' PIA'                                         
         DC    AL1(10,113,0,57),C'ABCDEF'                                       
         DC    AL1(08,113,0,60),C' ABC'                                         
         DC    AL1(15,113,0,61),C' 0123456789'                                  
         DC    AL1(07,113,0,62),C' NY'                                          
         DC    AL1(07,113,0,63),C' DS'                                          
         DC    AL1(09,113,0,66),C' 0123'                                        
         DC    AL1(07,114,0,56),C'PIA'                                          
         DC    AL1(10,114,0,57),C'ABCDEF'                                       
         DC    AL1(08,114,0,60),C' ABC'                                         
         DC    AL1(15,114,0,61),C' 0123456789'                                  
         DC    AL1(10,114,0,62),C' NYQJS'                                       
         DC    AL1(07,114,0,63),C' NY'                                          
         DC    AL1(07,114,0,64),C' DS'                                          
         DC    AL1(09,114,0,66),C' 0123'                                        
         DC    AL1(11,114,0,68),C' MCPFLB'    FILM TYPE FILTER                  
*                                  SPREQ02 PUTS F IN 67 IF 68 USED              
         DC    AL1(08,115,0,56),C' PIA'                                         
         DC    AL1(15,115,0,61),C' 0123456789'                                  
         DC    AL1(09,115,0,62),C' NYCP'                                        
         DC    AL1(07,115,0,63),C' NY'                                          
         DC    AL1(07,115,0,64),C' NY'                                          
         DC    AL1(07,115,0,65),C' NY'                                          
         DC    AL1(08,118,0,60),C' ABC'                                         
         DC    AL1(12,118,0,62),C' 1234WMN'                                     
         DC    AL1(09,118,0,63),C' MQ+C'                                        
         DC    AL1(22,118,0,64),C' 123456789ABCDEJKL'                           
         DC    AL1(06,118,0,65),C'YN'                                           
         DC    AL1(06,118,0,66),C' Y'                                           
         DC    AL1(07,118,0,50),C' YN'                                          
         DC    AL1(08,119,0,60),C' ABC'                                         
         DC    AL1(12,119,0,62),C' 1234WMN'                                     
         DC    AL1(10,119,0,63),C' NMQ+C'                                       
         DC    AL1(25,119,0,64),C' 123456789ABCDEFGHJKL'                        
         DC    AL1(07,119,0,65),C' YN'                                          
         DC    AL1(07,127,0,62),C'123'                                          
         DC    AL1(08,128,0,56),C' PIA'                                         
         DC    AL1(06,132,0,62),C'NY'                                           
         DC    AL1(08,132,0,63),C' AIB'                                         
         DC    AL1(07,132,0,64),C' D$'                                          
         DC    AL1(06,132,0,65),C' Y'                                           
         DC    AL1(09,136,0,62),C' ABM3'                                        
         DC    AL1(07,137,0,66),C' YN'                                          
         DC    AL1(07,137,0,67),C' YN'                                          
         DC    AL1(20,138,0,62),C'NY123456789ABCDE'                             
         DC    AL1(09,138,0,63),C' $DNC'                                        
         DC    AL1(06,138,0,64),C'YN'                                           
         DC    AL1(06,138,0,65),C'NY'                                           
         DC    AL1(06,139,0,66),C'NY'                                           
         DC    AL1(07,140,0,50),C' 12'                                          
         DC    AL1(06,140,0,62),C'NY'                                           
         DC    AL1(08,140,0,63),C'DSN$'                                         
         DC    AL1(06,140,0,64),C'YN'                                           
         DC    AL1(06,140,0,65),C'NY'                                           
         DC    AL1(06,142,0,62),C'NY'                                           
         DC    AL1(07,142,0,63),C'D$S'                                          
         DC    AL1(06,142,0,65),C'NY'                                           
         DC    AL1(12,150,0,62),C' 1234567'                                     
         DC    AL1(06,150,0,63),C'NY'                                           
         DC    AL1(06,152,0,58),C' Y'                                           
         DC    AL1(15,152,0,61),C' 0123456789'                                  
         DC    AL1(06,152,0,62),C'NY'                                           
         DC    AL1(12,152,0,63),C' WMERAFQ'                                     
         DC    AL1(06,153,0,58),C' Y'                                           
         DC    AL1(15,153,0,61),C' 0123456789'                                  
         DC    AL1(06,153,0,62),C'NY'                                           
         DC    AL1(12,153,0,63),C'L1234567'                                     
         DC    AL1(06,153,0,64),C'NY'                                           
         DC    AL1(06,154,0,62),C'NY'                                           
         DC    AL1(06,155,0,62),C'NY'                                           
         DC    AL1(06,158,0,63),C'NY'                                           
         DC    AL1(07,158,0,64),C' NY'                                          
         DC    AL1(06,158,0,66),C'NY'                                           
         DC    AL1(06,159,0,62),C' B'                                           
         DC    AL1(06,159,0,65),C'YN'                                           
         DC    AL1(07,160,0,11),C' NY'                                          
         DC    AL1(07,160,0,56),C' PI'                                          
         DC    AL1(16,160,0,62),C' 01234567NAB'                                 
         DC    AL1(14,160,0,63),C' 012345678'                                   
         DC    AL1(06,160,0,64),C'NY'                                           
         DC    AL1(09,160,0,66),C' 0123'                                        
         DC    AL1(07,161,0,11),C' NY'                                          
         DC    AL1(07,161,0,56),C' PI'                                          
         DC    AL1(14,161,0,62),C' 01234567N'                                   
         DC    AL1(14,161,0,63),C' 012345678'                                   
         DC    AL1(06,161,0,64),C'NY'                                           
         DC    AL1(09,161,0,66),C' 0123'                                        
         DC    AL1(07,162,0,11),C' NY'                                          
         DC    AL1(07,162,0,56),C' PI'                                          
         DC    AL1(14,162,0,62),C' 01234567N'                                   
         DC    AL1(14,162,0,63),C' 012345678'                                   
         DC    AL1(06,162,0,64),C'NY'                                           
         DC    AL1(09,162,0,66),C' 0123'                                        
         DC    AL1(07,163,0,11),C' NY'                                          
         DC    AL1(13,163,0,65),C' 01234567'                                    
         DC    AL1(09,163,0,67),C' 0123'                                        
         DC    AL1(11,163,0,62),C' 012345'                                      
         DC    AL1(07,164,0,11),C' NY'                                          
         DC    AL1(08,164,0,62),C' NYO'                                         
         DC    AL1(07,164,0,63),C' NY'                                          
         DC    AL1(09,164,0,64),C' NYRB'                                        
         DC    AL1(13,164,0,66),C' 01234567'                                    
         DC    AL1(09,166,0,57),C' HIJK'                                        
         DC    AL1(08,166,0,60),C' ABC'                                         
         DC    AL1(15,166,0,61),C' 0123456789'                                  
         DC    AL1(07,166,0,62),C' DS'                                          
         DC    AL1(09,166,0,66),C' 0123'                                        
         DC    AL1(09,167,0,57),C' HIJK'                                        
         DC    AL1(08,167,0,60),C' ABC'                                         
         DC    AL1(15,167,0,61),C' 0123456789'                                  
         DC    AL1(07,167,0,62),C' NY'                                          
         DC    AL1(07,167,0,64),C' DS'                                          
         DC    AL1(09,167,0,66),C' 0123'                                        
         DC    AL1(07,168,0,11),C' NY'                                          
         DC    AL1(07,168,0,56),C' PI'                                          
         DC    AL1(14,168,0,62),C' 01234567N'                                   
         DC    AL1(14,168,0,63),C' 012345678'                                   
         DC    AL1(09,168,0,66),C' 0123'                                        
         DC    AL1(07,169,0,56),C'IPU'                                          
         DC    AL1(40,169,0,61),C' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXY'         
         DC    AL1(07,169,0,62),C' NY'                                          
         DC    AL1(07,169,0,63),C'NYS'                                          
         DC    AL1(06,169,0,64),C'NY'                                           
*        DC    AL1(06,169,0,65),C'NY'     WAS EXCEPTIONS ONLY                   
         DC    AL1(09,169,0,66),C' 0123'                                        
         DC    AL1(07,170,0,56),C' PI'                                          
         DC    AL1(06,171,0,65),C' U'                                           
         DC    AL1(06,171,0,67),C'NY'                                           
         DC    AL1(07,172,0,11),C' NY'                                          
         DC    AL1(07,172,0,62),C' NY'                                          
         DC    AL1(06,178,0,62),C' Y'                                           
         DC    AL1(07,178,0,64),C' PS'                                          
         DC    AL1(07,180,0,56),C' PI'                                          
         DC    AL1(15,180,0,61),C' 0123456789'                                  
         DC    AL1(06,180,0,62),C'NY'                                           
         DC    AL1(06,180,0,63),C'NY'                                           
         DC    AL1(06,180,0,64),C'NY'                                           
         DC    AL1(06,180,0,65),C'NY'                                           
         DC    AL1(06,180,0,66),C'NY'                                           
         DC    AL1(06,180,0,67),C'NY'                                           
         DC    AL1(07,192,0,11),C' NY'                                          
         DC    AL1(12,192,0,62),C' OPUBRNC'                                     
         DC    AL1(08,192,0,63),C' NAG'                                         
         DC    AL1(07,192,0,64),C' NY'                                          
         DC    AL1(07,192,0,65),C' NY'                                          
         DC    AL1(07,192,0,66),C' NY'                                          
         DC    AL1(07,192,0,67),C' NY'                                          
         DC    AL1(07,192,0,68),C' YN'                                          
         DC    AL1(07,192,0,81),C' CU'                                          
         DC    AL1(07,193,0,11),C' NY'                                          
         DC    AL1(11,193,0,62),C'OPUBRNC'                                      
         DC    AL1(07,193,0,63),C'NAG'                                          
         DC    AL1(06,193,0,64),C'NY'                                           
         DC    AL1(06,193,0,65),C'NY'                                           
         DC    AL1(06,193,0,66),C'NY'                                           
         DC    AL1(07,193,0,67),C' NY'                                          
         DC    AL1(07,193,0,68),C' YN'                                          
         DC    AL1(12,195,0,62),C' 1234567'                                     
         DC    AL1(09,196,0,63),C' 4567'                                        
         DC    AL1(07,196,0,66),C'+NY'                                          
         DC    AL1(07,255,0,0),C'+T '                                           
         DC    AL1(06,197,0,61),C'NY'                                           
         DC    AL1(08,197,0,62),C' 123'                                         
         DC    AL1(07,197,0,63),C' NY'                                          
         DC    AL1(07,197,0,64),C' NY'                                          
         DC    AL1(07,197,0,65),C' NY'                                          
         DC    AL1(07,197,0,66),C' NY'                                          
         DC    AL1(08,197,0,67),C' 123'                                         
         DC    AL1(12,197,0,68),C' 1234567'                                     
         DC    AL1(06,199,0,62),C'NY'                                           
         DC    AL1(06,199,0,63),C'NY'                                           
         DC    AL1(07,199,0,64),C'NYM'                                          
         DC    AL1(07,203,0,62),C' NY'                                          
         DC    AL1(06,204,0,62),C'NY'                                           
         DC    AL1(06,205,0,62),C'NY'       IN - NISSAN INTERFACE               
         DC    AL1(07,206,0,62),C' NY'                                          
         DC    AL1(07,206,0,63),C' NY'                                          
         DC    AL1(07,206,0,64),C' NY'                                          
         DC    AL1(09,206,0,65),C' NYPB'                                        
         DC    AL1(08,206,0,66),C' PYO'                                         
         DC    AL1(08,206,0,67),C' NYP'                                         
****     DC    AL1(05,207,0,62),C' '                                            
         DC    AL1(19,207,0,62),C' TIUBSXEALO1234'                              
         DC    AL1(09,207,0,63),C' 4567'                                        
         DC    AL1(09,208,0,57),C' ABCD'                                        
         DC    AL1(07,208,0,66),C'ABC'                                          
         DC    AL1(09,209,0,57),C' ABCD'                                        
         DC    AL1(07,209,0,66),C'ABC'                                          
         DC    AL1(30,210,0,62),C'NYABCDEFGHIJKLMOPQRSTUVWXZ'                   
         DC    AL1(11,210,0,66),C' XACNPS'                                      
         DC    AL1(20,211,0,62),C'NY123456789ABCDE'                             
         DC    AL1(09,211,0,63),C' $DNC'                                        
         DC    AL1(06,211,0,64),C'YN'                                           
         DC    AL1(06,211,0,65),C'NY'                                           
         DC    AL1(07,212,0,30),C' YN'                                          
         DC    AL1(07,212,0,61),C' YN'                                          
         DC    AL1(07,212,0,62),C'GNB'                                          
         DC    AL1(10,212,0,63),C' BRLSF'                                       
         DC    AL1(07,212,0,64),C'NYX'                                          
         DC    AL1(08,212,0,65),C' YNS'                                         
         DC    AL1(07,212,0,67),C' YN'                                          
         DC    AL1(08,212,0,68),C' 123'                                         
         DC    AL1(06,213,0,62),C'NY'                                           
         DC    AL1(07,213,0,63),C' NY'                                          
         DC    AL1(06,213,0,64),C' Y'                                           
         DC    AL1(06,214,0,62),C' Y'                                           
         DC    AL1(07,215,0,62),C' GN'                                          
         DC    AL1(13,215,0,63),C' 12345678'                                    
         DC    AL1(08,215,0,64),C' 123'                                         
         DC    AL1(09,215,0,65),C' PURB'                                        
         DC    AL1(10,215,0,66),C' 01234'                                       
         DC    AL1(06,215,0,67),C' Y'                                           
         DC    AL1(07,215,0,30),C' BP'                                          
         DC    AL1(06,215,0,31),C' D'                                           
         DC    AL1(15,217,0,61),C' 0123456789'                                  
         DC    AL1(07,217,0,62),C' CM'                                          
         DC    AL1(06,217,0,63),C'NY'                                           
         DC    AL1(07,217,0,64),C'NYM'                                          
         DC    AL1(06,217,0,65),C'NY'                                           
         DC    AL1(07,217,0,66),C' NY'                                          
         DC    AL1(15,218,0,61),C' 0123456789'                                  
         DC    AL1(07,218,0,62),C' CM'                                          
         DC    AL1(06,218,0,63),C'NY'                                           
         DC    AL1(06,218,0,64),C'NM'                                           
         DC    AL1(06,218,0,65),C'NY'                                           
         DC    AL1(07,218,0,66),C' NY'                                          
         DC    AL1(09,221,0,57),C' HIJK'                                        
         DC    AL1(08,221,0,60),C' ABC'                                         
         DC    AL1(15,221,0,61),C' 0123456789'                                  
         DC    AL1(07,221,0,62),C' NY'                                          
         DC    AL1(07,221,0,64),C' DS'                                          
         DC    AL1(09,221,0,66),C' 0123'                                        
         DC    AL1(09,222,0,63),C' 4567'                                        
         DC    AL1(07,222,0,66),C'+NY'                                          
         DC    AL1(19,223,0,62),C' TIUBSXEALO1234'                              
         DC    AL1(09,223,0,63),C' 4567'                                        
         DC    AL1(08,224,0,65),C'1234'                                         
         DC    AL1(06,224,0,63),C'ND'                                           
         DC    AL1(06,224,0,64),C'NY'                                           
         DC    AL1(06,225,0,62),C'NY'                                           
         DC    AL1(07,225,0,63),C'ESP'                                          
         DC    AL1(06,225,0,64),C'YN'                                           
         DC    AL1(07,225,0,65),C' NY'                                          
         DC    AL1(07,226,0,11),C' NY'                                          
         DC    AL1(12,226,0,62),C' OPUBRNC'                                     
         DC    AL1(08,226,0,63),C' NAG'                                         
         DC    AL1(07,226,0,64),C' NY'                                          
         DC    AL1(07,226,0,65),C' NY'                                          
         DC    AL1(07,226,0,66),C' NY'                                          
         DC    AL1(07,226,0,67),C' NY'                                          
         DC    AL1(07,226,0,68),C' YN'                                          
         DC    AL1(06,227,0,62),C'YN'                                           
         DC    AL1(07,228,0,62),C' NY'                                          
         DC    AL1(07,228,0,63),C' NY'                                          
         DC    AL1(07,228,0,64),C' NY'                                          
         DC    AL1(09,228,0,65),C' NYPB'                                        
         DC    AL1(07,228,0,66),C' PY'                                          
         DC    AL1(08,228,0,67),C' NYP'                                         
         DC    AL1(07,229,0,11),C' NY'          SAME AS 163 D5                  
         DC    AL1(13,229,0,65),C' 01234567'                                    
         DC    AL1(09,229,0,67),C' 0123'                                        
         DC    AL1(11,229,0,62),C' 012345'                                      
         DC    AL1(07,230,0,11),C' NY'                  N2                      
         DC    AL1(11,230,0,62),C' YNFEXO'                                      
         DC    AL1(07,230,0,63),C' NY'                                          
         DC    AL1(08,230,0,64),C' 123'                                         
         DC    AL1(24,230,0,65),C' ABDS0123456789YNFEX'                         
         DC    AL1(08,230,0,66),C' NYP'                                         
         DC    AL1(07,230,0,68),C'NSA'                                          
         DC    AL1(06,231,0,64),C'RI'                                           
         DC    AL1(09,231,0,63),C'CSNOD'                                        
         DC    AL1(07,234,0,66),C' YN'                                          
         DC    AL1(07,234,0,65),C' YN'                                          
         DC    AL1(07,234,0,63),C' OP'                                          
         DC    AL1(07,236,0,62),C'123'                                          
         DC    AL1(06,236,0,63),C' Y'                                           
***      DC    AL1(07,237,0,11),C' NY'             REMOVED 5/10/97              
***      DC    AL1(07,237,0,56),C' PI'                                          
***      DC    AL1(16,237,0,62),C' 01234567NAB'                                 
***      DC    AL1(14,237,0,63),C' 012345678'                                   
***      DC    AL1(06,237,0,64),C'NY'                                           
***      DC    AL1(09,237,0,66),C' 0123'                                        
         DC    AL1(06,237,0,62),C'YN'                ADDED 5/10/97              
         DC    AL1(07,237,0,63),C'YNO'                                          
         DC    AL1(07,237,0,65),C' YN'                                          
         DC    AL1(07,237,0,66),C' YN'                                          
         DC    AL1(07,238,0,62),C'NYT'                                          
         DC    AL1(07,239,0,11),C' NY'                                          
         DC    AL1(07,240,0,11),C' NY'                                          
         DC    AL1(06,240,0,62),C' Y'                                           
         DC    AL1(08,242,0,62),C' YNO'                                         
         DC    AL1(07,242,0,63),C' MA'                                          
         DC    AL1(07,243,0,11),C' NY'                 I2                       
         DC    AL1(10,243,0,62),C' YNFEX'    (ANY CHANGE TO I2 SHOULD)          
         DC    AL1(07,243,0,63),C' NY'        (BE DONE TO N2 BELOW)             
         DC    AL1(08,243,0,64),C' 123'                                         
         DC    AL1(24,243,0,65),C' ABDS0123456789YNFEX'                         
         DC    AL1(08,243,0,66),C' NYP'                                         
         DC    AL1(07,243,0,68),C'NSA'                                          
         DC    AL1(07,245,0,62),C' YN'                                          
         DC    AL1(07,247,0,56),C' PI'                                          
         DC    AL1(07,248,0,64),C' NY'                                          
         DC    AL1(06,248,0,66),C'NY'                                           
         DC    AL1(11,249,0,57),C'ABCDEFG'                                      
         DC    AL1(20,251,0,62),C'NY123456789ABCDE'                             
         DC    AL1(08,251,0,63),C' $DN'                                         
         DC    AL1(06,251,0,64),C'YN'                                           
         DC    AL1(06,251,0,65),C'NY'                                           
         DC    AL1(06,252,0,62),C'NY'                                           
         DC    AL1(11,253,0,57),C'ABCDEFG'                                      
         DC    AL1(06,254,0,62),C'NY'                                           
         DC    X'0000'                                                          
         EJECT                                                                  
REPVL    NTR1  BASE=*,LABEL=*                REP NUMBER - FIND BITS             
         GOTO1 AINITV                        04=NNN                             
         CLI   IFLDH,1                       08=SYND REP                        
         BNE   REPVO                         REP = ALL OR MISSING               
         CLI   IFLDH+5,3                                                        
         BH    REPVE                                                            
*                                                                               
         GOTO1 ARJN                                                             
         CLC   RNUM,=C'I2'                   FOR I2 DONT CHK NUMERIC            
         BE    *+14                                                             
         CLC   FERN,=AL2(FLDNUM)                                                
         BE    REPVE                         REP NON NUMERIC                    
*                                                                               
         MVC   FERN,=AL2(FF)                    IGNORE SIZE                     
         MVC   IFLD(3),TEMP+1                                                   
         OI    FIND,X'04'                    REP = NNN                          
         MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'R'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(3),IFLD                                                   
         MVC   KEYS+5(2),AGY                                                    
         GOTO1 ARSTA                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    REPVO                                                            
         BH    *+14                                                             
         MVC   FERN,=AL2(REPNOF)                  REP NOT ON FILE               
         B     REPVO                                                            
         MVC   NAME(22),SPTREC+18                                               
         CLI   SPTREC+96,C'S'      SYNDICATION REP                              
         BNE   REPVO                                                            
         MVI   FIND,X'09'          RESET FIND FOR SYND REP                      
         B     REPVO                                                            
REPVE    MVC   FERN,=AL2(REPINV)                   REP INVALID                  
REPVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
REPVX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
DTEVL    NTR1  BASE=*,LABEL=*                DATE - FIND BITS                   
DTEVA    GOTO1 AINITV                        04=YYMM 08=YYMMDD                  
         CLI   IFLDH,1                                                          
         BL    DTEVX                         DATE NOT INPUT                     
         BH    DTEVE                                                            
         XC    TEMP+10(8),TEMP+10                                               
DTEVB    CLI   IFLDH+5,2                                                        
         BNE   DTEV                                                             
         CLC   IFLD(2),=C'NO'                                                   
         BNE   DTEVE                                                            
         OI    FIND,X'20'          X'20' = NO                                   
         MVC   TEMP(4),=C'NO  '                                                 
         B     DTEVO                                                            
*                                                                               
DTEV     DS    0H                                                               
         CLC   RNUM,=C'C0'         IF C0 REPORT 2 DATES                         
         BE    DTEVGO                                                           
*                                                                               
DTEVF    CLC   RNUM,=C'A3'      IF A3 REPORT                                    
         BNE   DTEVGO                                                           
         CLI   RPRO1,X'40'    REQ CARD FIELD IS SHARED                          
         BNE   DTEVGO                                                           
         CLI   RPRO1+1,X'40'                                                    
         BE    DTEVGO                                                           
         MVC   FERN,=AL2(FLDINV)                                                
         B     DTEVO                                                            
*                                                                               
DTEVGO   DS    0H                                                               
         GOTO1 DATVAL,PLIST,(0,IFLD),TEMP                                       
         OC    PLIST(4),PLIST                                                   
         BE    DTEV1                                                            
         CLC   RNUM,=C'C0'         FOR C0 REQUEST THIS FORMAT NOT VALID         
         BE    DTEVE                                                            
         OI    FIND,X'08'                    DATE = YYMMDD                      
         B     DTEVO                                                            
DTEV1    GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    DTEVE                                                            
         OI    FIND,X'04'                    DATE = YYMM                        
         CLC   RNUM,=C'C0'                                                      
         BNE   DTEVO                                                            
*                                                                               
         OC    TEMP+10(4),TEMP+10  1ST TIME THROUGH                             
         BNZ   DTEV5                                                            
         MVC   TEMP+10(4),TEMP                                                  
         MVI   ROUTSUB,2           2ND FIELD                                    
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BL    DTEVO                                                            
         BH    DTEVE                                                            
         B     DTEVGO              VALIDATE 2ND FIELD                           
DTEV5    MVC   TEMP+4(4),TEMP                                                   
         MVC   TEMP(4),TEMP+10                                                  
         OI    FIND,X'80'          SPECIAL FOR C0 REQUEST                       
         B     DTEVO                                                            
DTEVE    MVC   FERN,=AL2(DTEINV)                   DATE INVALID                 
*                                                                               
DTEVO    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         TM    FIND,X'80'                                                       
         BZ    *+14                                                             
         MVC   0(8,R7),TEMP                                                     
         B     DTEVX                                                            
*                                                                               
         TM    FIND,X'24'                                                       
         BZ    *+14                                                             
         MVC   0(4,R7),TEMP                                                     
         B     *+10                                                             
*                                                                               
         MVC   0(6,R7),TEMP                                                     
DTEVX    CLI   FIND,0              IF NO INPUT                                  
         BE    DTEVXX              EXIT                                         
         CLC   FERN,=AL2(FF)       IF NO ERROR                                  
         BE    DTEVXX              EXIT                                         
         B     CKRFP               ELSE CHECK RFP                               
DTEVXX   J     XIT                                                              
*                                                                               
CKRFP    DS    0H                                                               
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    DTEVXX                                                           
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   GTFX                                                             
*                                                                               
         L     RF,APARM                 CHECK SYSTEM                            
         L     RF,16(RF)                                                        
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,2                2=SPOT,3=NET                            
         BNE   GTF50                                                            
         DROP  R1,RF                                                            
*                                       SPOT                                    
         CLC   QRFPDICT,=Y(SP#RFPBD)    DATE(YYMMDD) CHECKED FOR MOS            
         BE    *+14                                  LOCKOUT                    
         CLC   QRFPDICT,=Y(SP#ABDAT)    NEW BDATE ADJUSTMENTS                   
         BNE   GTF100                                                           
         OI    FIND,X'08'                                                       
         B     GTF100                                                           
*                                       NET                                     
GTF50    CLC   QRFPDICT,=Y(NE#RFPBD)    DATE(YYMMDD) CHECKED FOR MOS            
         BE    *+14                                  LOCKOUT                    
         CLC   QRFPDICT,=Y(NE#ABDAT)    NEW BDATE ADJUSTMENTS                   
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     GTF100                                                           
*                                                                               
GTF100   LH    R7,COLNUM                STORE ESCAPE SEQ IN REQCARD             
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),SPACES                                                   
         MVC   0(L'QRFPESC,R7),QRFPESC                                          
         MVI   3(R7),6                                                          
         MVC   FERN,=AL2(FF)                                                    
GTFX     B     DTEVXX                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MSCHVL   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   MSCHVX                                                           
         XC    HALF2,HALF2                                                      
*                             TRY TO READ SCHEME RECORD                         
         LA    RF,SPMGRTAB                                                      
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   IFLD(2),0(RF)                                                    
         BE    MSCH30                                                           
         AHI   RF,L'SPMGRTAB                                                    
         BCT   RE,*-14                                                          
         B     MSCHVO              NOT FOUND                                    
*                                                                               
MSCH30   MVC   HALF2(1),2(RF)      PUT IN TRANSLATION VALUE                     
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),KEYS+1         AGY/MED/CLT                              
         CLI   IFLD,C'G'                                                        
         BL    *+10                                                             
         XC    KEY+3(2),KEY+3           CLEAR CLT FOR MGRPS  G-K                
         MVC   KEY+8(1),HALF2                                                   
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    MSCHVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     MSCHVO                                                           
         OI    FIND,X'04'                                                       
*                                                                               
MSCHVO   MVC   KEY(13),KEYS        RESTORE KEY                                  
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),HALF2                                                    
*                                                                               
MSCHVX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
PCTVL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                        04=NN                              
         CLI   IFLDH,1                                                          
         BL    PCTVX                         PCT NOT INPUT                      
         CLI   IFLD,C'E'           MEANS EXCEPTIONS ONLY                        
         BNE   PCTV20                                                           
         CLC   RNUM,=C'D8'         SEE IF D8 - SWEEP REPORT                     
         BNE   PCTVE                                                            
         MVI   RO4,C'Y'            SET ON EXCEPTIONS ONLY OPTION                
         CLI   IFLDH+5,1                                                        
         BE    PCTVW               ACCEPT 'E' ALONE                             
         MVI   IFLD,C'0'           CHG E TO 0 AND GO TO ARJN                    
PCTV20   GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BNE   PCTVE                                                            
         CLI   TEMP,100                                                         
         BL    PCTVO                                                            
PCTVE    MVC   FERN,=AL2(PCTINV)                   PCT INVALID                  
PCTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),TEMP+2                                                   
PCTVW    OI    FIND,X'04'                    PCT = NN                           
PCTVX    J     XIT                                                              
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
AMTVL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                        04=XX...                           
         CLI   FIND,1                                                           
         BL    AMTVX                         AMOUNT MISSING                     
*                                                                               
         CLC   RNUM,=C'A3'            FOR A3 REPORT                             
         BNE   AMTVB                                                            
         CLC   RPRO1(12),=12X'40'    REQUEST CARD FLD IS SHARED                 
         BE    *+14                  MUST BE CLEAR                              
         MVC   FERN,=AL2(FLDINV)                                                
         B     AMTVO                                                            
         CLC   IFLD(2),=C'BF'      A3 ACCEPTS BF                                
         BNE   AMTVB                                                            
         MVC   RPRO1+1(2),IFLD                                                  
         OI    FIND,X'04'                                                       
         B     AMTVX                                                            
*                                                                               
AMTVB    CLC   IFLD(4),=C'PAID'                                                 
         BNE   CKPCT                                                            
         OI    FIND,X'08'          AMOUNT=P-PAID                                
         MVC   TEMP(12),=CL12'PAID'                                             
         B     AMTVO                                                            
*                                                                               
CKPCT    CLC   IFLD(4),=C'PCT='                                                 
         BNE   AMTV1                                                            
         LR    R7,R5                                                            
         SHI   R7,4                                                             
         BNP   AMTVE                                                            
         CHI   R7,5                                                             
         BH    AMTVE                                                            
         GOTO1 CASHVAL,PLIST,IFLD+4,(R7)                                        
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         TM    PLIST+4,X'80'        TEST NEGATIVE                               
         BNZ   AMTVE                                                            
         L     R7,PLIST+4                                                       
         C     R7,=F'100'       LESS THAN 1.00                                  
         BL    AMTVE                                                            
         C     R7,=F'9999'         GREATER THAN 99.99                           
         BH    AMTVE                                                            
         CVD   R7,DUB                                                           
         UNPK  TEMP+4(4),DUB+5(3)                                               
         OI    TEMP+7,X'F0'                                                     
         MVC   TEMP+8(5),=5C' '                                                 
         MVC   TEMP(4),=C'PCT='                                                 
         OI    FIND,X'08'                                                       
         B     AMTVO                                                            
*                                                                               
*                                                                               
AMTV1    CLC   RNUM,=C'A3'     A3 ALLOWS UP TO 4 DECIMALS                       
         BNE   AMTV3                                                            
         GOTO1 CASHVAL,PLIST,(4,IFLD),(R5)                                      
         B     AMTV5                                                            
*                                                                               
AMTV3    GOTO1 CASHVAL,PLIST,IFLD,(R5)                                          
*                                                                               
AMTV5    CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         L     R7,PLIST+4                                                       
         CVD   R7,DUB                                                           
         UNPK  TEMP(10),DUB                                                     
         OI    TEMP+9,X'F0'                                                     
         MVC   TEMP+10(2),=C'  '                                                
         TM    PLIST+4,X'80'                 TEST BINARY NEGATIVE               
         BZ    AMTOK                                                            
         CLC   RNUM,=C'B1'         NO NEGATIVES FOR NEW BILLING                 
         BE    AMTVE                                                            
         MVC   TEMP+10(2),=C'CR'             SET LOGICAL NEGATIVE (CR)          
         NI    TEMP+9,X'DF'       SET TO MINUS                                  
AMTOK    OI    FIND,X'04'                    AMOUNT = XX..                      
         B     AMTVO                                                            
AMTVE    MVC   FERN,=AL2(AMTINV)                   AMOUNT INVALID               
AMTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
*                                                                               
         CLC   RNUM,=C'PR'         FOR PR                                       
         BNE   AMTVO10                                                          
         MVC   0(2,R7),IFLD                                                     
         B     AMTVX                                                            
*                                                                               
AMTVO10  CLC   RNUM,=C'A3'                                                      
         BNE   *+14                                                             
         MVC   0(10,R7),TEMP                                                    
         B     AMTVX                                                            
         MVC   0(12,R7),TEMP                                                    
AMTVX    J     XIT                                                              
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
REALOC   CSECT                                X'04'                             
*                               INPUT=AAA,AAA/30,AAA-BBB/10-20=WGT              
         NMOD1 0,REALOC                                                         
*                                                                               
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
         XC    STATSX,STATSX                                                    
         XC    STATWRX,STATWRX                                                  
         CLC   RCARD2+20(4),=C'DELE'    HAVE WE BEEN HERE ALREADY               
         BE    ROCX                                                             
         CLC   =C'UNMATCHED',RCARD2+20                                          
         BE    ROCX                                                             
         CLC   =C'UNAL',IFLD           NO/CHECK FOR DEL/UNALLOCATE              
         BNE   ROCO0                                                            
         MVC   STATWRX(4),=C'UNAL'                                              
         MVI   RCARD2,X'40'                                                     
         MVC   RCARD2+1(L'RCARD2-1),RCARD2                                      
         OI    FIND,X'04'                                                       
         B     ROCX                                                             
ROCO0    CLC   =C'DELE',IFLD                                                    
         BNE   ROCO00                                                           
         CLI   RMED,C'N'           IF MEDIA = N                                 
         BNE   NOTK1                                                            
         CLC   =C'K1',RNUM         AND K1                                       
         BE    ROCINV              IT'S INVALID                                 
NOTK1    MVC   RCARD2+20(6),=C'DELETE'                                          
         OI    FIND,X'04'                                                       
         B     ROCX                                                             
ROCO00   CLC   =C'UNMATCHED',IFLD      IF UNMATCHED                             
         BNE   ROC1                                                             
         TM    SEDSAVE,X'10'           'ES' IS NOT ACCEPTABLE                   
         BO    INVDAT                                                           
****     TM    PROSAVE,X'08'            AND MUST BE POL                         
****     BNO   POLMUST                                                          
         MVC   RCARD2+20(9),=C'UNMATCHED'                                       
         OI    FIND,X'04'                                                       
         B     ROCX                                                             
ROC1     DS    0H           * SPLIT AAA-BBB/NNN-NNN=NNN                         
         LA    R2,IFLD             FIND =NNN                                    
         SR    R4,R4                                                            
         LA    R5,16                                                            
ROC1A    CLI   0(R2),C'='                                                       
         BE    ROC1B                                                            
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,ROC1A                                                         
         B     ROCINV                                                           
ROC1B    DS    0H                                                               
         LLC   R1,IFLDH+5                                                       
         SR    R1,R4               GET LENGTH OF =NNN                           
         C     R1,=F'4'                                                         
         BH    ROCINV                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STATWRX+91(0),1(R2)    SAVE NNN                                  
         LA    R1,1(R1)                                                         
         STC   R1,STATWRX+90       AND ITS LENGTH                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       CLEAR =NNN FROM IFLD                         
         STC   R4,IFLDH+5          SET NEW LENGTH                               
         LA    R4,8(R4)                                                         
         STC   R4,IFLDH                                                         
*                                                                               
         XC    STATSX,STATSX       HANDLE AAA-BBB/NNN-NNN                       
         LA    R2,STATSX                                                        
         GOTO1 SCANNER,PLIST,IFLDH,(3,STATSX),C',=/-'                           
         LLC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    ROCX                                                             
         C     R5,=F'2'                                                         
         BH    ROCINV                                                           
         CLC   =C'POL',12(R2)       POL NOT ALLOWED                             
         BE    ROCINV                                                           
         CLC   =C'POL',22(R2)                                                   
         BE    ROCINV                                                           
         MVC   KEY+4(3),12(R2)                                                  
         XC    KEY+7(6),KEY+7                                                   
         LA    R4,STATWRX                                                       
ROC5     GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    ROCX                                                             
         BH    *+14                                                             
         MVC   FERN,=AL2(41)           PRODUCT NOT FOUND                        
         B     ROCX                                                             
* NOW SEE IF PROD/EST IS THERE                                                  
         CLC   =C'ALL',REST                                                     
         BE    SKIPIT                                                           
         CLC   =C'NO',REST                                                      
         BE    SKIPIT                                                           
         CLI   REST1,X'40'                                                      
         BNE   SKIPIT                                                           
         PACK  DUB,REST                                                         
         CVB   R1,DUB                                                           
         STC   R1,KEY+7                                                         
         GOTO1 ARSPT                                                            
         BL    ROCX                                                             
         BH    *+14                                                             
         MVC   FERN,=AL2(42)            PROD/EST NOT THERE                      
         B     ROCX                                                             
*                                                                               
SKIPIT   MVC   0(3,R4),12(R2)                                                   
         OI    FIND,X'04'                                                       
         CLI   STATSX+1,0          IS THERE SECOND PRODUCT                      
         BE    ROC10                                                            
         MVI   STATSX+1,0          YES BUT KICK OUT NEXT TIME                   
         LA    R2,10(R2)                                                        
         MVC   KEY+4(3),12(R2)     SET TO READ SECOND PRODUCT                   
         LA    R4,3(R4)                                                         
         B     ROC5                                                             
*                                                                               
ROC10    LA    R2,STATSX+32                                                     
         LA    R4,STATWRX+6                                                     
         CLI   0(R2),0                                                          
         BE    ROC20                                                            
         TM    2(R2),X'80'         NUMERIC                                      
         BNO   ROCINV                                                           
         LLC   R1,7(R2)            CHK VALID LENGTH                             
         BAS   RE,CHKBIN                                                        
         BE    ROCINV                                                           
         MVC   0(3,R4),12(R2)                                                   
         CLI   1(R2),0                                                          
         BE    ROC20                                                            
         TM    3(R2),X'80'         NUMERIC                                      
         BNO   ROCINV                                                           
         LLC   R1,11(R2)                                                        
         BAS   RE,CHKBIN                                                        
         BE    ROCINV                                                           
         MVC   3(3,R4),22(R2)                                                   
ROC20    DS    0H                                                               
         XC    IFLD,IFLD           NOW HANDLE (=)NNN                            
         LLC   R1,STATWRX+90                                                    
         STC   R1,IFLDH+5          SET UP IFLD                                  
         LR    R2,R1                                                            
         LA    R2,8(R2)                                                         
         STC   R2,IFLDH                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),STATWRX+91                                               
         XC    PLIST(16),PLIST                                                  
         XC    STATSX,STATSX                                                    
         GOTO1 SCANNER,PLIST,IFLDH,(2,STATSX)                                   
         LLC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    ROCINV                                                           
         TM    STATSX+2,X'80'          MUST BE NUMERIC                          
         BNO   ROCINV                                                           
         LLC   R1,STATSX+7                                                      
         C     R1,=F'100'                                                       
         BH    ROCINV                                                           
         MVC   STATWRX+12(1),STATSX+7                                           
ROCOK    LH    R7,COLNUM                                                        
         C     R7,=F'0'                    IS IT PRD5                           
         BNE   ROCOCO                                                           
         CLI   STATWRX+8,0                 YES/NO SPOT LENGTHS                  
         BNE   ROCINV                                                           
         CLI   STATWRX+11,0                                                     
         BNE   ROCINV                                                           
         MVC   RNUM+152(3),STATWRX                                              
         MVC   RNUM+155(3),STATWRX+3                                            
         MVC   RNUM+158(1),STATWRX+12                                           
         B     ROCX                                                             
ROCOCO   LA    R7,RNUM(R7)                                                      
         MVC   0(13,R7),STATWRX                                                 
ROCX     XC    PLIST(16),PLIST                                                  
XIT1     XIT1                                                                   
*                                                                               
ROCINV   MVC   FERN,=AL2(FLDINV)                                                
         B     ROCX                                                             
INVDAT   MVC   FERN,=AL2(20)       INVALID DATE FORMAT                          
         B     ROCX                                                             
POLMUST  MVC   FERN,=AL2(101)      PROD MUST BE POL                             
         B     ROCX                                                             
*                                                                               
CHKBIN   NTR1                                                                   
         STC   R1,FULL                                                          
         L     R1,SLNTAB                                                        
         LH    RE,0(R1)              GET ENTRY LENGTH                           
         L     RF,2(R1)              DISPL TO EOT                               
         AR    RF,R1                 RELOCATE EOT ADDRESS                       
         AHI   R1,6                  POINT TO FIRST ENTRY                       
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'R'                                                          
         CLI   RMED,C'R'                                                        
         BE    CB10                                                             
         CLI   RMED,C'X'                                                        
         BE    CB10                                                             
         LA    R0,C'T'                                                          
*                                                                               
CB10     CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CB15                                                             
         CLC   RAGY,0(R1)          MATCH AGY                                    
         BNE   *+12                                                             
CB15     CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    CB20                                                             
*                                                                               
         BXLE  R1,RE,CB10          NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CB20     AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,FULL             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BILLNM   CSECT                                                                  
         NMOD1 0,BILLNM,RR=R2                                                   
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
         ST    R2,BILRELO                                                       
         B     BILLGO                                                           
BILRELO  DS    F'0'                                                             
*                                                                               
BILLGO   DS    0H                                                               
         LA    R5,SPTREC+500                                                    
         XC    SPTREC+500(100),SPTREC+500  CLEAR ENOUGH FOR SCANNER             
         USING SCAND,R5                                                         
         LA    R4,IFLDH                                                         
         GOTO1 SCANNER,PLIST,(R4),(10,SPTREC+500)                               
         LLC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    BILLX                                                            
*                                                                               
         CLC   RNUM,=C'07'        FOR SPOT/NET UNBILLING DIFFR RULES            
         BE    *+14                                                             
         CLC   RNUM,=C'7U'                                                      
         BNE   BILLGO5                                                          
*                                                                               
         CLI   FLD1LEN,4          ONLY 1 INVOICE (LAST 4 DIGITS)                
         BNE   BILLINV                                                          
         XC    DMCB,DMCB                                                        
         GOTO1 =V(SPFMTINO),DMCB,,(C'P',FLD1),RR=BILRELO                        
         L     RF,DMCB+4                                                        
         OC    0(2,RF),0(RF)       IF NULLS -> INVALID                          
         BZ    BILLINV                                                          
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),FLD1                                                     
         LA    R5,32(R5)           BUMP TO 2ND SCANNER OUT FIELD                
         CLI   FLD1LEN,0           SHOULD BE NO 2ND NUMBER                      
         BNE   BILLINV                                                          
         B     BILLOK                                                           
*                                                                               
BILLGO5  CLI   FLD1LEN,6           1ST NUMBER                                   
         BNE   BILLINV                                                          
         GOTO1 =V(HEXIN),DMCB,FLD1,TEMP,2,RR=BILRELO                            
         OC    DMCB+12(4),DMCB+12  IS FIELD VALID?                              
         BZ    BILLINV                                                          
         XC    DMCB,DMCB                                                        
         GOTO1 =V(SPFMTINO),DMCB,,(C'P',FLD1+2),RR=BILRELO                      
         L     RF,DMCB+4                                                        
         OC    0(2,RF),0(RF)       IF NULLS -> INVALID                          
         BZ    BILLINV                                                          
         MVC   TEMP+1(2),0(RF)       SAVE HEX VALUE FOR COMPARE                 
         MVC   RCARD2+20(6),FLD1                                                
*                                                                               
         LA    R5,32(R5)           BUMP TO 2ND SCANNER OUT FIELD                
         CLI   FLD1LEN,0           2ND NUMBER                                   
         BE    BILLOK                                                           
         CLI   FLD1LEN,6                                                        
         BNE   BILLINV                                                          
         GOTO1 =V(HEXIN),DMCB,FLD1,TEMP+10,2,RR=BILRELO                         
         OC    DMCB+12(4),DMCB+12  IS FIELD VALID?                              
         BZ    BILLINV                                                          
         XC    DMCB,DMCB                                                        
         GOTO1 =V(SPFMTINO),DMCB,,(C'P',FLD1+2),RR=BILRELO                      
         L     RF,DMCB+4                                                        
         OC    0(2,RF),0(RF)       IF NULLS -> INVALID                          
         BZ    BILLINV                                                          
         MVC   TEMP+11(2),0(RF)                                                 
         CLC   TEMP(3),TEMP+10     COMPARE WITH 1ST INV #                       
         BH    BILLINV                                                          
         MVC   RCARD2+26(6),FLD1                                                
         B     BILLOK                                                           
*                                                                               
BILLOK   OI    FIND,X'04'                                                       
         B     BILLX                                                            
*                                                                               
BILLINV  MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
BILLX    XIT1                                                                   
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PROGRM   CSECT                                                                  
         NMOD1 0,PROGRM,RR=R2                                                   
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
         MVC   RCARD2+20(20),IFLD                                               
         OI    FIND,X'04'                                                       
PROGX    XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
OPTRTN   CSECT                                                                  
         NMOD1 0,OPTION,RR=R2                                                   
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,4095(RA)                                                      
         LA    R8,1(R8)                                                         
         USING OPTRTN,RB,RA,R8        NOTE 3 BASE REGISTERS RB,RA,R8            
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
         ST    R2,OPTRELO                                                       
         B     OPTGO                                                            
OPTRELO  DS    F'0'                                                             
*                                                                               
***GO    CLI   IFLD,C'?'                                                        
***      BE    OPTHELP                                                          
OPTGO    CLC   RNUM,=C'BT'         IF BT GO TO SPECIFIC ROUTINE                 
         BNE   *+8                                                              
         BRAS  RE,BTVAL                                                         
*                                                                               
         BRAS  RE,COMMOPT                                                       
*                           AT THIS POINT UNIVERSAL OPTIONS ARE SET             
*                           OPTION FIELD IS RESCANNED TO SEE IF                 
*                           SPECIFIC REQUEST VALIDATION IS REQUIRED             
*                           UNIVERSAL OPTIONS ARE IGNORED HERE                  
         LA    R5,SPTREC+500                                                    
         L     R4,FLDHADR                                                       
         GOTO1 SCANNER,PLIST,(20,(R4)),(10,SPTREC+500)                          
         LLC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    OPTX                                                             
         LA    R6,1                                                             
         L     R4,=A(REQATBL)      YES/GET ADDR OF VALROUTINE                   
         A     R4,OPTRELO                                                       
OPT250   CLC   RNUM,0(R4)                                                       
         BNE   OPT300                                                           
         L     R2,4(R4)                                                         
         LR    R4,R1               PASS NUM OF SCANNER FLDS IN R4               
         A     R2,OPTRELO                                                       
         BR    R2                                                               
OPT300   LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   OPT250                                                           
         B     OPTX                NO MATCH ON RNUM                             
         EJECT                                                                  
*                                                                               
* INPUT: R5 POINTS TO SCANNER                                                   
*        R6 POSITION WITHIN SCANNER FOR ERROR ROUTINE                           
*        R4 CONTROLS LOOP IN SCANNER                                            
*                                                                               
         USING SCAND,R5                                                         
A2VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    A2V100                                                           
         BAS   RE,D1TO8                                                         
         BE    A2V100                                                           
         CLI   FLD1,C'S'          S=C' YN' (REQ2,COL22)                         
         BNE   A2V15                                                            
         BAS   RE,VONEFLD                                                       
         BNE   OPTINV                                                           
         B     A2V99                                                            
*                                                                               
A2V15    CLI   FLD1,C'G'         G=BF/G=NNN.NNNN (Q2,COL23)                     
         BNE   A2V25             G=GROSS/G=NET                                  
         CLI   FLD1LEN,1                                                        
         BNE   A2V25                                                            
         CLC   =C'BF',FLD2                    G=BF                              
         BNE   A2V16                                                            
         CLI   FLD2LEN,2                                                        
         BNE   OPTINV                                                           
         B     A2V19                                                            
A2V16    CLC   =C'NET',FLD2                   G=NET                             
         BNE   A2V16B                                                           
         CLI   FLD2LEN,3                                                        
         BNE   OPTINV                                                           
         B     A2V19                                                            
A2V16B   CLC   =C'GROSS',FLD2                 G=GROSS                           
         BNE   A2V17                                                            
         CLI   FLD2LEN,5                                                        
         BNE   OPTINV                                                           
         B     A2V19                                                            
A2V17    LLC   R2,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(4,FLD2),(R2)    G=NNN.NNNN                         
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    OPTINV                                                           
         C     R2,=F'9999999'                MAX=9999999                        
         BH    OPTINV                                                           
         CLI   FLD2LEN,4           IF MORE THAN 3 DIGITS, NEED DECIMAL          
         BL    A2V19                                                            
         LA    R2,4                                                             
         LA    R1,FLD2                                                          
         CLI   0(R1),C'.'                                                       
         BE    A2V19                                                            
         LA    R1,1(R1)                                                         
         BCT   R2,*-12                                                          
         B     OPTINV                                                           
A2V19    MVC   RCARD2+22(8),FLD2                                                
         B     A2V99                                                            
*                                                                               
A2V25    DS    0H                                                               
*&&DO                                                                           
         CLC   =C'GST',FLD1        GST=Y/I/O                                    
         BNE   A2V30                                                            
         CLI   FLD2,C'Y'                                                        
         BE    A2V27                                                            
         CLI   FLD2,C'I'                                                        
         BE    A2V27                                                            
         CLI   FLD2,C'O'                                                        
         BE    A2V27                                                            
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
A2V27    MVC   RCARD2+2(1),FLD2                                                 
         B     A2V99                                                            
*&&                                                                             
*                                                                               
A2V30    DS    0H                                                               
         CLC   =C'CV',FLD1         CLIENT VERSION                               
         BNE   A2V40                                                            
         MVI   RCARD2+18,C'Y'                                                   
         B     A2V99                                                            
*                                                                               
A2V40    DS    0H                                                               
         CLC   =C'NOTOT',FLD1      NOTOTAL OPTION                               
         BNE   A2V44                                                            
         MVI   RCARD2+30,C'Y'                                                   
         B     A2V99                                                            
                                                                                
A2V44    DS    0H                                                               
         CLC   =C'RATE',FLD1       RATE=A-Z OPTION   FOR A2 REPORT              
         BNE   A2V46                                                            
         CLC   =C'A2',RNUM                                                      
         BNE   OPTINV                                                           
         CLI   FLD2,C'A'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'Z'                                                        
         BH    OPTINV                                                           
         MVC   RCARD2+31(1),FLD2                                                
         B     A2V99                                                            
*                                                                               
A2V46    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   OPTINV                                                           
         MVI   RCARD2+15,C'Y'                                                   
*****    B     A2V99                                                            
*                                                                               
*****A2V47    DS    0H                                                          
*                                                                               
*****A2V49    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
A2V99    OI    FIND,X'04'                                                       
*                                                                               
A2V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,A2VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
RZVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    RZV100                                                           
         CLI   FLD1,C'R'        R=D5,RS   (COL101-102)                          
         BNE   OPTINV                                                           
         CLC   =C'D2',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'D3',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'D4',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'D5',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'D6',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'DC',FLD2                                                      
         BE    RZV10                                                            
         CLC   =C'RS',FLD2                                                      
         BNE   OPTINV                                                           
RZV10    MVC   RNUM+100(2),FLD2                                                 
         OI    FIND,X'02'                                                       
         B     OPTX                                                             
*                                                                               
*****RZV15    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
RZV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,RZVAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
A7VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    A7V100                                                           
         CLC   =C'CABLE',FLD1       CABLE = NO                                  
*****    BNE   OPTINV                                                           
         BNE   A7V15                                                            
         CLC   =C'ONLY',FLD2        CABLE = ONLY?                               
         BNE   A7V01                NO                                          
         MVI   RSTA+4,C'/'          YES - CABLE ONLY REQUEST                    
         B     A7V02                                                            
*                                                                               
A7V01    CLC   =C'NO',FLD2                                                      
         BNE   OPTINV                                                           
         MVI   RSTA+4,C'-'                                                      
A7V02    OI    FIND,X'04'                                                       
*****    B     OPTX                                                             
         B     A7V100                                                           
*                                                                               
A7V15    DS    0H                                                               
         CLC   =C'TOTAL',FLD1       TOTALS ?                                    
         BNE   A7V100                                                           
         MVI   RO5,C'Y'                                                         
         OI    FIND,X'04'                                                       
*                                                                               
A7V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,A7VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
B1VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    B1V100                                                           
         BAS   RE,D1TO8                                                         
         BE    B1V100                                                           
         CLC   =C'R',FLD1       R(ETAIL)=N   COL64                              
         BNE   B1V15                                                            
         CLC   =C'REG',FLD1        CAN CONFUSE                                  
         BE    B1V15                                                            
         BAS   RE,VONEFLD                                                       
         BNE   OPTINV                                                           
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V15    DS    0H                                                               
         CLC   =C'COM',FLD1                                                     
         BE    B1V17                                                            
         CLC   =C'NET',FLD1                                                     
         BE    B1V17                                                            
         CLC   =C'REG',FLD1                                                     
         BNE   B1V20                                                            
B1V17    MVC   RO6,FLD1                                                         
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V20    DS    0H                                                               
         CLC   =C'PKG',FLD1        PKG= FOR BU/DU  ONLY                         
         BNE   B1V25                                                            
         CLC   RNUM,=C'BU'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'DU'                                                      
         BNE   B1V25                                                            
         CLI   FLD2LEN,5                                                        
         BH    OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BL    OPTINV                                                           
         MVC   R2USER+6(5),FLD2                                                 
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V25    DS    0H                                                               
         CLC   =C'ERR',FLD1     ERROR=Y?                                        
         BNE   B1V30                                                            
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
         CLC   RNUM,=C'D1'                                                      
         BNE   OPTINV                                                           
         MVI   RO4,C'E'                                                         
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
                                                                                
B1V30    DS    0H                                                               
         CLC   =C'PRIOR',FLD1      PRIOR=NA (N=4-11,A=T)                        
         BNE   B1V35                                                            
         LA    R3,FLD2                                                          
         CLI   FLD2LEN,2                                                        
         BE    B1V31                                                            
         CLI   FLD2LEN,3                                                        
         BE    B1V32                                                            
         B     OPTINV                                                           
*****    CLI   FLD2,C'4'           FLD2LEN = 2                                  
B1V31    CLI   FLD2,C'1'           FLD2LEN = 2                                  
         BL    OPTINV                                                           
         CLI   FLD2,C'9'                                                        
         BH    OPTINV                                                           
         MVI   R2USER+22,C'0'                                                   
         MVC   R2USER+23(1),FLD2                                                
         LA    R3,1(R3)                                                         
         B     B1V33                                                            
B1V32    CLC   =C'10',FLD2         FLD2LEN=3                                    
         BE    *+14                                                             
         CLC   =C'11',FLD2                                                      
         BNE   OPTINV                                                           
         MVC   R2USER+22(2),FLD2                                                
         LA    R3,2(R3)                                                         
B1V33    CLI   0(R3),C'T'                                                       
         BE    *+12                                                             
         CLI   0(R3),C'S'                                                       
         BNE   OPTINV                                                           
*                                                                               
         MVC   R2USER+24(1),0(R3)                                               
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V35    DS    0H                                                               
         CLC   =C'GSKEST',FLD1    GSK ESTIMATE COMMENT                          
         BNE   B1V40                                                            
         CLI   FLD2,C'A'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'D'                                                        
         BH    OPTINV                                                           
         MVC   R2USER+44(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V40    DS    0H                                                               
         CLC   =C'TRADE',FLD1     TRADE REQUST?                                 
         BNE   *+12                                                             
         LA    RE,RCARD2+31                                                     
         B     *+18                                                             
*                                                                               
         CLC   =C'MTRADE',FLD1    GROUP M TRADE REQUST?                         
         BNE   B1V45                                                            
         LA    RE,R2USER+20                                                     
*                                                                               
         CLI   DDS,1              ALLOW SOON FOR DDS                            
         BE    B1V40D                                                           
         CLC   RNUM,=C'D1'                                                      
         BNE   OPTINV                                                           
         L     RF,ASAVE                                                         
         USING T208FFD,RF                                                       
         CLC   =C'SOON',BVROUT     ONLY FOR SOON                                
         BNE   OPTINV                                                           
         DROP  RF                                                               
B1V40D   CLI   0(RE),C'C'          IS IT OUR SITUATION ?                        
         BE    B1V43                                                            
         CLC   =C'TRADE',FLD1      TRADE REQUST?                                
         BNE   OPTINV                                                           
         TM    CLISAVE,X'80'       IS IT GMI CLT ?                              
         BNO   OPTINV                                                           
*        B     B1V43                                                            
*                                                                               
*        TM    DEMS,COP4TRD        GROUP M TRADE?                               
*        BNO   OPTINV                                                           
B1V43    MVI   0(RE),C'T'          YES, PUT T INSTEAD OF C                      
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V45    DS    0H                                                               
         CLC   =C'ADJSEP',FLD1     OVERRIDING B2 PROFILE SEP ADJUSTMNT          
         BNE   B1V50                                                            
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   R2USER+43(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V50    DS    0H                                                               
         CLC   =C'CORP',FLD1       CORP= FOR BU/DU  ONLY                        
         BNE   B1V55                                                            
         CLC   RNUM,=C'BU'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'DU'                                                      
         BNE   OPTINV                                                           
         CLI   FLD1LEN,4                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,3                                                        
         BH    OPTINV                                                           
         CLI   FLD2LEN,2                                                        
         BL    OPTINV                                                           
         TM    FLD2VAL,X'80'       TEST NUMERIC                                 
         BO    OPTINV                                                           
         CLC   FLD2(3),=C'POL'     DON'T ACCEPT POL                             
         BE    OPTINV                                                           
         MVC   KEYS(13),KEY        KEY SHOULD BE FILLED IN                      
         XC    KEY,KEY                                                          
         MVC   KEY(4),KEYS           AGY/MED/CLT                                
         MVC   KEY+4(3),FLD2                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FF)                                                    
         BNE   OPTINV                                                           
         MVC   KEY,KEYS            RESTORE                                      
         MVC   R2USER+25(3),FLD2                                                
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V55    DS    0H                                                               
         CLC   =C'WEEK',FLD1       WEEK= 01 TO 99                               
         BNE   B1V60                                                            
         CLC   RNUM,=C'BU'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'DU'                                                      
         BNE   OPTINV                                                           
*                                                                               
         TM    FLD2VAL,X'80'       NUMERIC                                      
         BNO   OPTINV                                                           
         CLI   FLD2LEN,2                                                        
         BH    OPTINV                                                           
         CLC   FLD2B,=F'99'                                                     
         BH    OPTINV                                                           
         MVC   R2USER+30(2),=C'00'                                              
         CLI   FLD2LEN,1                                                        
         BNE   *+14                                                             
         MVC   R2USER+31(1),FLD2                                                
         B     *+10                                                             
*                                                                               
         MVC   R2USER+30(2),FLD2                                                
*                                                                               
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V60    CLC   =C'F-RATE',FLD1     FRATE REQUEST?                               
         BNE   B1V65                                                            
         CLC   RNUM,=C'D1'                                                      
         BNE   OPTINV                                                           
         TM    CLISAVE,X'80'       IS IT GMI CLT ?                              
         BO    OPTINV              F-RATE IS NOT ALLOWED                        
         L     RF,ASAVE                                                         
         USING T208FFD,RF                                                       
         CLC   =C'SOON',BVROUT     ONLY FOR SOON                                
         BNE   OPTINV                                                           
         DROP  RF                                                               
         MVI   RCARD2+30,C'F'      YES, PUT F AND WILL DO MORE CHECK            
         OI    FIND,X'04'          IN SPREQ02 OVERLAY                           
         B     B1V100                                                           
*                                                                               
B1V65    CLC   =C'GMBILL',FLD1     GM LMG BILLING                               
         BNE   B1V70                                                            
         TM    CLISAVE,X'04'       IF FOR SINGLE CLIENT                         
         BZ    *+12                                                             
         TM    DEMS,X'10'          BPCT ON THE CLIENT REC?                      
         BZ    B1V65ERR                                                         
*                                                                               
         CLI   FLD2,C'L'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'R'                                                        
         BNE   B1V65ERR                                                         
         MVC   R2USER+32(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V65ERR MVC   FERN,=AL2(FLDINV)                                                
         B     OPTX                                                             
*                                                                               
B1V70    DS    0H                                                               
         CLC   =C'IREG',FLD1       REQUESTING INVOICE REGISTER?                 
         BNE   B1V75                                                            
         MVI   R2USER+28,C'R'                                                   
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V75    DS    0H                                                               
         CLC   =C'WBF',FLD1        WB FLIGHT FILTER                             
         BNE   B1V80                                                            
         CLI   FLD2LEN,10                                                       
         BNE   OPTINV              MUST BE 10 CHARACTERS                        
         TM    FLD2VAL,X'80'       MUST BE NUMERIC                              
         BZ    OPTINV                                                           
         MVC   R2USER+33(10),FLD2                                               
         OI    FIND,X'04'                                                       
         B     B1V100                                                           
*                                                                               
B1V80    DS    0H                                                               
         CLC   =C'CURRENT',FLD1    REQUESTING FLIGHT DATES                      
         BNE   OPTINV                                                           
         CLC   =C'FLIGHT',FLD2                                                  
         BNE   OPTINV                                                           
         USING T208FFD,RF                                                       
         CLC   =C'SOON',BVROUT     FOR SOON DISALLOW                            
         BE    OPTINV              THIS OPTION                                  
         DROP  RF                                                               
         TM    RFPSTAT,RFPINUSE    SEE IF $RFP IN USE                           
         BZ    OPTINV              ONLY FOR RFP                                 
         MVI   R2USER+29,C'C'      CURRENT AND PRIOR                            
         CLI   FLD1LEN,7                                                        
         BE    B1V85                                                            
         CLC   =C'CURRENT+1',FLD1                                               
         BNE   OPTINV                                                           
         CLI   FLD1LEN,9                                                        
         BNE   OPTINV                                                           
         MVI   R2USER+29,C'1'      CURRENT + NEXT AND PRIOR                     
B1V85    OI    FIND,X'04'                                                       
*****    B     B1V100                                                           
*                                                                               
*****B1V90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
B1V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,B1VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D2VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D2V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D2V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL65)                               
         BNE   D2V10                                                            
         MVI   RNUM+64,C'Y'                                                     
         B     D2V99                                                            
*                                                                               
D2V10    DS    0H                                                               
         LA    RE,RCARD2+24        FOR MCOM                                     
         CLC   =C'MCOM',FLD1                                                    
         BE    D2V12                                                            
         LA    RE,30(RE)           FOR SCOM                                     
         CLC   =C'SCOM',FLD1                                                    
         BNE   D2V20                                                            
D2V12    CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   0(1,RE),FLD2                                                     
**       MVC   RCARD2+24(1),FLD2                                                
         B     D2V99                                                            
*                                                                               
*                                                                               
D2V20    DS    0H                                                               
         BAS   RE,D23467           CLIENT VERSION/MAKEGOOD ANALYSIS             
         BE    D2V100                                                           
*                                                                               
D2V30    DS    0H                                                               
         CLC   =C'NET',FLD1        NET                                          
         BNE   *+12                                                             
         MVI   RCARD2+36,C'Y'                                                   
         B     D2V33                                                            
         CLC   =C'BILL',FLD1        BILL                                        
         BNE   *+12                                                             
         MVI   RCARD2+36,C'B'                                                   
         B     D2V33                                                            
         CLC   =C'RPT',FLD1        RPT=1-6                                      
         BNE   D2V40                                                            
         CLI   FLD2,C'1'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'6'                                                        
         BH    OPTINV                                                           
         MVC   RCARD2+37(1),FLD2                                                
D2V33    B     D2V99                                                            
*                                                                               
*                                                                               
D2V40    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   D2V42                                                            
         MVI   RCARD2+15,C'Y'                                                   
         B     D2V99                                                            
*                                                                               
D2V42    DS    0H                                                               
         CLC   =C'UNW',FLD1                                                     
         BNE   D2V60                                                            
         CLI   FLD1LEN,3                                                        
         BE    *+14                                                             
         CLC   =C'UNWIND',FLD1             UNWIND=M+-                           
         BNE   D2V60                  * POSSIBLE ENTRIES: M+-                   
*                                                                               
         MVI   RCARD2+39,C'U'         *                   M+                    
         CLI   FLD2LEN,3              *                   M-                    
         BH    OPTINV                 *                   +-                    
         BL    D2V45                  *                   M                     
         CLC   =C'M+-',FLD2           *                   +                     
         BNE   OPTINV                 *                   -                     
         MVC   RCARD2+40(3),=C'YYY'                                             
         B     D2V99                                                            
D2V45    CLI   FLD2LEN,2                                                        
         BL    D2V55                                                            
         CLI   FLD2,C'M'                                                        
         BE    D2V47                                                            
         CLC   =C'+-',FLD2                                                      
         BNE   OPTINV                                                           
         MVC   RCARD2+41(2),=C'YY'                                              
         B     D2V99                                                            
D2V47    CLC   =C'M+',FLD2                                                      
         BNE   *+16                                                             
         MVI   RCARD2+40,C'Y'                                                   
         MVI   RCARD2+42,C'Y'                                                   
         B     D2V99                                                            
*                                                                               
         CLC   =C'M-',FLD2                                                      
         BNE   OPTINV                                                           
         MVC   RCARD2+40(2),=C'YY'                                              
         B     D2V99                                                            
*                                                                               
D2V55    CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'M'                                                        
         BNE   *+12                                                             
         MVI   RCARD2+40,C'Y'                                                   
         B     D2V99                                                            
*                                                                               
         CLI   FLD2,C'+'                                                        
         BNE   *+12                                                             
         MVI   RCARD2+42,C'Y'                                                   
         B     D2V99                                                            
*                                                                               
         CLI   FLD2,C'-'                                                        
         BNE   OPTINV                                                           
         MVI   RCARD2+41,C'Y'                                                   
         B     D2V99                                                            
*                                                                               
D2V60    DS    0H                                                               
         CLC   =C'UNWPER',FLD1        UNWPER=MMMDD/YY-MMMDD/YY                  
         BNE   D2V65                                                            
         XC    TEMP,TEMP              USE TEMP FOR PERVALD                      
         LLC   R7,FLD2LEN                                                       
         GOTO1 =V(PERVAL),PLIST,((R7),FLD2),TEMP,RR=OPTRELO                     
         CLI   PLIST+4,0                                                        
         BNE   OPTINV                                                           
         LA    R1,TEMP             A(OUTPUT)                                    
         USING PERVALD,R1                                                       
         CLI   PVALASSM,0          2 FULL DATES ENTERED?                        
         BNE   OPTINV                                                           
         MVC   RCARD2+43(12),PVALESTA                                           
         B     D2V99                                                            
         DROP  R1                                                               
*                                                                               
D2V65    DS    0H                                                               
         CLC  =C'UDEF',FLD1        UDEF?                                        
         BNE   OPTINV                                                           
         CLC   =C'E1',FLD2         E1?                                          
         BNE   D2V65C                                                           
         OI    R2USER+16,X'01'                                                  
         B     D2V99                                                            
*                                                                               
D2V65C   DS    0H                                                               
         CLC   =C'E2',FLD2         E2?                                          
         BNE   D2V65G                                                           
         OI    R2USER+16,X'02'                                                  
         B     D2V99                                                            
*                                                                               
D2V65G   CLI   FLD2,C'E'           E?                                           
         BNE   OPTINV                                                           
         OI    R2USER+16,X'03'                                                  
*****    B     D2V99                                                            
*                                                                               
*****D2V90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D2V99    OI    FIND,X'04'                                                       
*                                                                               
D2V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D2VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D3VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D3V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D3V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL65)                               
         BNE   D3V10                                                            
         MVI   RNUM+64,C'Y'                                                     
         OI    FIND,X'04'                                                       
         B     D3V100                                                           
*                                                                               
D3V10    DS    0H                                                               
         CLC   =C'MCOM',FLD1                                                    
         BNE   D3V20                                                            
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+24(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     D3V100                                                           
*                                                                               
D3V20    DS    0H                                                               
         BAS   RE,D23467           CLIENT VERSION/MAKEGOOD ANALYSIS             
         BE    D3V100                                                           
*                                                                               
D3V30    DS    0H                                                               
         CLC   =C'NET',FLD1        NET                                          
         BNE   *+12                                                             
         MVI   RCARD2+36,C'Y'                                                   
         B     D3V32                                                            
         CLC   =C'BILL',FLD1       BILL                                         
         BNE   OPTINV                                                           
         MVI   RCARD2+36,C'B'                                                   
D3V32    OI    FIND,X'04'                                                       
*****    B     D3V100                                                           
*                                                                               
*****D3V40    B     OPTINV                                                      
*                                                                               
D3V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D3VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D4VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D4V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D4V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL65)                               
         BNE   D4V10                                                            
         MVI   RNUM+64,C'Y'                                                     
         B     D4V99                                                            
*                                                                               
D4V10    DS    0H                                                               
         LA    RE,RCARD2+24        FOR MCOM                                     
         CLC   =C'MCOM',FLD1                                                    
         BE    D4V12                                                            
         LA    RE,30(RE)           FOR SCOM                                     
         CLC   =C'SCOM',FLD1                                                    
         BNE   D4V20                                                            
D4V12    CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   0(1,RE),FLD2                                                     
         B     D4V99                                                            
*                                                                               
D4V20    DS    0H                                                               
         BAS   RE,D23467           CLIENT VERSION/MAKEGOOD ANALYSIS             
         BE    D4V100                                                           
*                                                                               
D4V30    DS    0H                                                               
         CLC   =C'NET',FLD1        NET                                          
         BNE   *+12                                                             
         MVI   RCARD2+36,C'Y'                                                   
         B     D4V32                                                            
         CLC   =C'BILL',FLD1        BILL                                        
         BNE   D4V40                                                            
         MVI   RCARD2+36,C'B'                                                   
D4V32    B     D4V99    '                                                       
*                                                                               
D4V40    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   D4V50                                                            
         MVI   RCARD2+15,C'Y'                                                   
         B     D4V99                                                            
*                                                                               
D4V50    DS    0H                                                               
         CLC   =C'UNW',FLD1                                                     
         BNE   D4V70                                                            
         CLI   FLD1LEN,3                                                        
         BE    *+14                                                             
         CLC   =C'UNWIND',FLD1             UNWIND=M+-                           
         BNE   D4V70                  * POSSIBLE ENTRIES: M+-                   
*                                                                               
         MVI   RCARD2+39,C'U'         *                   M+                    
         CLI   FLD2LEN,3              *                   M-                    
         BH    OPTINV                 *                   +-                    
         BL    D4V55                  *                   M                     
         CLC   =C'M+-',FLD2           *                   +                     
         BNE   OPTINV                 *                   -                     
         MVC   RCARD2+40(3),=C'YYY'                                             
         B     D4V99                                                            
D4V55    CLI   FLD2LEN,2                                                        
         BL    D4V60                                                            
         CLI   FLD2,C'M'                                                        
         BE    D4V57                                                            
         CLC   =C'+-',FLD2                                                      
         BNE   OPTINV                                                           
         MVC   RCARD2+41(2),=C'YY'                                              
         B     D4V99                                                            
D4V57    CLC   =C'M+',FLD2                                                      
         BNE   *+16                                                             
         MVI   RCARD2+40,C'Y'                                                   
         MVI   RCARD2+42,C'Y'                                                   
         B     D4V99                                                            
*                                                                               
         CLC   =C'M-',FLD2                                                      
         BNE   OPTINV                                                           
         MVC   RCARD2+40(2),=C'YY'                                              
         B     D4V99                                                            
*                                                                               
D4V60    CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'M'                                                        
         BNE   *+12                                                             
         MVI   RCARD2+40,C'Y'                                                   
         B     D4V99                                                            
*                                                                               
         CLI   FLD2,C'+'                                                        
         BNE   *+12                                                             
         MVI   RCARD2+42,C'Y'                                                   
         B     D4V99                                                            
*                                                                               
         CLI   FLD2,C'-'                                                        
         BNE   OPTINV                                                           
         MVI   RCARD2+41,C'Y'                                                   
         B     D4V99                                                            
*                                                                               
D4V70    CLC   =C'UNWPER',FLD1        UNWPER=MMMDD/YY-MMMDD/YY                  
         BNE   OPTINV                                                           
         XC    TEMP,TEMP              USE TEMP FOR PERVALD                      
         LLC   R7,FLD2LEN                                                       
         GOTO1 =V(PERVAL),PLIST,((R7),FLD2),TEMP,RR=OPTRELO                     
         CLI   PLIST+4,0                                                        
         BNE   OPTINV                                                           
         LA    R1,TEMP             A(OUTPUT)                                    
         USING PERVALD,R1                                                       
         CLI   PVALASSM,0          2 FULL DATES ENTERED?                        
         BNE   OPTINV                                                           
         MVC   RCARD2+43(12),PVALESTA                                           
*****    B     D4V99                                                            
         DROP  R1                                                               
*                                                                               
*****D4V90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D4V99    DS    0H                                                               
         OI    FIND,X'04'                                                       
*                                                                               
D4V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D4VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D5VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D5V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D5V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL68)                               
         BNE   D5V10                                                            
         MVI   RNUM+67,C'Y'                                                     
         OI    FIND,X'04'                                                       
         B     D5V100                                                           
*                                                                               
D5V10    DS    0H                                                               
         CLC   =C'MCOM',FLD1                                                    
         BNE   D5V20                                                            
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+24(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     D5V100                                                           
*                                                                               
D5V20    DS    0H                                                               
         CLC   =C'MGA',FLD1         MAKE GOOD ANALYSIS                          
         BNE   OPTINV                                                           
         MVI   RCARD2+17,C'Y'                                                   
         OI    FIND,X'04'                                                       
*****    B     D5V100                                                           
*                                                                               
*****D5V30    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D5V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D5VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
D6VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D6V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D6V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL65)                               
         BNE   D6V10                                                            
         MVI   RNUM+64,C'Y'                                                     
         B     D6V99                                                            
*                                                                               
D6V10    DS    0H                                                               
         BAS   RE,D23467           CLIENT VERSION/MAKEGOOD ANALYSIS             
         BE    D6V100                                                           
*                                                                               
D6V20    DS    0H                                                               
         CLC   =C'MCOM',FLD1                                                    
         BNE   D6V30                                                            
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+24(1),FLD2                                                
         B     D6V99                                                            
*                                                                               
D6V30    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   D6V40                                                            
         MVI   RCARD2+15,C'Y'                                                   
         B     D6V99                                                            
*                                                                               
D6V40    DS    0H                                                               
         CLC   =C'UDEF',FLD1       UDEF?                                        
         BNE   OPTINV                                                           
         CLC   =C'E1',FLD2         E1?                                          
         BNE   D6V40C                                                           
         OI    R2USER+16,X'01'                                                  
         B     D6V99                                                            
*                                                                               
D6V40C   DS    0H                                                               
         CLC   =C'E2',FLD2         E2?                                          
         BNE   D6V40G                                                           
         OI    R2USER+16,X'02'                                                  
         B     D6V99                                                            
*                                                                               
D6V40G   CLI   FLD2,C'E'           E?                                           
         BNE   OPTINV                                                           
         OI    R2USER+16,X'03'                                                  
*****    B     D6V99                                                            
*                                                                               
*****D6V90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D6V99    DS    0H                                                               
         OI    FIND,X'04'                                                       
*                                                                               
D6V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D6VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
J6VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    J6V100                                                           
         CLC   =C'NOCOM',FLD1       NOCOM=N(COL62)                              
         BNE   OPTINV                                                           
         MVI   RNUM+61,C'N'                                                     
         OI    FIND,X'04'                                                       
*****    B     J6V100                                                           
*                                                                               
*****J6V30    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
J6V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,J6VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D7VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D7V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D7V100                                                           
         CLC   =C'NOC',FLD1       NOCOST=Y(COL65)                               
         BNE   D7V10                                                            
         MVI   RNUM+64,C'Y'                                                     
         B     D7V99                                                            
*                                                                               
D7V10    DS    0H                                                               
         CLC   =C'COM',FLD1        COM=NO (RCARD2+35=2)                         
         BNE   D7V20                                                            
         CLC   =C'NO',FLD2                                                      
         BNE   D7V20                                                            
         MVI   RCARD2+35,C'N'                                                   
         B     D7V99                                                            
*                                                                               
D7V20    DS    0H                                                               
         BAS   RE,D23467           CLIENT VERSION/MAKEGOOD ANALYSIS             
         BE    D7V100                                                           
*                                                                               
D7V30    DS    0H                                                               
         CLC   =C'DEM',FLD1        DEM=Y IN COL39 OF 2ND REQ                    
         BNE   D7V40                                                            
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    D7V30D                                                           
         CLC   RNUM,=C'DX'         FOR DX NOW ACCEPT Y,N,1,2,3,4                
         BNE   OPTINV                                                           
         CLI   FLD2,C'N'                                                        
         BE    D7V30D                                                           
         CLI   FLD2,C'1'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'4'                                                        
         BH    OPTINV                                                           
D7V30D   MVC   RCARD2+38(1),FLD2                                                
         B     D7V99                                                            
*                                                                               
D7V40    DS    0H                                                               
         CLC   =C'SCOM',FLD1                                                    
         BNE   D7V45                                                            
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+54(1),FLD2                                                
         B     D7V99                                                            
*                                                                               
D7V45    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   OPTINV                                                           
         MVI   RCARD2+15,C'Y'                                                   
*****    B     D7V99                                                            
*                                                                               
*****D7V90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D7V99    OI    FIND,X'04'                                                       
D7V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D7VAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
D8VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    D8V100                                                           
         BAS   RE,D1TO8                                                         
         BE    D8V100                                                           
         CLC   =C'PRG',FLD1       PRG TYPE=N                                    
         BNE   D8V10                                                            
         MVC   RNUM+58(1),FLD2                                                  
         OI    FIND,X'04'                                                       
         B     D8V100                                                           
*                                                                               
D8V10    DS    0H                                                               
         CLC   =C'NOCOST',FLD1       NO COST                                    
         BNE   D8V11                                                            
         MVI   RCARD2+21,C'Y'                                                   
         OI    FIND,X'04'                                                       
         B     D8V100                                                           
                                                                                
D8V11    DS    0H                                                               
         CLC   =C'LKUP',FLD1         LKUP=ALL                                   
         BNE   D8V12                                                            
         CLC   =C'ALL',FLD2                                                     
         BNE   D8V12                                                            
         MVI   RCARD2+23,C'Y'                                                   
         OI    FIND,X'04'                                                       
         B     D8V100                                                           
                                                                                
D8V12    DS    0H                                                               
         CLC   =C'COST',FLD1          COST                                      
         BNE   OPTINV                                                           
         MVI   RCARD2+21,C'N'                                                   
         OI    FIND,X'04'                                                       
*****    B     D8V100                                                           
*                                                                               
*****D8V14    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
D8V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,D8VAL                                                         
         B     OPTX                                                             
*                                                                               
DNVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    DNV100                                                           
         CLC   =C'BRD',FLD1       BRD                                           
         BNE   OPTINV                                                           
         MVI   RNUM+67,C'B'                                                     
         OI    FIND,X'04'                                                       
*****    B     DNV100                                                           
*                                                                               
*****DNV10    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
DNV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,DNVAL                                                         
         B     OPTX                                                             
*                                                                               
DLVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    DLV100                                                           
         CLC   =C'IMPS',FLD1       IMPS=Y                                       
         BNE   DLV10                                                            
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
         MVI   RNUM+62,C'Y'                                                     
         OI    FIND,X'04'                                                       
         CLI   RNUM+61,C'Y'        MUST HAVE POST=Y                             
         BE    DLV100              OK                                           
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** REQUIRES POST=Y *'                               
         B     OPTX                                                             
*                                                                               
DLV10    CLC   =C'MKTDEM',FLD1      MKTDEM                                      
         BNE   OPTINV                                                           
         MVI   RO3,C'Y'                                                         
         OI    FIND,X'04'                                                       
*        B     DLV100                                                           
*LV20    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
DLV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,DLVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
* - COMMON ROUTINE FOR D2,D3,D4,D6,D7                                           
D23467   DS    0H                                                               
         CLC   =C'CV',FLD1         CLIENT VERSION                               
         BNE   D234A                                                            
         CLI   RCARD2+18,X'40'     MAKE SURE NO CONFLICT WITH WIM               
         BHR   RE                                                               
         MVI   RCARD2+18,C'Y'                                                   
         B     D234OK                                                           
*                                                                               
D234A    DS    0H                                                               
         CLC   =C'MGA',FLD1         MAKE GOOD ANALYSIS                          
         BNE   D234B                                                            
         MVI   RCARD2+17,C'Y'                                                   
         B     D234OK                                                           
*                                                                               
D234B    DS    0H                                                               
         CLC   =C'WIM',FLD1                                                     
         BNER  RE                                                               
         CLI   RCARD2+18,X'40'     MAKE SURE NO CONFLICT WITH CV                
         BHR   RE                                                               
         MVI   RCARD2+18,C'N'                                                   
*                                                                               
D234OK   OI    FIND,X'04'                                                       
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         BR    RE                                                               
                                                                                
* - COMMON ROUTINE FOR D1 THROUGH D8 AND A2                                     
* - *** DO NOT USE R4,R5,R6 ***                                                 
D1TO8    DS    0H                                                               
         CLC   =C'ORIG',FLD1         ORIG=Y                                     
         BNE   D1T805                                                           
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
         MVI   RCARD2+19,C'Y'                                                   
         B     D1T8OK                                                           
                                                                                
D1T805   CLC   =C'TRD',FLD1           TRD=Y/N                                   
         BNE   D1T8XX                                                           
         TM    CLISAVE,X'04'          MUST BE SPECIFIC CLIENT                   
         BNO   OPTINV                                                           
         TM    PROSAVE,X'04'          MUST BE SPECIFIC PRODUCT                  
         BNO   OPTINV                                                           
         CLI   PROSAVE+2,C'#'         AND 3D CHAR CAN NOT = #                   
         BE    OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+76(1),FLD2                                                
*                                                                               
D1T8OK   OI    FIND,X'04'                                                       
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
D1T8XX   BR    RE                                                               
         EJECT                                                                  
*                                                                               
RNVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    RNV100                                                           
         CLC   =C'PNAME',FLD1      PNAME=Y                                      
         BNE   OPTINV                                                           
         MVI   RCARD2+20,C'Y'                                                   
         OI    FIND,X'04'                                                       
*        B     RNV100                                                           
*                                                                               
*NV10    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
RNV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,RNVAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
RSVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    RSV100                                                           
         CLC   =C'PNAME',FLD1       PNAME                                       
         BNE   RSV10                                                            
         MVI   RCARD2+20,C'Y'                                                   
         OI    FIND,X'04'                                                       
         B     RSV100                                                           
*                                                                               
RSV10    DS    0H                                                               
         CLC   =C'MGA',FLD1         MAKE GOOD ANALYSIS                          
         BNE   RSV20                                                            
         MVI   RCARD2+17,C'Y'                                                   
         OI    FIND,X'04'                                                       
         B     RSV100                                                           
*                                                                               
RSV20    DS    0H                                                               
         CLC   =C'PB',FLD1          PIGGYBACK OPTION                            
         BNE   OPTINV                                                           
         CLI   FLD2,C'N'                                                        
         BE    RSV24                                                            
         LA    R6,2                 FIELD 2 NG                                  
         B     OPTINV                                                           
RSV24    DS    0H                                                               
         CLI   RPRO1,C' '           2ND PRODUCT ?                               
         BNH   RSV20OK              NO                                          
         MVC   FERN,=AL2(PBNVAL)    PB= OPTION NOT VALID FOR PIGGYBACK          
         B     OPTX                                                             
RSV20OK  DS    0H                                                               
         MVI   R2USER+5,C'Y'                                                    
         OI    FIND,X'04'                                                       
*****    B     RSV100                                                           
*                                                                               
*****RSV90    DS    0H                                                          
*****         B     OPTINV                                                      
*                                                                               
RSV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,RSVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
M2VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    M2V100                                                           
         CLC   =C'CV',FLD1         CLIENT VERSION                               
         BNE   M2V20                                                            
         MVI   RCARD2+18,C'Y'                                                   
         B     M2V99                                                            
*                                                                               
M2V20    DS    0H                                                               
         CLC   =C'NET',FLD1        NET                                          
         BNE   *+12                                                             
         MVI   RCARD2+36,C'Y'                                                   
         B     M2V99                                                            
         CLC   =C'BILL',FLD1        BILL                                        
         BNE   M2V30                                                            
         MVI   RCARD2+36,C'B'                                                   
         B     M2V99                                                            
*                                                                               
M2V30    DS    0H                                                               
         CLC   =C'GL',FLD1          GLOCK OPTION                                
         BNE   M2V40                                                            
         MVI   RO6,C'Y'                                                         
         B     M2V99                                                            
*                                                                               
M2V40    DS    0H                                                               
         CLC   =C'UDEF',FLD1       UDEF?                                        
         BNE   OPTINV                                                           
         CLC   =C'E1',FLD2         E1?                                          
         BNE   M2V40C                                                           
         OI    R2USER+17,X'01'                                                  
         B     M2V99                                                            
*                                                                               
M2V40C   DS    0H                                                               
         CLC   =C'E2',FLD2         E2?                                          
         BNE   M2V40G                                                           
         OI    R2USER+17,X'02'                                                  
         B     M2V99                                                            
*                                                                               
M2V40G   CLI   FLD2,C'E'           E?                                           
         BNE   OPTINV                                                           
         OI    R2USER+17,X'03'                                                  
*        B     M2V99                                                            
*                                                                               
*2V90    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
M2V99    OI    FIND,X'04'                                                       
                                                                                
M2V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,M2VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
M3VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    M3V100                                                           
         CLC   =C'CV',FLD1         CLIENT VERSION                               
         BNE   M3V20                                                            
         MVI   RCARD2+18,C'Y'                                                   
         B     M3V99                                                            
*                                                                               
M3V20    DS    0H                                                               
         CLC   =C'GL',FLD1          GLOCK OPTION                                
         BNE   M3V30                                                            
         MVI   RO6,C'Y'                                                         
         B     M3V99                                                            
*                                                                               
M3V30    DS    0H                                                               
         CLC   =C'UDEF',FLD1       UDEF?                                        
         BNE   OPTINV                                                           
         CLC   =C'M9',RNUM         IS IT M9?                                    
         BNE   OPTINV                                                           
         CLC   =C'E1',FLD2         E1?                                          
         BNE   M3V30C                                                           
         OI    R2USER,X'01'                                                     
         B     M3V99                                                            
*                                                                               
M3V30C   DS    0H                                                               
         CLC   =C'E2',FLD2         E2?                                          
         BNE   M3V30G                                                           
         OI    R2USER,X'02'                                                     
         B     M3V99                                                            
*                                                                               
M3V30G   CLI   FLD2,C'E'           E?                                           
         BNE   OPTINV                                                           
         OI    R2USER,X'03'                                                     
*        B     M3V99                                                            
*                                                                               
*3V90    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
M3V99    DS    0H                                                               
         OI    FIND,X'04'                                                       
*                                                                               
M3V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,M3VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
M4VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    M4V100                                                           
         CLI   FLD1,C'F'        F=ALPHA  COL68  FILM TYPE                       
         BNE   M4V15                  (IF USED F IN COL67)                      
         CLC   RAGY,=C'MC'         ONLY FOR MCANN                               
         BNE   OPTINV                                                           
         CLI   FLD2,C'A'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'Z'                                                        
         BH    OPTINV                                                           
         OI    FIND,X'04'                                                       
         MVI   RNUM+66,C'F'                                                     
         MVC   RNUM+67(1),FLD2                                                  
         B     M4V100                                                           
*                                                                               
M4V15    DS    0H                                                               
         CLC   =C'CV',FLD1         CLIENT VERSION                               
         BNE   M4V20                                                            
         MVI   RCARD2+18,C'Y'                                                   
         OI    FIND,X'04'                                                       
         B     M4V100                                                           
*                                                                               
M4V20    DS    0H                                                               
         CLC   =C'GL',FLD1          GLOCK OPTION                                
         BNE   OPTINV                                                           
         MVI   RO6,C'Y'                                                         
         OI    FIND,X'04'                                                       
*        B     M4V100                                                           
*                                                                               
*4V90    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
M4V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,M4VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
MLVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    MLV100                                                           
         CLC   =C'GLOCK',FLD1      GLOCK = Y, N, G                              
         BNE   OPTINV              ONLY OPTION FOR NOW                          
         CLC   =C'ONLY',FLD2                                                    
         BNE   *+12                                                             
         MVI   RO1,C'G'                                                         
         B     MLV20                                                            
*                                                                               
         CLI   FLD2,C'N'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
*                                                                               
         MVC   RO1,FLD2                                                         
MLV20    OI    FIND,X'04'                                                       
*        B     MLV100                                                           
*                                                                               
*LV99    DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
MLV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,MLVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
R48VL    DS    0H                  48 REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    R48Z                                                             
         CLC   =C'CAB',FLD1              CABLE ONLY                             
         BNE   R48B                                                             
R48A     MVC   RSTA(5),=C'ALL /'                                                
         B     R48OK                                                            
R48B     CLI   FLD1,C'C'                   CABLE ONLY                           
         BE    R48A                                                             
*                                                                               
         CLC   =C'BROAD',FLD1             BROADCAST ONLY                        
         BNE   R48D                                                             
R48C     MVC   RSTA(5),=C'ALL -'                                                
         B     R48OK                                                            
R48D     CLI   FLD1,C'B'                   BROADCAST ONLY                       
         BE    R48C                                                             
                                                                                
         CLC   =C'EIX',FLD1        EIX OPTION                                   
         BNE   R48G                                                             
         CLI   FLD2,C'Y'                                                        
         BE    R48E                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
R48E     MVC   RNUM+65(1),FLD2         FOR S48                                  
         B     R48OK                                                            
*                                                                               
R48G     CLC   =C'DOWN',FLD1       DOWN OPTION                                  
         BNE   R48J                                                             
         CLI   FLD1LEN,4           HAS TO BE EXACT MATCH                        
         BNE   OPTINV                                                           
         CLI   RO1,C'S'            THIS OPT IS ONLY VALID FOR STATIONS          
         BNE   OPTINV                                                           
         MVI   RO6,C'D'                                                         
         MVC   REQOUT,=C'DOWN  '   SET OUTPUT TYPE TO DOWNLOAD                  
         B     R48OK                                                            
*                                                                               
R48J     CLC   =C'LOCK',FLD1       LOCK=Y OPTION                                
         BNE   OPTINV                                                           
         MVI   RO6+1,C'Y'                                                       
         B     R48OK                                                            
*                                                                               
R48OK    OI    FIND,X'04'                                                       
         B     R48Z                                                             
*                                                                               
R48Z     DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,R48VL                                                         
         B     OPTX                                                             
         SPACE 3                                                                
*                                                                               
RXDVL    DS    0H                  XD REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    RXDZ                                                             
         CLC   =C'AFF',FLD1              AFF =                                  
         BNE   RXDB                                                             
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         LA    RE,RXDTBL                                                        
RXDA     CLI   0(RE),0                                                          
         BE    OPTINV                                                           
         CLC   0(1,RE),FLD2                                                     
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     RXDA                                                             
         MVC   RNUM+57(1),FLD2                                                  
         OI    FIND,X'04'                                                       
         B     RXDZ                                                             
*                                                                               
RXDTBL   DC    C'INCAFWU'                                                       
         DC    X'00'                                                            
*                                                                               
RXDB     CLC   =C'DPT',FLD1             DPT =                                   
         BNE   OPTINV                                                           
         MVC   RCARD2+20(1),FLD2                                                
         OI    FIND,X'04'                                                       
*        B     RXDZ                                                             
*                                                                               
*XDD     B     OPTINV                                                           
*                                                                               
RXDZ     DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,RXDVL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
MGVAL    DS    0H                  MG REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    MGVZ                                                             
         CLC   =C'MTOT',FLD1                                                    
         BNE   MGVB                                                             
MGVA     MVC   RO3,FLD2                                                         
         CLI   FLD2,X'40'                                                       
         BNH   OPTINV                                                           
         B     MGVOK                                                            
*                                                                               
MGVB     DS    0H                                                               
         CLC   =C'BREAK',FLD1                                                   
         BNE   OPTINV                                                           
         CLI   FLD2,C'M'           BREAK BY MARKET                              
         BNE   *+12                                                             
         MVI   RO6,C'M'                                                         
         B     MGVOK                                                            
         CLI   FLD2,C'S'           STATION IS DEFAULT                           
         BNE   OPTINV                                                           
*                                                                               
*GVC     B     OPTINV                                                           
*                                                                               
*                                                                               
MGVOK    DS    0H                                                               
         OI    FIND,X'04'                                                       
*                                                                               
MGVZ     DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,MGVAL                                                         
         B     OPTX                                                             
*                                                                               
C1VAL    DS    0H                  C1 AND 48 VAL FOR EIX OPTION                 
         BRAS  RE,VALUNIS                                                       
         BE    C1VZ                                                             
         CLC   =C'EIX',FLD1                                                     
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    C1VD                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
C1VD     MVC   RNUM+63(1),FLD2         FOR C1                                   
*                                                                               
C1VOK    OI    FIND,X'04'                                                       
*        B     C1VZ                                                             
*                                                                               
*1VB     B     OPTINV                                                           
*                                                                               
*                                                                               
C1VZ     DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,C1VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
NVVAL    DS    0H                  NV REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    NVZ                                                              
         CLC   =C'CLT',FLD1        CLT=ALL                                      
         BNE   NVB                                                              
         CLC   =C'ALL',FLD2                                                     
         BNE   NVB                                                              
         MVI   RNUM+66,C'Y'        Y IN COL 67                                  
         OI    FIND,X'04'                                                       
         B     NVZ                                                              
*                                                                               
NVB      DS    0H                                                               
         CLC   =C'EASI',FLD1       EASI                                         
         BNE   NVC                                                              
         CLI   FLD2,C'Y'                                                        
         BE    NVB1                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
NVB1     MVC   RNUM+59(1),FLD2                                                  
         OI    FIND,X'04'                                                       
         B     NVZ                                                              
*                                                                               
NVC      DS    0H                                                               
         CLC   =C'TOTAL',FLD1       TOTAL                                       
         BNE   NVD                                                              
         MVI   RO5,C'Y'                                                         
         OI    FIND,X'04'                                                       
         B     NVZ                                                              
*                                                                               
NVD      DS    0H                                                               
         CLC   =C'ETYPE',FLD1       EST TYPE B/S/U/*                            
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'B'                                                        
         BE    NVF                                                              
         CLI   FLD2,C'S'                                                        
         BE    NVF                                                              
         CLI   FLD2,C'U'                                                        
         BE    NVF                                                              
         CLI   FLD2,C'*'                                                        
         BNE   OPTINV                                                           
*                                                                               
NVF      MVC   R2USER(1),FLD2      PUT THAT CHAR IN                             
         OI    FIND,X'04'                                                       
*        B     NVZ                                                              
*                                                                               
*                                                                               
*VY      DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
NVZ      DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,NVVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
K2VAL    DS    0H                  NV REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    K2Z                                                              
         CLC   =C'BUYS',FLD1       BUYS=T                                       
         BNE   K2B                                                              
         CLC   =C'DF',RAGY         FOR DF ONLY                                  
         BNE   OPTINV                                                           
         CLI   FLD2,C'T'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'C'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+20(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     K2Z                                                              
*                                                                               
K2B      DS    0H                                                               
         CLC   =C'DAYPART',FLD1     DAYPART=X                                   
         BE    *+14                                                             
         CLC   =C'DPT',FLD1                                                     
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1           2ND FIELD HAS TO BE 1 BYTE                   
         BNE   OPTINV              IF NOT -> ERROR                              
         MVC   RO6,FLD2            PUT THE VALUE INTO QOPT4                     
         OI    FIND,X'04'                                                       
*        B     K2Z                                                              
*                                                                               
*2R      DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
K2Z      DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,K2VAL                                                         
         B     OPTX                                                             
*                                                                               
K1VAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    K1Z                                                              
         CLC   =C'DAYPART',FLD1     DAYPART=X                                   
         BE    *+14                                                             
         CLC   =C'DPT',FLD1                                                     
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1           2ND FIELD HAS TO BE 1 BYTE                   
         BNE   OPTINV              IF NOT -> ERROR                              
         MVC   RO3,FLD2            PUT THE VALUE INTO QOPT3                     
         OI    FIND,X'04'                                                       
*        B     K1Z                                                              
*                                                                               
*1R      DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
K1Z      DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,K1VAL                                                         
         B     OPTX                                                             
*                                                                               
K4VAL    DS    0H                  NV REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    K4Z                                                              
         CLC   =C'SPLIT',FLD1      SPLIT='X'                                    
         BNE   OPTINV                                                           
         TM    FLD2VAL,X'C0'       ALPHA NUMERIC                                
         BZ    OPTINV                                                           
         MVC   RNUM+67(1),FLD2     'X' IN COL 68                                
         OI    FIND,X'04'                                                       
*        B     K4Z                                                              
*                                                                               
*4B      DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
K4Z      DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,K4VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
K5VAL    DS    0H                  NV REPORT                                    
         BRAS  RE,VALUNIS                                                       
         BE    K5Z                                                              
         CLC   =C'SPLIT',FLD1      SPLIT='X'                                    
         BNE   K5B                                                              
         TM    FLD2VAL,X'C0'       ALPHA NUMERIC                                
         BZ    OPTINV                                                           
         MVC   RNUM+67(1),FLD2     'X' IN COL 68                                
         OI    FIND,X'04'                                                       
         B     K5Z                                                              
*                                                                               
K5B      DS    0H                                                               
         CLC   RNUM,=C'K5'         K5 CAN HAVE REASON CODE                      
         BNE   OPTINV                                                           
         CLC   =C'RS',FLD1        REASON CODE                                   
         BNE   K5C                                                              
         MVC   RCARD2+20(4),FLD2                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D77'                                                  
         MVC   KEY+2(2),RAGY                                                    
         MVI   KEY+4,C'N'                                                       
         MVC   KEY+5(4),FLD2                                                    
         OC    KEY+5(6),SPACES                                                  
         MVC   KEYD,KEY                USE DEMO KEY FOR KEYSAVE                 
         GOTO1 DATAMGR,PLIST,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                  
         CLC   KEYD(13),KEY                                                     
         BE    K5BB                                                             
         MVC   FERN,=AL2(1123)     INVALID REASON CODE                          
         B     OPTX                                                             
K5BB     OI    FIND,X'04'                                                       
         B     K5Z                                                              
*                                                                               
K5C      DS    0H                                                               
         CLC   =C'SUBMEDIA',FLD1   K5 HAS SUBMEDIA NOW (C OR S)                 
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1           HAS TO BE 1 CHAR LONG                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'C'           CABLE ?                                      
         BNE   *+12                                                             
         MVI   RO5,C'C'                                                         
         B     K5BB                TURN ON FIND BIT                             
         CLI   FLD2,C'S'           SYNDICATION ?                                
         BNE   OPTINV                                                           
         MVI   RO5,C'S'                                                         
         B     K5BB                TURN ON FIND BIT                             
*                                                                               
*5F      DS    0H                                                               
*        B     OPTINV                                                           
*                                                                               
K5Z      DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,K5VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
I2VAL    DS    0H                      I2 REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    I2V10                                                            
*                                                                               
* INT= OPTION FOR N2 ONLY                                                       
         CLC   RNUM,=C'N2'         FOR N2 ONLY                                  
         BNE   I2INTX                                                           
         CLC   =C'INTONLY',FLD1                                                 
         BNE   *+12                                                             
         MVI   R2USER+2,C'O'                                                    
         B     I2INTOK                                                          
         CLC   =C'INT',FLD1                                                     
         BNE   I2INTX                                                           
         MVI   R2USER+2,C'I'                                                    
I2INTOK  OI    FIND,X'04'                                                       
         B     I2V10                                                            
                                                                                
I2INTX   EQU   *                                                                
* ID= OPTION FOR I2 ONLY                                                        
         CLC   =C'ID',FLD1         ID=5 DIGIT NUMBER                            
         BNE   I2V0                                                             
         CLC   RNUM,=C'I2'         FOR I2 REPORT ONLY                           
         BNE   I2V0                                                             
         CLC   =C'CK',RAGY         FOR AGENCY CK ONLY                           
         BNE   OPTINV                                                           
         TM    FLD2VAL,X'80'       MUST BE NUMBERIC                             
         BZ    OPTINV                                                           
         CLI   FLD2LEN,5           MUST BE FIVE DIGITS                          
         BNE   OPTINV                                                           
         MVC   RNUM+49(5),FLD2                                                  
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V0     CLC   =C'RSP',FLD1            RSP OR RESPONSE                          
         BE    *+14                                                             
         CLC   =C'RESP',FLD1                                                    
         BNE   I2V1                                                             
         CLI   RNUM+32,X'40'       SHARES BYTE WITH MCT OPTION                  
         BH    OPTINV                                                           
         MVI   RNUM+32,C'R'                                                     
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V1     DS    0H                                                               
         CLC   FLD1(2),=C'M '           M=N                                     
         BNE   I2V2                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVI   RNUM+33,C'N'                                                     
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V2     DS    0H                                                               
         CLC   FLD1(3),=C'RRS'     RRS=Y                                        
         BNE   I2V3                                                             
         CLI   FLD2,C'Y'                                                        
         BNE   *+8                                                              
         MVI   R2USER,C'R'                                                      
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
                                                                                
I2V3     DS    0H                                                               
         CLC   =C'MCT',FLD1        MCT (SHARES FIELD WITH 'RESPONSE')           
         BNE   I2V4                                                             
         CLI   RNUM+32,X'40'                                                    
         BH    OPTINV                                                           
         MVI   RNUM+32,C'M'                                                     
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V4     DS    0H                                                               
         CLC   =C'LETT',FLD1      LETTER=Y/N                                    
         BNE   I2V6                                                             
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RCARD2+20(1),FLD2                                                
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V6     DS    0H                                                               
         CLC   =C'MOS',FLD1        MOS=C/B                                      
         BNE   I2V8                                                             
         MVI   RCARD2+21,C'B'                                                   
         CLI   FLD2,C'B'                                                        
         BE    I2V6B                                                            
         MVI   RCARD2+21,C'C'                                                   
         CLI   FLD2,C'C'                                                        
         BNE   OPTINV                                                           
I2V6B    OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V8     DS    0H                                                               
         CLC   =C'AUTOPAY',FLD1                                                 
         BNE   I2V9                                                             
         BNE   OPTINV                                                           
         MVI   RNUM+31,C'Z'                                                     
         OI    FIND,X'04'                                                       
         B     I2V10                                                            
*                                                                               
I2V9     DS    0H                                                               
         CLC   =C'REPXCL',FLD1                                                  
         BNE   OPTINV                                                           
         MVI   RCARD2+26,C'Y'                                                   
         OI    FIND,X'04'                                                       
*                                                                               
I2V10    DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,I2VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
L2VAL    DS    0H                      L2 REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    L2V10                                                            
                                                                                
         DS    0H                                                               
         CLC   =C'OOWR',FLD1     OOWR = SHOW ONLY OUT OF WEEK ROTATORS          
         BNE   OPTINV                                                           
         MVI   RCARD2+20,C'Y'                                                   
         OI    FIND,X'04'                                                       
*                                                                               
L2V10    DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,L2VAL                                                         
         B     OPTX                                                             
*                                                                               
PMVAL    DS    0H                      PM REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    PMV100                                                           
*                                                                               
         DS    0H                                                               
         CLC   =C'NET',FLD1     NET (LIKE D2)                                   
         BNE   PMV03                                                            
         MVI   RCARD2+36,C'Y'                                                   
         B     PMV99                                                            
*                                                                               
PMV03    DS    0H                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   PM04                                                             
         MVI   RCARD2+15,C'Y'                                                   
         B     PMV99                                                            
*                                                                               
PM04     DS    0H                                                               
         CLC   =C'REP',FLD1     REPORT BUY REP?                                 
         BNE   PMV90            NO                                              
         CLI   FLD2,C'Y'        REP=Y?                                          
         BE    *+12             YES                                             
         CLI   FLD2,C'B'        REP=B(BOTH REP AND REP NAME)?                   
         BNE   PMV90            NO                                              
         MVC   R2USER(1),FLD2                                                   
         B     PMV99                                                            
                                                                                
PMV90    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
PMV99    DS    0H                                                               
         OI    FIND,X'04'                                                       
*                                                                               
PMV100   DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,PMVAL                                                         
         B     OPTX                                                             
*                                                                               
PRVAL    DS    0H                      PM REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    PRV100                                                           
*                                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   PRV90                                                            
         MVI   RCARD2+15,C'Y'                                                   
         B     PRV99                                                            
                                                                                
PRV90    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
PRV99    DS    0H                                                               
         OI    FIND,X'04'                                                       
PRV100   DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,PRVAL                                                         
         B     OPTX                                                             
*                                                                               
K3VAL    DS    0H                      PM REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    K3V10                                                            
         CLC   =C'DELALL',FLD1     DELETE SOLO AND PIGGYBACK GOALS              
         BNE   K3V8                                                             
         MVI   RNUM+66,C'Y'                                                     
         OI    FIND,X'04'                                                       
         B     K3V10                                                            
*                                                                               
K3V8     DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
K3V10    DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,K3VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
PFVAL    DS    0H                      PF REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    PFV50                                                            
         CLC   =C'TEST',FLD1           PRELIMINARY RUN                          
         BNE   PFV40                                                            
         MVI   RO1,C'Y'                                                         
         OI    FIND,X'04'                                                       
         B     PFV50                                                            
*                                                                               
PFV40    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
PFV50    DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,PFVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
FFVAL    DS    0H                      FF REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    FFV100                                                           
         CLC   =C'FIXALL',FLD1         FIX ALL FILMS                            
         BNE   FFV90                                                            
         CLI   DDS,1                   ONLY FOR DDS TERMINALS                   
         BNE   FFV90                                                            
         MVI   RO1,C'Y'                                                         
         OI    FIND,X'04'                                                       
         B     FFV100                                                           
*                                                                               
FFV90    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
FFV100   DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,FFVAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
S74VL    DS    0H                      74 - PROFILE LISTING REPORT              
         BRAS  RE,VALUNIS                                                       
         BE    S74V100                                                          
         CLC   =C'CLT',FLD1            CLIENT FILTER?                           
         BE    S74VOK                                                           
         CLC   =C'OFF',FLD1            OFFICE FILTER?                           
         BNE   S74V90                  NO, INVALID                              
*                                                                               
*                                                                               
         XC    TEMP(60),TEMP          TEMP MUST BE AT LEAST 48 BYTES            
         LA    R1,TEMP            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,6(R3)                                                    
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC2,FLD2+1     EXTERNAL OFFICE CODE                          
*                                                                               
         MVC   OFCLMT,6(R3)                                                     
         MVC   OFCSECD,ASECBLK    A("SECRET BLOCK")                             
*                                                                               
         DROP  R1                                                               
*                                                                               
*        VALIDATE OFFICE CODE VIA OFFICER AND GET OFFICE NAME                   
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',TEMP),(1,ACOMFACS),                  X        
               (C'L',NAME)                                                      
*                                                                               
         CLI   0(R1),0             DID IT PASS SECURITY?                        
         BE    *+14                YES                                          
         MVC   FERN,=AL2(CAERROR) ACCESS ERROR                                  
         B     OFFINV                                                           
*                                                                               
         LA    R1,TEMP            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         LA    RF,FLD2+1           POINT TO OFFICE CODE SAVEAREA                
         MVI   FLD2+2,C' '         SET TO SPACE                                 
*                                                                               
         MVC   0(1,RF),OFCOFC      SAVE INTERNAL OFFICE CODE                    
*                                                                               
         DROP  R1                                                               
*                                                                               
S74VOK   MVC   RPRO1,FLD2                                                       
         OI    FIND,X'04'                                                       
         B     S74V100                                                          
*                                                                               
S74V90   DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
S74V100  DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,S74VL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
S07VL    DS    0H                      SPOT/NET UNBILLING                       
         BRAS  RE,VALUNIS                                                       
         BE    S07V100                                                          
         CLC   =C'DETAILS',FLD1        STA BUCKETS OR U/BILLING RECS            
         BE    *+14                                                             
         CLC   =C'UDETAILS',FLD1       UNITS TOTAL                              
         BNE   S07V40                                                           
*                                                                               
         LLC   R2,FLD2LEN                                                       
         GOTO1 CASHVAL,PLIST,(X'80',FLD2),(R2)                                  
         CLI   PLIST,0                                                          
         BNE   S07V90                                                           
         MVC   DUB,PLIST+4                                                      
         UNPK  TEMP(12),DUB                                                     
         OI    TEMP+11,X'F0'                                                    
         CP    PLIST+4(8),=P'0'      TEST NEGATIVE                              
         BNL   S07V10                                                           
         MVI   TEMP,C'-'                                                        
         CP    DUB,=P'-99999999999'                                             
         BL    S07V90               CAN'T HANDLE MINUS 1 BILLION                
S07V10   OI    FIND,X'04'                                                       
         MVC   RCARD2+32(12),TEMP                                               
         CLC   RNUM,=C'7U'         IF NETWORK, DO MORE...                       
         BNE   S07V100             IF SPOT, DONE                                
         MVI   RCARD2+44,C'U'      'U' FOR U/BILLING RECS                       
         CLC   =C'UDETAILS',FLD1                                                
         BNE   S07V100                                                          
         MVI   RCARD2+44,C'O'      'O' FOR OLD UNIT RECS                        
         B     S07V100                                                          
*                                                                               
S07V40   DS    0H                                                               
         CLC   =C'PURGE',FLD1       PURGE UNBILLED ELEMS ?                      
         BNE   S07V50                                                           
         MVI   RO3,C'Y'                                                         
         OI    FIND,X'04'                                                       
         B     S07V100                                                          
*                                                                               
S07V50   DS    0H                                                               
         CLC   =C'CORRECTION',FLD1                                              
         BNE   S07V90                                                           
         MVI   RO4,C'D'                                                         
         OI    FIND,X'04'                                                       
         B     S07V100                                                          
*                                                                               
S07V90   DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
S07V100  DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,S07VL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
I5VAL    DS    0H                      I5 REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    I5V100                                                           
         CLC   =C'ID',FLD1         ID=5 DIGIT NUMBER                            
         BNE   I5VL20                                                           
         CLC   =C'CK',RAGY         FOR AGENCY CK ONLY                           
         BNE   I5VL90                                                           
         TM    FLD2VAL,X'80'       MUST BE NUMBERIC                             
         BZ    I5VL90                                                           
         CLI   FLD2LEN,5           MUST BE FIVE DIGITS                          
         BNE   I5VL90                                                           
         MVC   RNUM+49(5),FLD2                                                  
         OI    FIND,X'04'                                                       
         B     I5V100                                                           
*                                                                               
I5VL20   DS    0H                                                               
         CLC   =C'REP',FLD1                                                     
         BNE   I5VL90                                                           
         CLC   =C'INV',FLD2                                                     
         BE    I5VL25                                                           
         CLC   =C'EST',FLD2                                                     
         BE    I5VL25                                                           
         CLI   FLD2,C'N'                                                        
         BNE   I5VL90                                                           
I5VL25   MVC   RO4,FLD2                                                         
         OI    FIND,X'04'                                                       
         B     I5V100                                                           
*                                                                               
I5VL90   DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
I5V100   DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,I5VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
I6VAL    DS    0H                      I6 REPORT                                
         BRAS  RE,VALUNIS                                                       
         BE    I6V10                                                            
         CLC   =C'ZERO',FLD1       ZERO=N                                       
         BNE   I6V02                                                            
         CLI   FLD2LEN,1           MUST BE ONE CHARACTER                        
         BNE   OPTINV                                                           
         MVC   RO3,FLD2                                                         
         OI    FIND,X'04'                                                       
         B     I6V10                                                            
                                                                                
I6V02    DS    0H                      I6 REPORT                                
         CLC   =C'DETAIL',FLD1       DETAIL=Y/N                                 
         BNE   I6V03                                                            
         CLI   FLD2LEN,1           MUST BE ONE CHARACTER                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    I6V02A                                                           
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
I6V02A   MVC   RO4,FLD2                                                         
         OI    FIND,X'04'                                                       
         B     I6V10                                                            
                                                                                
I6V03    DS    0H                                                               
         CLC   =C'PERSON',FLD1       PERSON=CCCCCC                              
         BNE   I6V10                                                            
         CLI   FLD2LEN,8             MAX 8 CHARACTERS                           
         BH    OPTINV                                                           
         MVC   RCARD2+20(8),FLD2                                                
         OI    FIND,X'04'                                                       
***      B     I6V10                                                            
*                                                                               
I6V10    DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,I6VAL                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
N5VAL    DS    0H                  N5 REPORT OPTIONS                            
         BRAS  RE,VALUNIS                                                       
         BE    N5V100                                                           
         CLC   =C'POSTOVER',FLD1   POST OVERRIDES (HATE THIS HARDCODE)          
         BNE   N5V10                                                            
         CLI   FLD2LEN,1           MUST BE ONE CHARACTER                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'O'                                                        
         BNE   OPTINV                                                           
         MVC   R5668(1),FLD2       USES RERATE FIELD 56                         
         OI    FIND,X'04'                                                       
         B     N5V100                                                           
*                                                                               
N5V10    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
N5V100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,N5VAL                                                         
         B     OPTX                                                             
*                                                                               
SSVAL    DS    0H                  SS REPORT OPTIONS                            
         BRAS  RE,VALUNIS                                                       
         BE    SSV100                                                           
         CLC   =C'BUY',FLD1                                                     
         BNE   *+14                                                             
         MVC   HALF,=X'0D62'                                                    
         B     SSV20                                                            
*                                                                               
         CLC   =C'SUP',FLD1                                                     
         BNE   SSV30                                                            
         MVC   HALF,=X'0D61'                                                    
SSV20    XC    KEY,KEY                                                          
         MVC   KEY(2),HALF                                                      
         MVC   KEY+2(1),AGYB                                                    
         MVC   KEY+3(6),FLD2                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FF)                                                    
         BNE   SSV90                                                            
         MVC   R2USER(6),KEY+3                                                  
         MVC   R2USER+6(1),FLD1                                                 
         OI    FIND,X'04'                                                       
         B     SSV100                                                           
*                                                                               
SSV30    DS    0H                                                               
         CLC   =C'DOWN',FLD1       DOWN OPTION                                  
         BNE   SSV90                                                            
         MVI   RO3,C'D'                                                         
         MVC   REQOUT,=C'DOWN  '   SET OUTPUT TYPE TO DOWNLOAD                  
         OI    FIND,X'04'                                                       
         B     SSV100                                                           
*                                                                               
SSV90    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
SSV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,SSVAL                                                         
         B     OPTX                                                             
*                                                                               
MYVAL    DS    0H                                                               
         BRAS  RE,VALUNIS                                                       
         BE    MYV100                                                           
         CLC   =C'UNPOST',FLD1    UNPOST (ONLY DDS TERMINAL)                    
         BNE   MYV10              (UNPOST AND MARK MUTUALLY EXCLUSIVE)          
         CLI   RO4,X'40'                                                        
         BNE   OPTINV                                                           
         MVI   RO4,C'U'                                                         
         CLI   DDS,1                                                            
         BNE   OPTINV                                                           
         OI    FIND,X'04'                                                       
         B     MYV100                                                           
*                                                                               
MYV10    DS    0H                                                               
         CLC   =C'MARK',FLD1           MARK                                     
         BNE   MYV20                                                            
         CLI   RO4,X'40'                                                        
         BNE   OPTINV                                                           
         MVI   RO4,C'M'                                                         
         CLI   DDS,1                                                            
         BNE   OPTINV                                                           
         OI    FIND,X'04'                                                       
         B     MYV100                                                           
*                                                                               
*                                                                               
MYV20    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
MYV100   LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R4,MYVAL                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
VONEFLD  NTR1                                                                   
         L     R2,=A(ONEFLD)                                                    
         A     R2,OPTRELO                                                       
VO5      CLC   1(2,R2),RNUM                                                     
         BE    VO10                                                             
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   VO5                                                              
         B     OPTINV              NO MATCH                                     
VO10     LLC   R1,0(R2)                                                         
         SHI   R1,4                                                             
         LR    R4,R2                                                            
VO12     CLC   FLD2(1),4(R4)                                                    
         BE    VO20                                                             
         LA    R4,1(R4)                                                         
         BCT   R1,VO12                                                          
         B     VONO                NO MATCH,INVALID                             
VO20     LLC   R1,3(R2)                                                         
         LA    R1,RNUM(R1)                                                      
         MVC   0(1,R1),4(R4)                                                    
         SR    R1,R1                                                            
         B     *+8                                                              
VONO     LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     OPTX                                                             
*                                                                               
OPTINV   DS    0H                  INVALID ENTRY                                
         MVC   FERN,=AL2(FE)                                                    
OFFINV   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** ERROR FLD #   ***'                               
         LA    R5,TEMP+15                                                       
         EDIT  (R6),(2,0(R5)),ALIGN=LEFT,WRK=TEMP+61                            
         NI    FIND,X'FB'    SET OFF VALID BIT (X'04')                          
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
*PTHELP  DS    0H                    HELP SCREEN                                
**       MVC   FERN,=AL2(FE)                                                    
**       XC    TEMP(60),TEMP                                                    
**       MVC   TEMP(3),=C'***'                                                  
**       L     R4,FLDHADR                                                       
**       LA    R5,HELPTBL          IS OPT FIELD ACTIVE FOR REQUEST              
*LP2     CLC   RNUM,1(R5)                                                       
**       BE    HLP10                                                            
**       LLC   R1,0(R5)                                                         
**       AR    R5,R1                                                            
**       CLI   0(R5),0                                                          
**       BNE   HLP2                                                             
**       XC    8(50,R4),8(R4)                                                   
**       MVC   8(23,R4),=C' *** FOR FUTURE USE ***'                             
**       OI    6(R4),X'80'                                                      
**       B     OPTX                                                             
*                                         CANADIAN                              
*LP10    MVC   8(10,R4),=C'  $C,$U,FR'                                          
**       OI    6(R4),X'88'                                                      
**       CLI   DDS,1                                                            
**       BE    HLP12                                                            
**       BAS   RE,CHKCAN                                                        
**       BE    HLP12                                                            
**       XC    8(10,R4),8(R4)                                                   
**       B     *+8                                                              
*LP12    LA    R4,11(R4)                                                        
**       LA    R5,HELPTBL          SEARCH HELP TABLE                            
*LP15    CLC   RNUM,1(R5)                                                       
**       BE    HLP20                                                            
**       LLC   R1,0(R5)                                                         
**       AR    R5,R1                                                            
**       CLI   0(R5),0                                                          
**       BNE   HLP15                                                            
**       B     OPTX                                                             
*LP20    LLC   R1,0(R5)                                                         
**       S     R1,=F'4'                                                         
**       EX    R1,*+8                                                           
**       B     *+10                                                             
**       MVC   8(0,R4),3(R5)       SET HELP INFO TO OUT LINE                    
**       LA    R5,8(R4)                                                         
**       AR    R5,R1               POINT TO END OF HELP INFO                    
**       LR    R4,R5                                                            
**       BAS   RE,SPECIALS         CHK INFO FOR CERTAIN AGENCIES                
**       BE    OPTX                                                             
**       EX    R1,*+8                                                           
**       B     *+10                                                             
**       MVC   0(0,R4),0(R2)                                                    
OPTX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
GMVAL    DS    0H                                                               
         CLC   =C'ACTION',FLD1     ACTION=YEAR                                  
         BNE   OPTINV                                                           
         CLC   RNUM,=C'GM'         FOR GM REPORT ONLY                           
         BNE   OPTINV                                                           
         TM    FLD2VAL,X'80'       MUST BE NUMERIC                              
         BZ    OPTINV                                                           
         CLI   FLD2LEN,4           MUST BE 4 DIGIT YEAR                         
         BNE   OPTINV                                                           
         OI    FIND,X'04'          IT IS VALID                                  
         MVI   RNUM+30,C'*'        NEED 2ND REQUEST CARD (R3037+1)              
         MVI   RO1,C'Y'            TURN ON OPTION#1                             
***  2ND REQUEST CARD                                                           
         MVC   R2USER(2),FLD2+2    TAKING LAST 2 DIGITS OF THE YEAR             
         B     OPTX                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* - UNIVERSAL OPTIONS ARE VALIDATED FIRST                                       
* - SO IDEA WAS THEY WOULD BE INPUT BEFORE PROGRAM SPECIFIC OPTIONS             
* - LATER THIS WAS OBJECTED TO AS NOT USER FRIENDLY                             
* - SO THIS IS A FUDGE                                                          
VALUNIS  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'FR',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'$C',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'$U',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'58',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'WTP',FLD1                                                     
         BE    VALUX                                                            
         CLC   =C'2D',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'1D',FLD1                                                      
         BE    VALUX                                                            
         CLC   =C'LPMWK',FLD1                                                   
         BE    VALUX                                                            
         CLC   =C'OVN',FLD1                                                     
         BE    VALUX                                                            
         CLC   =C'CBL',FLD1                                                     
VALUX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*PECIALS NTR1              OPTIONS RESTRICTED TO CERTAIN AGENCIES               
*        SR    R1,R1                                                            
*        CLC   RAGY,=C'MC'    MC FOR M4 REPORT                                  
*        BNE   SPCX                                                             
*        CLC   RNUM,=C'M4'                                                      
*        BNE   SPCX                                                             
*        LA    R1,4                                                             
*        LA    R2,SPCM4                                                         
*PCX     LTR   R1,R1                                                            
*        XIT1  REGS=(R1,R2)                                                     
*                                                                               
*PCM4    DC    C'F=A-Z'                                                         
         EJECT                                                                  
*ELPTBL  DS    0F                                                               
*        DC    AL1(31),C'A2',C'GST=Y/I/O,S=YN,G=BF/NNN.NNNN'                    
*        DC    AL1(21),C'AB',C'S=YN,G=BF/NNN.NNNN'                              
*        DC    AL1(6),C'B1',C'R=N'                                              
*        DC    AL1(6),C'D1',C'R=N'                                              
*        DC    AL1(14),C'D2',C'C58=N,DF=NY'                                     
*        DC    AL1(8),C'D3',C'C58=N'                                            
*        DC    AL1(8),C'D4',C'C58=N'                                            
*        DC    AL1(14),C'D5',C'C58=N,DF=NY'                                     
*        DC    AL1(8),C'D6',C'C58=N'                                            
*        DC    AL1(8),C'D7',C'C58=N'                                            
*        DC    AL1(8),C'DX',C'C58=N'                                            
**       DC    AL1(20),C'D8',C'C58=N,PRG=N,DF=NY'                               
**       DC    AL1(8),C'I2',C'DF=NY'                                            
**       DC    AL1(8),C'M1',C'DF=NY'                                            
**       DC    AL1(14),C'M2',C'C58=N,DF=NY'                                     
**       DC    AL1(14),C'M3',C'C58=N,DF=NY'                                     
**       DC    AL1(14),C'M4',C'C58=N,DF=NY'                                     
**       DC    AL1(8),C'M5',C'DF=NY'                                            
**       DC    AL1(14),C'M6',C'C58=N,DF=NY'                                     
**       DC    AL1(14),C'M7',C'C58=N,DF=NY'                                     
**       DC    AL1(8),C'M8',C'C58=N'                                            
**       DC    AL1(8),C'M9',C'C58=N'                                            
**       DC    AL1(8),C'ML',C'DF=NY'                                            
**       DC    AL1(8),C'N5',C'C58=N'                                            
**       DC    AL1(21),C'RZ',C'R=RS/DC/D2/3/4/5/6'                              
**       DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* TABLE OF VALID VALUES                                                         
*                                                                               
* XL1 ENTRY LENGTH                                                              
* CL2 ENTRY REQUEST NUMBER                                                      
* XL1 ENTRY COLUMN LESS ONE                                                     
* CLN VALUE LIST                                                                
*                                                                               
ONEFLD   DS    0C                                                               
         DC    AL1(7),C'A2',AL1(101),C' NY'                                     
         DC    AL1(7),C'AB',AL1(101),C' NY'                                     
         DC    AL1(6),C'B1',AL1(63),C' N'                                       
         DC    AL1(6),C'D1',AL1(63),C' N'                                       
         DC    AL1(7),C'M4',AL1(62),C' NY'                                      
         DC    AL1(9),C'D8',AL1(66),C' 0123'                                    
         DC    X'0000'                                                          
         SPACE                                                                  
*                                                                               
* THIS TABLE CONTAINS ADDRESSES OF REQUEST VALIDATION ROUTINES                  
*                                                                               
REQATBL  DS    0F                                                               
         DC    C'A2',X'0000',A(A2VAL)                                           
         DC    C'AB',X'0000',A(A2VAL)       AB SAME AS A2                       
         DC    C'A7',X'0000',A(A7VAL)                                           
         DC    C'B1',X'0000',A(B1VAL)                                           
         DC    C'BU',X'0000',A(B1VAL)       BU SAME AS B1                       
         DC    C'D1',X'0000',A(B1VAL)       D1 SAME AS B1                       
         DC    C'DU',X'0000',A(B1VAL)       DU SAME AS B1                       
         DC    C'D2',X'0000',A(D2VAL)                                           
         DC    C'D3',X'0000',A(D3VAL)                                           
         DC    C'D4',X'0000',A(D4VAL)                                           
         DC    C'D5',X'0000',A(D5VAL)                                           
         DC    C'D6',X'0000',A(D6VAL)                                           
         DC    C'J6',X'0000',A(J6VAL)                                           
         DC    C'D7',X'0000',A(D7VAL)                                           
         DC    C'DX',X'0000',A(D7VAL)                                           
         DC    C'D8',X'0000',A(D8VAL)                                           
         DC    C'DN',X'0000',A(DNVAL)                                           
         DC    C'DL',X'0000',A(DLVAL)                                           
         DC    C'GM',X'0000',A(GMVAL)                                           
         DC    C'I2',X'0000',A(I2VAL)                                           
         DC    C'I5',X'0000',A(I5VAL)                                           
         DC    C'I6',X'0000',A(I6VAL)                                           
         DC    C'K4',X'0000',A(K4VAL)                                           
         DC    C'K2',X'0000',A(K2VAL)                                           
         DC    C'K1',X'0000',A(K1VAL)                                           
         DC    C'K5',X'0000',A(K5VAL)                                           
         DC    C'KL',X'0000',A(K5VAL)           KL=K5 VAL                       
         DC    C'LI',X'0000',A(I2VAL)           LI=I2 VAL                       
         DC    C'L2',X'0000',A(L2VAL)                                           
         DC    C'M2',X'0000',A(M2VAL)                                           
         DC    C'M3',X'0000',A(M3VAL)                                           
         DC    C'M4',X'0000',A(M4VAL)                                           
         DC    C'M8',X'0000',A(M3VAL)            M8=M3 VAL                      
         DC    C'M9',X'0000',A(M3VAL)            M9=M3 VAL                      
         DC    C'MG',X'0000',A(MGVAL)                                           
         DC    C'ML',X'0000',A(MLVAL)                                           
         DC    C'MY',X'0000',A(MYVAL)                                           
         DC    C'N2',X'0000',A(I2VAL)            I2=N2                          
         DC    C'NV',X'0000',A(NVVAL)                                           
         DC    C'RN',X'0000',A(RNVAL)                                           
         DC    C'RS',X'0000',A(RSVAL)                                           
         DC    C'RY',X'0000',A(RSVAL)        RY=FAXABLE RS REPORT               
         DC    C'RX',X'0000',A(RNVAL)        RX=FAXABLE RN REPORT               
         DC    C'RZ',X'0000',A(RZVAL)                                           
         DC    C'48',X'0000',A(R48VL)                                           
         DC    C'XD',X'0000',A(RXDVL)                                           
         DC    C'C1',X'0000',A(C1VAL)                                           
         DC    C'PM',X'0000',A(PMVAL)                                           
         DC    C'K3',X'0000',A(K3VAL)                                           
         DC    C'07',X'0000',A(S07VL)        SPOT UNBILLING                     
         DC    C'7U',X'0000',A(S07VL)        NET UNBILLING                      
         DC    C'74',X'0000',A(S74VL)        74 PROFILE LISTING                 
         DC    C'PF',X'0000',A(PFVAL)        PF VALIDATION                      
         DC    C'N5',X'0000',A(N5VAL)        NETWORK TIMESHEET (CANADA)         
         DC    C'SS',X'0000',A(SSVAL)        SUPERDESK STATUS                   
         DC    C'PR',X'0000',A(PRVAL)                                           
         DC    C'FF',X'0000',A(FFVAL)                                           
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
         DROP  R8                                                               
*                                                                               
BTVAL    NTR1  BASE=*,LABEL=*          BT REPORT                                
         LA    RE,SPTREC+500                                                    
         L     RF,=F'320'                                                       
         XCEF                                                                   
         LA    R5,SPTREC+500                                                    
         USING SCAND,R5                                                         
         L     R4,FLDHADR                                                       
*                             CALLING SCANNER WITH EXTENDED 2ND FIELD           
         GOTO1 SCANNER,PLIST,(X'0F',(R4)),(10,SPTREC+500)                       
         LLC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    BTVX                                                             
         LR    R4,R1               PASS NUM OF SCANNER FLDS IN R4               
         LHI   R6,1                KEEPS TRACK OF THE FIELD IN USE              
*                                                                               
BTV3     CLC   =C'MOS',FLD1                                                     
         BNE   BTV8                                                             
         CLI   RNUM+49,C' '        IN CASE THEY PUT THE PERIOD TWICE            
         BH    BTINV               HONOR 1ST ONE, GIVE ERROR ON 2ND             
         XC    TEMP,TEMP                                                        
         GOTO1 DATVAL,PLIST,(2,FLD2),TEMP                                       
         OC    PLIST(4),PLIST                                                   
         BE    BTINV                                                            
         MVC   RNUM+49(4),TEMP                                                  
         L     R2,PLIST            PUT LEN OF FIELD IN R2                       
         CLM   R2,1,FLD2LEN        ONLY ONE DATE INPUT ?                        
         BE    BTV5                                                             
         LA    RF,FLD2                                                          
         AR    RF,R2                                                            
         CLI   0(RF),C'-'          DID THEY TRY TO INPUT PERIOD ?               
         BNE   BTINV                                                            
         LA    RF,1(RF)            POINT PAST -                                 
         MVC   TEMP+20(6),0(RF)    GET 2ND DATE                                 
         GOTO1 DATVAL,PLIST,(2,TEMP+20),TEMP                                    
         OC    PLIST(4),PLIST                                                   
         BE    BTINV                                                            
BTV5     MVC   RNUM+53(4),TEMP                                                  
         CLC   RNUM+49(4),RNUM+53  IS START DATE EARLIER THAN END DATE?         
         BH    BTINV                                                            
         OI    FIND,X'04'                                                       
         B     BTV10                                                            
*                                                                               
BTV8     DS    0H                                                               
         B     BTINV                                                            
*                                                                               
BTV10    DS    0H                                                               
         LA    R5,37(R5)           **THE LEN OF 2ND FIELD IS 15 NOT 10!         
         LA    R6,1(R6)                                                         
         BCT   R4,BTV3                                                          
BTVX     XIT1                                                                   
*                                                                               
BTINV    DS    0H                  INVALID ENTRY                                
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** ERROR FLD #   ***'                               
         LA    R5,TEMP+15                                                       
         EDIT  (R6),(2,0(R5)),ALIGN=LEFT,WRK=TEMP+61                            
         B     BTVX                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
COMMOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,SPTREC+500                                                    
         L     RF,=F'320'                                                       
         XCEF                                                                   
         LA    R5,SPTREC+500                                                    
         USING SCAND,R5                                                         
         L     R4,FLDHADR                                                       
         MVI   6(R4),X'80'         SET OFF HILITE AND TRANSMIT                  
         MVI   7(R4),0             SET OFF OUTPUT LENGTH                        
         GOTO1 SCANNER,PLIST,(20,(R4)),(10,SPTREC+500)                          
         LLC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    COMX                                                             
         LR    R4,R1                                                            
         LA    R6,1                R6=SCANNER POSITION FOR ERROR RTN            
*                                                                               
*                                *UNIVERSAL OPTIONS                             
OPT10    DS    0H                                                               
         CLC   =C'FR',FLD1       *LANGUAGE TYPE                                 
         BNE   OPT15                                                            
         CLI   FLD1LEN,2                                                        
         BNE   COMINV                                                           
         OI    FIND,X'04'                                                       
         MVC   RLANG,FLD1                                                       
         BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT15    DS    0H                                                               
         CLC   =C'$C',FLD1        *CURRENCY OPTION                              
         BE    *+14                (DDS OR CANADIAN)                            
         CLC   =C'$U',FLD1                                                      
         BNE   OPT20                                                            
         CLI   FLD1LEN,2                                                        
         BNE   COMINV                                                           
         OI    FIND,X'04'                                                       
         MVC   RCRRNCY,FLD1+1                                                   
         CLI   DDS,1               IS IT DDS TERMINAL                           
         BE    OPT17                                                            
         BRAS  RE,CHKCAN           OR CANDIAN AGENCY                            
         BNE   COMINV                                                           
OPT17    BCTR  R4,0                                                             
         B     OPT200                                                           
         EJECT                                                                  
*                                                                               
OPT20    DS    0H                                                               
         CLC   =C'C58',FLD1        C58=N   (COL22 OF REQ2)                      
         BNE   OPT24                                                            
         CLI   FLD2,C'N'                                                        
         BNE   COMINV                                                           
         CLC   RNUM,=C'M2'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M3'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M4'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M6'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M7'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M8'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'M9'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D2'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D3'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D4'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D5'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D6'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D7'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'DX'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'D8'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'N5'                                                      
         BE    OPT22                                                            
         CLC   RNUM,=C'GM'                                                      
         BNE   COMINV                                                           
OPT22    OI    FIND,X'04'                                                       
         MVI   RNUM+101,C'N'                                                    
         BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT24    DS    0H                                                               
         CLC   =C'WTP',FLD1        WTP=N   (COL23 OF REQ2)                      
         BE    OPT24C                                                           
         CLC   =C'LPMWK',FLD1      OVERRIDE LPM POSTING PROFILE                 
         BE    *+14                                                             
         CLC   =C'OVN',FLD1        OVERRIDE OVERNIGHT POSTING PROFILE           
         BNE   OPT30                                                            
*                                                                               
         CLC   RNUM,=C'JV'                                                      
         BE    OPT26C                                                           
*                                                                               
OPT24C   DS    0H                                                               
         CLC   RNUM,=C'I2'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'N2'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M2'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M3'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M4'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M6'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M7'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M8'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'M9'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'D2'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'D5'                                                      
         BE    OPT26                                                            
         CLC   RNUM,=C'D8'                                                      
         BNE   COMINV                                                           
OPT26    CLC   =C'WTP',FLD1                                                     
         BNE   *+12                                                             
         MVI   RCARD2+22,C'W'                                                   
         B     OPT29                                                            
*                                                                               
OPT26C   CLC   =C'OVN',FLD1                                                     
         BNE   OPT26D                                                           
         CLI   FLD2,C'M'                                                        
         BE    OPT26E                                                           
OPT26D   CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   COMINV                                                           
*                                                                               
OPT26E   CLC   =C'LPMWK',FLD1                                                   
         BNE   *+14                                                             
         MVC   RCARD2+77(1),FLD2                                                
         B     OPT29                                                            
*                                                                               
         CLC   =C'OVN',FLD1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCARD2+78(1),FLD2                                                
OPT29    OI    FIND,X'04'                                                       
         BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT30    DS    0H                                                               
         CLC   =C'CBL',FLD1        CBLDEM=F/N/L/0                               
         BNE   OPT40                                                            
         BRAS  RE,SORN             ONLY FOR SPOT 'D' & 'M' REPORTS              
         BE    COMINV                                                           
         CLI   RNUM,C'D'                                                        
         BE    *+12                                                             
         CLI   RNUM,C'M'                                                        
         BNE   COMINV                                                           
*                                                                               
         CLI   FLD2,C'F'           FUSION                                       
         BE    OPT35                                                            
         CLI   FLD2,C'N'           NIELSEN                                      
         BE    OPT35                                                            
         CLI   FLD2,C'L'           LPM MKTS ONLY                                
         BE    OPT35                                                            
         CLI   FLD2,C'0'           NONE                                         
         BNE   COMINV                                                           
OPT35    MVC   RCARD2+3(1),FLD2                                                 
         OI    FIND,X'04'                                                       
         BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT40    DS    0H                                                               
         CLC   =C'2D',FLD1         2D - 2 DECIMAL RATING FEATURE                
         BE    *+14                FOR SPOT 'D' & 'M' REQUESTS ONLY             
         CLC   =C'1D',FLD1         1D - SUPPRESS 2-DECIMAL RATING               
         BNE   OPT50               FOR SPOT 'D' & 'M' REQUESTS ONLY             
*                                                                               
         BRAS  RE,SORN             ONLY FOR SPOT                                
         BE    COMINV                                                           
         CLI   RNUM,C'D'                                                        
         BE    *+12                                                             
         CLI   RNUM,C'M'                                                        
         BNE   COMINV                                                           
*                                                                               
         MVI   RCARD2+79,C'Y'                                                   
         CLC   =C'2D',FLD1                                                      
         BE    *+8                                                              
         MVI   RCARD2+79,C'N'                                                   
*                                                                               
         OI    FIND,X'04'                                                       
         BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT50    DS    0H                                                               
*                                                                               
OPT200   DS    0H                                                               
         LA    R5,42(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R1,OPT10            ANY MORE UNIVERSAL OPTS TO CHECK             
COMX     XIT1                                                                   
*                                                                               
COMINV   DS    0H                  INVALID ENTRY                                
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** ERROR FLD #   ***'                               
         LA    R5,TEMP+15                                                       
         EDIT  (R6),(2,0(R5)),ALIGN=LEFT,WRK=TEMP+61                            
         B     COMX                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SORN     NTR1  BASE=*,LABEL=*                                                   
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,2           IF SPOT                                      
         BNE   *+8                                                              
         OI    ODDMNTS,SPOTSYS                                                  
         CLI   FAOVSYS,3           IF NET                                       
         BNE   *+8                                                              
         OI    ODDMNTS,NETSYS                                                   
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   *+6                                                              
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CHKCAN   NTR1  BASE=*,LABEL=* READ AGY HEADER TO SEE IF CANADIAN AGENCY         
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),RAGY                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BNH   CHKNO                                                            
         LA    R2,SPTREC                                                        
         USING AGYHDR,R2                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   CHKNO                                                            
         MVC   KEY(13),KEYS                                                     
         B     CHKYES                                                           
CHKNO    LA    R1,1                                                             
         B     *+8                                                              
CHKYES   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* THIS IS A REQUEST FOR A RERATE                                                
* NEED TO SEE IF ANY COMSCORE DEMOS IN REQUESTED ESTIMATES                      
* """"""""""""""""""""""""""""""""""""""                                        
* IN ROUTINE BELOW, *** REMEMBER ****                                           
* KEY IS ONLY 13 BYTES LONG <<================                                  
* KEYS IS 18 BYTES LONG!!!  <<================                                  
*===============================================================                
                                                                                
CHKCOMSC NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),RPRO                                                    
         CLC   KEY+4(3),=C'ALL'                                                 
         JNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
*                                                                               
         CLI   REST,C'0'           TEST ESTIMATE NUMERIC                        
         JL    CHKCOM2                                                          
         PACK  DUB,REST                                                         
         CVB   R0,DUB                                                           
         STC   R0,KEY+7                                                         
         STC   R0,HALF             SET START OF EST SERIES                      
         STC   R0,HALF+1           SET END OF EST SERIES                        
*                                                                               
         CLI   REST1,C'0'          TEST SERIES INPUT                            
         JL    CHKCOM4             NO                                           
         PACK  DUB,REST1                                                        
         CVB   R0,DUB                                                           
         STC   R0,HALF+1           SAVE END OF SERIES EST                       
         J     CHKCOM4                                                          
*                                                                               
CHKCOM2  CLC   REST(3),=C'ALL'                                                  
         JE    CHKCOM2X                                                         
         CLC   REST(2),=C'NO'                                                   
         JE    CHKCOM2X                                                         
         DC    H'0'                                                             
*                                                                               
CHKCOM2X MVI   HALF,0              SET DOING ALL ESTS                           
         MVI   HALF+1,255                                                       
*                                                                               
CHKCOM4  GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEYS                      
         J     CHKCOM12                                                         
*                                                                               
CHKCOM10 GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEY,KEYS                      
*                                                                               
CHKCOM12 CLC   KEY(7),KEYS         SAME A-M/CLT/PRD                             
         JNE   CHKCOMX                                                          
*                                                                               
         CLC   KEYS+7(1),HALF+1    TEST PAST END OF SERIES                      
         JH    CHKCOMX                                                          
*                                                                               
         OC    KEYS+8(5),KEYS+8    TEST BILL                                    
         JNZ   CHKCOM10                                                         
*                                                                               
         GOTO1 (RF),(R1),=C'DMRDIR',=C'SPTFILE',KEYS+14,SPTREC                  
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         LA    RF,SPTREC                                                        
         USING ESTHDR,RF                                                        
                                                                                
*==================================================================             
* IF DOING MULTIPLE ESTIMATES, FILTER ON DATES AND FILTERS IF ANY               
*==================================================================             
                                                                                
         CLI   HALF,0              DOING ALL ESTS?                              
         JNE   CHKCOM30            NO                                           
*                                                                               
         CLC   EEND,RSTRD          EST END BEFORE REQ START                     
         JL    CHKCOM10            YES                                          
         CLC   ESTART,RENDD        EST START AFTER REQ END                      
         JH    CHKCOM10                                                         
*                                                                               
         CLC   REST1,=C'   '       TEST ANY FILTERS                             
         JE    CHKCOM30            NO                                           
*                                                                               
         LA    R0,3                                                             
         LA    R1,REST1                                                         
         LA    RE,EPROF                                                         
*                                                                               
CHKCOM20 CLI   0(R1),C'*'                                                       
         JE    CHKCOM24                                                         
         CLI   0(R1),C' '                                                       
         JE    CHKCOM24                                                         
         TM    0(R1),X'40'         TEST NEGATIVE FILTER                         
         JZ    CHKCOM22            YES                                          
*                                                                               
         CLC   0(1,R1),0(RE)       POSITIVE FILTER MUST MATCH                   
         JNE   CHKCOM10                                                         
         J     CHKCOM24                                                         
*                                                                               
CHKCOM22 MVC   FULL(1),0(R1)                                                    
         OI    FULL,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   FULL(1),0(RE)       IF NEG FILTER MATCHES,                       
         JE    CHKCOM10              SKIP THIS EST                              
*                                                                               
CHKCOM24 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         JCT   R0,CHKCOM20                                                      
*                                                                               
CHKCOM30 LA    R1,EDEMLST                                                       
         LA    R0,20                                                            
*                                                                               
CHKCOM32 OC    0(3,R1),0(R1)       TEST EOL                                     
         JZ    CHKCOM10                                                         
         CLI   2(R1),0             TEST COMSCORE DEMO                           
         JE    CHKCOM34                                                         
*                                                                               
         LA    R1,3(R1)                                                         
         JCT   R0,CHKCOM32                                                      
         J     CHKCOM10                                                         
*                                                                               
CHKCOM34 MVI   COMSCORE,C'Y'       SET COMSCORE REQUEST                         
         DROP  RF                                                               
CHKCOMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
SCAND    DSECT                                                                  
*        DESCT TO COVER SCANNER LINES                                           
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
         EJECT                                                                  
         PRINT GEN                                                              
       ++INCLUDE SPREQSAVE                                                      
       ++INCLUDE SPREQTEMP                                                      
       ++INCLUDE SPREQFFBD                                                      
       ++INCLUDE FLDIND                                                         
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE NEDDEQUS                                                       
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPGENEST                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'253SPREQ04   02/26/20'                                      
         END                                                                    
