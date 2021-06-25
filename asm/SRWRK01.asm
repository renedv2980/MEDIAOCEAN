*          DATA SET SRWRK01    AT LEVEL 016 AS OF 10/30/13                      
*PHASE T15101A                                                                  
*INCLUDE XSORT                                                                  
         TITLE '$WK - DISPLAY/STATUS FILE FUNCTIONS'                            
         PRINT NOGEN                                                            
WKDISP   CSECT                                                                  
         NMOD1 000,**$WK1**,RA                                                  
         LR    RC,R1                                                            
         USING WRKWKD,RC                                                        
         L     R2,APARM                                                         
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM6                                                       
         USING SRWRKFFD,R3         R3=A(TWA)                                    
         LR    R8,R3                                                            
         USING WRKSVD,R8           R8=A(TWA SAVE DATA)                          
         L     R9,SRPARM4                                                       
         USING COMFACSD,R9         R9=A(COM FAC LIST)                           
*                                                                               
         MVC   LACTN,ACTN          SET MODE TO FILE DISPLAY                     
         MVI   MODE,0                                                           
*                                                                               
         MVI   LOCKT,0             SET NO LOCK                                  
         MVI   NDXRT,0             SET NO READ UPDATE                           
         CLI   ACTN,ACTDIS                                                      
         BE    SRCH                                                             
         CLI   ACTN,ACTSIZ                                                      
         BE    SRCH                                                             
         MVI   LOCKT,X'80'         SET LOCK                                     
         MVI   NDXRT,X'80'         SET READ FOR UPDATE                          
         CLC   WKFILE,=CL8'FACWRK'                                              
         BNE   *+12                                                             
         BAS   RE,FWLOCK                                                        
         B     SRCH                                                             
         BAS   RE,WKLOCK                                                        
         EJECT                                                                  
SRCH     DS    0H                  SEARCH INDEX FOR MATCHING ENTRIES            
         ZAP   QF,=P'0'                                                         
         ZAP   QA,=P'0'                                                         
         ZAP   QH,=P'0'                                                         
         ZAP   QD,=P'0'                                                         
         ZAP   QK,=P'0'                                                         
         ZAP   QT,=P'0'                                                         
         ZAP   QX,=P'0'                                                         
         ZAP   QY,=P'0'                                                         
         MVC   SEQL,FFS                                                         
         XC    SEQH,SEQH                                                        
         XC    CITOTS,CITOTS                                                    
*                                                                               
         L     R6,AFILTAB          R6=(SORT TABLE ENTRY)                        
         MVC   0(10,R6),FFS                                                     
         BAS   RE,CXLOOPI                                                       
         USING WKRECD,R5           R5=A(WKFILE INDEX ENTRY)                     
*                                                                               
SRCH0    BAS   RE,GETXAD                                                        
         GOTO1 CDATAMGR,DMCB,(NDXRT,DMREAD),WKFILE,CXADDR,ACXREC                
         CLI   8(R1),0                                                          
         BNE   SRCHERR                                                          
*                                                                               
SRCH2    CLI   WKSTAT,WKSTPU       TEST IF PURGED                               
         BE    SRCHX                                                            
SRCH2A   TM    WKSTAT,WKSTTE       TEST IF TEMPORARY                            
         BZ    SRCH2B                                                           
         LH    R1,CIX1             BUMP PART1 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX1                                                          
         B     SRCHX                                                            
SRCH2B   ZIC   R0,WKAGES           GET SIZE OF FILE                             
         SH    R0,=H'1'                                                         
         BP    *+6                                                              
         SR    R0,R0                                                            
         TM    WKSTAT,WKSTKE                                                    
         BZ    SRCH2C                                                           
         LH    R1,CIK1             BUMP PART1 KEEP                              
         LA    R1,1(R1)                                                         
         STH   R1,CIK1                                                          
         LH    R1,CIK2             BUMP PART2 KEEP                              
         AR    R1,R0                                                            
         STH   R1,CIK2                                                          
         B     SRCH2D                                                           
SRCH2C   TM    WKSTAT,WKSTAC+WKSTHO                                             
         BZ    SRCH2D                                                           
         LH    R1,CIX1             BUMP PART1 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX1                                                          
         LH    R1,CIX2             BUMP PART2 NOTAVAIL                          
         AR    R1,R0                                                            
         STH   R1,CIX2                                                          
SRCH2D   EQU   *                                                                
*                                                                               
SRCH3    OC    IFUSRID,IFUSRID     FILTER ON USER ID                            
         BZ    SRCH3C                                                           
         TM    DDS,X'20'                                                        
         BO    SRCH3A                                                           
         CLC   WKUSRID,IFUSRID     USERID WAS SPECIFIED                         
         BE    SRCH3C                                                           
         B     SRCHX                                                            
*                                                                               
SRCH3A   CLC   WKUSRID,IFUSRID     +USERID WAS SPECIFIED                        
         BNH   SRCHX                                                            
SRCH3C   CLC   WKFILNO,SEQL        SAVE LOWEST FILE NUM                         
         BH    *+10                                                             
         MVC   SEQL,WKFILNO                                                     
         CLC   WKFILNO,SEQH        SAVE HIGHEST FILE NUM                        
         BL    *+10                                                             
         MVC   SEQH,WKFILNO                                                     
         AP    QF,=P'1'            BUMP FILES                                   
         TM    WKSTAT,WKSTAC                                                    
         BZ    *+10                                                             
         AP    QA,=P'1'            BUMP ACTIVE                                  
         TM    WKSTAT,WKSTHO                                                    
         BZ    *+10                                                             
         AP    QH,=P'1'            BUMP HOLD                                    
         TM    WKSTAT,WKSTUS                                                    
         BZ    *+10                                                             
         AP    QD,=P'1'            BUMP DEAD                                    
         TM    WKSTAT,WKSTKE                                                    
         BZ    *+10                                                             
         AP    QK,=P'1'            BUMP KEEP                                    
*                                                                               
SRCH4    CLI   IFSYSPRG,0          FILTER ON FILE ID                            
         BE    SRCH4A                                                           
         CLC   IFSYSPRG(1),WKSYSPRG                                             
         BNE   SRCHX                                                            
SRCH4A   OC    IFSYSPRG+1(2),IFSYSPRG+1                                         
         BZ    SRCH4B                                                           
         CLC   IFSYSPRG+1(2),WKSYSPRG+1                                         
         BNE   SRCHX                                                            
SRCH4B   CLI   IFSUBPRG,0                                                       
         BE    SRCH4C                                                           
         CLC   IFSUBPRG,WKSUBPRG                                                
         BNE   SRCHX                                                            
SRCH4C   CLI   IFDAY,0                                                          
         BE    SRCH4D                                                           
         CLC   IFDAY,WKDAY                                                      
         BNE   SRCHX                                                            
SRCH4D   CLI   IFCLASS,0                                                        
         BE    SRCH4E                                                           
         CLC   IFCLASS,WKCLASS                                                  
         BNE   SRCHX                                                            
SRCH4E   CLI   IFEXTRA,0                                                        
         BE    SRCH4F                                                           
         CLC   IFEXTRA,WKEXTRA                                                  
         BNE   SRCHX                                                            
SRCH4F   CLC   IFFILNO,WKFILNO                                                  
         BH    SRCHX                                                            
         CLC   IFFILNOX,WKFILNO                                                 
         BL    SRCHX                                                            
*                                                                               
SRCH5    CLI   IFCLAF,0            FILTER ON CLASS                              
         BE    SRCH5X                                                           
         LA    RE,IFCLAV                                                        
SRCH5A   CLI   0(RE),0             TEST FOR END OF INPUT CLASS LIST             
         BE    SRCH5C                                                           
         CLI   0(RE),C'*'                                                       
         BNE   *+16                                                             
         CLI   WKCLASS,0                                                        
         BE    SRCH5B                                                           
         B     *+14                                                             
         CLC   0(1,RE),WKCLASS                                                  
         BE    SRCH5B                                                           
         LA    RE,1(RE)                                                         
         B     SRCH5A                                                           
*                                                                               
SRCH5B   CLI   IFCLAF,X'80'        CLASS VALUE IN LIST                          
         BE    SRCH5X                                                           
         B     SRCHX                                                            
*                                                                               
SRCH5C   CLI   IFCLAF,X'70'        CLASS VALUE NOT IN LIST                      
         BE    SRCH5X                                                           
         B     SRCHX                                                            
SRCH5X   EQU   *                                                                
*                                                                               
SRCH6    ZIC   R1,IFDATF           FILTER ON DATE                               
         LTR   R1,R1                                                            
         BZ    SRCH6X                                                           
         CLC   WKAGED,IFDATV                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH6X                                                         
SRCH6X   EQU   *                                                                
*                                                                               
SRCH7    ZIC   R1,IFNCIF           FILTER ON NUMBER OF CI'S                     
         LTR   R1,R1                                                            
         BZ    SRCH7X                                                           
         CLC   WKAGES,IFNCIV                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH7X                                                         
SRCH7X   EQU   *                                                                
*                                                                               
SRCH8    CLI   IFSTAF,0            FILTER ON STATUS                             
         BE    SRCH8X                                                           
         MVC   DUB(1),IFSTAV                                                    
         TM    DUB,WKSTKE          TEST KEEP STATUS FIRST                       
         BZ    SRCH8A                                                           
         TM    WKSTAT,WKSTKE                                                    
         BO    *+16                                                             
         CLI   IFSTAF,X'80'                                                     
         BNE   SRCH8A                                                           
         B     SRCHX                                                            
         CLI   IFSTAF,X'80'                                                     
         BE    SRCH8A                                                           
         B     SRCHX                                                            
SRCH8A   NI    DUB,255-WKSTKE                                                   
         BZ    SRCH8X                                                           
         IC    R1,DUB                                                           
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    WKSTAT,0                                                         
         BZ    *+16                                                             
         CLI   IFSTAF,X'80'                                                     
         BE    SRCH8X                                                           
         B     SRCHX                                                            
         CLI   IFSTAF,X'80'                                                     
         BNE   SRCH8X                                                           
         B     SRCHX                                                            
SRCH8X   EQU   *                                                                
*                                                                               
SRCHT    CP    QT,QTMAX            TEST FOR ROOM IN FILE TABLE                  
         BL    SRCHU                                                            
         CP    QX,=P'0'                                                         
         BNE   SRCHT1                                                           
         BAS   RE,GETCAD                                                        
         LH    R0,CIADDR                                                        
         CVD   R0,DUB                                                           
         ZAP   QY,DUB              QY=TTTT OF FIRST CI                          
SRCHT1   AP    QX,=P'1'            QX=COUNT OF NO ROOM ENTRYS                   
         B     SRCHX                                                            
*                                                                               
SRCHU    TM    DDS,X'40'           BUILD SAVE TABLE ENTRY                       
         BO    SRCHV                                                            
         BAS   RE,GETCAD                                                        
         MVC   0(2,R6),WKUSRID     USRID/SYSPRG/FILNO/STATUS/ADDR               
         XC    2(3,R6),2(R6)                                                    
         MVC   FLAG,IFSORV         SORT DEFAULT IS D (DESCENDING NUM)           
         CLI   FLAG,0                                                           
         BNE   *+8                                                              
         MVI   FLAG,C'D'                                                        
         CLI   FLAG,C'N'           NUMERIC ASCENDING                            
         BE    SRCHU1                                                           
         CLI   FLAG,C'A'           ALPHA ON KEY                                 
         BNE   *+14                                                             
         MVC   2(3,R6),WKSYSPRG                                                 
         B     SRCHU1                                                           
         LH    R1,=H'9999'         NUMERIC DESCENDING                           
         SH    R1,WKFILNO                                                       
         STH   R1,2(R6)                                                         
SRCHU1   MVC   5(2,R6),WKFILNO                                                  
         MVC   7(1,R6),WKSTAT                                                   
         MVC   8(2,R6),CIADDR                                                   
         LA    R6,10(R6)                                                        
         AP    QT,=P'1'                                                         
         B     SRCHX                                                            
*                                                                               
SRCHV    L     R6,AFILTAB          BUILD COUNT SAVE TABLE ENTRY                 
         CLC   0(2,R6),FFS                                                      
         BE    SRCHV2                                                           
         CLC   WKUSRID,0(R6)       USRID/COUNT/FILNO/STATUS/ADDR                
         BE    SRCHV1                                                           
         LA    R6,10(R6)                                                        
         B     SRCHV+4                                                          
SRCHV1   LH    RE,2(R6)            BUMP NUM OF FILES IF USRID IN TABLE          
         LA    RE,1(RE)                                                         
         STH   RE,2(R6)                                                         
         CLC   WKFILNO,5(R6)                                                    
         BL    SRCHX                                                            
         B     SRCHV3                                                           
SRCHV2   CP    QT,QTMAX                                                         
         BNL   SRCHX                                                            
         AP    QT,=P'1'            BUILD NEW ENTRY FOR USRID                    
         MVC   10(10,R6),FFS                                                    
         MVC   0(2,R6),WKUSRID                                                  
         MVC   2(2,R6),=H'1'                                                    
         MVI   4(R6),0                                                          
SRCHV3   MVC   5(2,R6),WKFILNO     SAVE LAST FILE DATA FOR USRID                
         MVC   7(1,R6),WKSTAT                                                   
         BAS   RE,GETCAD                                                        
         MVC   8(2,R6),CIADDR                                                   
*                                                                               
SRCHX    BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     SRCH2                                                            
         B     SRCH0               END OF PAGE                                  
         TM    DDS,X'40'                                                        
         BO    SORT+6                                                           
         B     SORT                                                             
*                                                                               
SRCHERR  CLI   LOCKT,0             UNLOCK WORK FILE IF LOCKED AND DIE           
         BE    SRCHERRX                                                         
         CLC   WKFILE,=CL8'FACWRK'                                              
         BNE   *+12                                                             
         BAS   RE,FWUNLK                                                        
         B     SRCHERRX                                                         
         BAS   RE,WKUNLK                                                        
SRCHERRX DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Apply sort                                                                    
***********************************************************************         
SORT     MVC   0(10,R6),FFS        SET END OF TABLE AND SORT ON KEY             
         ZAP   DUB,QT                                                           
         CVB   R6,DUB                                                           
         LA    R6,1(R6)                                                         
         CH    R6,=H'2'                                                         
         BNH   SORT1                                                            
         L     R0,AFILTAB                                                       
         GOTO1 =V(XSORT),DMCB,(R0),(R6),10,7,0,RR=RB                            
*                                                                               
SORT1    CLI   ACTN,ACTSIZ         SET HEADLINES FOR SIZE ACTION                
         BNE   SORT2                                                            
         CLC   SRVEH1(16),HLINE                                                 
         BNE   *+16                                                             
         XC    SRVEH1,SRVEH1                                                    
         XC    SRVEH2,SRVEH2                                                    
         B     SIZE                                                             
*                                                                               
SORT2    LA    R4,SRVEH1H          SET HEADLINES FOR OTHER ACTIONS              
         USING WKSLD,R4                                                         
         ZAP   QO,=P'0'            QO=NUM LINES DISPLAYED                       
         ZAP   QS,=P'0'            QS=NUM OF STATUS CHANGES                     
         XC    LUSRID,LUSRID                                                    
*                                                                               
SORT2A   CLC   SRVEH1(16),HLINE                                                 
         BE    SORT2B                                                           
         MVC   SRVEH1,HLINE                                                     
         MVC   SRVEH2,ULINE                                                     
         OI    SRVEH1H+6,X'80'                                                  
         OI    SRVEH2H+6,X'80'                                                  
*                                                                               
SORT2B   CLI   IFFMTV,1                                                         
         BH    SORT2B1                                                          
         CLC   WKSLCOMN,=CL16'Comment'                                          
         BE    SORT2C                                                           
         MVC   WKSLCOMN,=CL16'Comment'                                          
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLCOMN,=CL16'----------------'                                 
         OI    WKSLHDR+6,X'80'                                                  
         B     SORT2C                                                           
SORT2B1  CLC   WKSLCOMN,=CL16'Recl Nci Pct'                                     
         BE    SORT2C                                                           
         MVC   WKSLCOMN,=CL16'Recl Nci Pct'                                     
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLCOMN,=CL16'---- --- ---'                                     
         OI    WKSLHDR+6,X'80'                                                  
*                                                                               
SORT2C   LA    R4,SRVEH1H                                                       
         CLI   ACTN,ACTDIS                                                      
         BNE   SORT2D                                                           
         TM    DDS,X'40'                                                        
         BZ    SORT2C1                                                          
         CLC   WKSLX,=CL8'Totl'                                                 
         BE    SORT3                                                            
         MVC   WKSLX,=CL8'Totl'                                                 
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLX,=CL8'----'                                                 
         OI    WKSLHDR+6,X'80'                                                  
         B     SORT3                                                            
SORT2C1  CLC   WKSLX,=CL8' '                                                    
         BE    SORT3                                                            
         MVC   WKSLX,=CL8' '                                                    
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLX,=CL8' '                                                    
         OI    WKSLHDR+6,X'80'                                                  
         B     SORT3                                                            
*                                                                               
SORT2D   LA    R4,SRVEH1H                                                       
         CLI   ACTN,ACTCHA                                                      
         BNE   SORT2D1                                                          
         CLC   WKSLX,=CL8'Old Fid'                                              
         BE    SORT3                                                            
         MVC   WKSLX,=CL8'Old Fid'                                              
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLX,=CL8'-------'                                              
         OI    WKSLHDR+6,X'80'                                                  
         B     SORT3                                                            
SORT2D1  CLC   WKSLX,=CL8'Oldsta'                                               
         BE    SORT3                                                            
         MVC   WKSLX,=CL8'Oldsta'                                               
         OI    WKSLHDR+6,X'80'                                                  
         LA    R4,SRVEH2H                                                       
         MVC   WKSLX,=CL8'------'                                               
         OI    WKSLHDR+6,X'80'                                                  
         B     SORT3                                                            
*                                                                               
SORT3    CLC   IFFILNO,IFFILNOX    TEST FOR SINGLE/GROUP FILES                  
         BNE   GROUP                                                            
         B     INDIV                                                            
         EJECT                                                                  
***********************************************************************         
* INDIVIDULE FILE FUNCTIONS                                                     
***********************************************************************         
INDIV    LA    R4,SRVP2H           INDIVIDUAL FILE FUNCTIONS                    
         L     R6,AFILTAB                                                       
         CLC   IFFILNO,5(R6)       R6=A(FIRST FILE TABLE ENTRY)                 
         BNE   ERR1                                                             
         MVC   CISTAT,7(R6)        SAVE FILE STATUS AND ADDRESS                 
         MVC   CIADDR(2),8(R6)                                                  
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
         L     R5,ACXREC           READ FIRST CI REC INTO CXREC                 
         USING WKRECD,R5                                                        
         GOTO1 CDATAMGR,DMCB,(X'00',DMREAD),WKFILE,CIADDR,(R5)                  
         MVC   CIERR,DMCB+8                                                     
         CLI   CIERR,0             TEST FOR DISK ERROR                          
         BE    INDIV1                                                           
         XC    WKINDEX(48),WKINDEX SET FOR MINIMUM DISPLAY                      
         MVC   WKUSRID,0(R6)                                                    
         MVC   WKFILNO,5(R6)                                                    
         MVC   WKSTAT,7(R6)                                                     
         B     INDDSP                                                           
*                                                                               
INDIV1   MVC   SAVE(64),WKINDEX                                                 
         GOTO1 CDATAMGR,DMCB,(X'00',=C'BUF'),,WKFNDX,AWKFREC,(R5)               
         MVC   WKINDEX(64),SAVE                                                 
         L     RE,DWKSAVE          POINT TO BUFFER SAVE AREA                    
         AR    RE,R5                                                            
         USING SKBUFFD,RE                                                       
         MVC   SKADDR,CIADDR       SET FILE ADDRESS                             
         MVC   SKFSTCI,CIADDR                                                   
         MVC   SKKEY,WKKEY         SET FILE KEY                                 
*                                                                               
INDACT   CLI   ACTN,ACTACT         ACTIVATE FILE                                
         BNE   INDACTX                                                          
INDACT1  TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    INDACT2                                                          
         SP    QH,=P'1'                                                         
         NI    CISTAT,255-WKSTHO                                                
         B     INDACTA                                                          
INDACT2  TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    INDACT3                                                          
         SP    QD,=P'1'                                                         
         NI    CISTAT,255-WKSTUS                                                
         B     INDACTA                                                          
INDACT3  B     ERR2                                                             
INDACTA  OI    CISTAT,WKSTAC                                                    
         AP    QA,=P'1'                                                         
         LA    R0,=C'RES'                                                       
         B     INDUPD                                                           
INDACTX  EQU   *                                                                
*                                                                               
INDCHA   CLI   ACTN,ACTCHA         KEY CHANGE FILE                              
         BNE   INDCHAX                                                          
         CLI   IFKEYF,0            MUST INPUT NEW KEY                           
         BNE   *+12                                                             
         LA    R4,SRVP3H                                                        
         B     ERR3                                                             
         XC    WKFNDX,WKFNDX                                                    
         MVC   WKFNDX(L'IFKEYV),IFKEYV                                          
         LA    R0,=C'KEY'                                                       
         B     INDUPD                                                           
INDCHAX  EQU   *                                                                
*                                                                               
INDDEL   CLI   ACTN,ACTDEL         DELETE FILE                                  
         BNE   INDDELX                                                          
INDDEL1  TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    INDDEL2                                                          
         SP    QA,=P'1'                                                         
         NI    CISTAT,255-WKSTAC                                                
         B     INDDELA                                                          
INDDEL2  TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    INDDEL3                                                          
         SP    QH,=P'1'                                                         
         NI    CISTAT,255-WKSTHO                                                
         B     INDDELA                                                          
INDDEL3  B     ERR4                                                             
INDDELA  OI    CISTAT,WKSTUS                                                    
         AP    QD,=P'1'                                                         
         LA    R0,=C'DEL'                                                       
         B     INDUPD                                                           
INDDELX  EQU   *                                                                
*                                                                               
INDHOL   CLI   ACTN,ACTHOL         HOLD FILE                                    
         BNE   INDHOLX                                                          
INDHOL1  TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    INDHOL2                                                          
         SP    QA,=P'1'                                                         
         NI    CISTAT,255-WKSTAC                                                
         B     INDHOLA                                                          
INDHOL2  TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    INDHOL3                                                          
         SP    QD,=P'1'                                                         
         NI    CISTAT,255-WKSTUS                                                
         B     INDHOLA                                                          
INDHOL3  B     ERR5                                                             
INDHOLA  OI    CISTAT,WKSTHO                                                    
         AP    QH,=P'1'                                                         
         LA    R0,=C'HOL'                                                       
         B     INDUPD                                                           
INDHOLX  EQU   *                                                                
*                                                                               
INDKEE   CLI   ACTN,ACTKEE         KEEP FILE                                    
         BNE   INDKEEX                                                          
         TM    CISTAT,WKSTKE       CANT BE KEEP                                 
         BO    ERR6                                                             
         OI    CISTAT,WKSTKE                                                    
         AP    QK,=P'1'                                                         
         LA    R0,=C'KEE'                                                       
         B     INDUPD                                                           
INDKEEX  EQU   *                                                                
*                                                                               
INDPUR   CLI   ACTN,ACTPUR         PURGE FILE                                   
         BNE   INDPURX                                                          
         TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    *+10                                                             
         SP    QA,=P'1'                                                         
         TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    *+10                                                             
         SP    QH,=P'1'                                                         
         TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    *+10                                                             
         SP    QD,=P'1'                                                         
         TM    CISTAT,WKSTKE       CAN BE KEEP                                  
         BZ    *+10                                                             
         SP    QK,=P'1'                                                         
         MVI   CISTAT,WKSTPU                                                    
         SP    QF,=P'1'                                                         
         LA    R0,=C'PUR'                                                       
         B     INDUPD                                                           
INDPURX  EQU   *                                                                
*                                                                               
INDUNK   CLI   ACTN,ACTUNK         UNKEEP FILE                                  
         BNE   INDUNKX                                                          
         TM    CISTAT,WKSTKE       MUST BE KEEP                                 
         BZ    ERR7                                                             
         NI    CISTAT,255-WKSTKE                                                
         SP    QK,=P'1'                                                         
         LA    R0,=C'UNK'                                                       
         B     INDUPD                                                           
INDUNKX  EQU   *                                                                
*                                                                               
INDDIS   CLI   ACTN,ACTDIS         DISPLAY FILE                                 
         BE    INDDSP                                                           
         DC    H'0'                                                             
*                                                                               
INDUPD   GOTO1 CDATAMGR,DMCB,(X'40',(R0)),WKFILE,WKFNDX,AWKFREC,(R5)            
         MVC   CIERR,DMCB+8                                                     
         CLI   CIERR,0                                                          
         BNE   INDDSP                                                           
         AP    QS,=P'1'                                                         
*                                                                               
INDDSP   BAS   RE,FILDSP           DISPLAY FILE DATA                            
         BAS   RE,SCREND           TRUNCATE SCREEN                              
         BAS   RE,CNTDSP           DISPLAY FILE COUNTERS                        
         TM    DDS,X'01'                                                        
         BZ    INDDSP1             DISPLAY DISK ADDR IF DDS TERMINAL            
         LA    R4,SRVEC+L'SRVEC-1                                               
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
         LA    RE,SRVEC+L'SRVEC-10                                              
         CR    R4,RE                                                            
         BH    INDDSP1             IGNORE IF NO ROOM                            
         MVC   0(6,R4),=C',ADDR='                                               
         GOTO1 CHEXOUT,DMCB,CIADDR,6(R4),2,=C'MIX'                              
*                                                                               
INDDSP1  MVC   MSG(14),=C'FILE DISPLAYED'                                       
         CLI   CIERR,0                                                          
         BE    *+14                                                             
         MVC   MSG+15(20),=C'WITH DISK ERROR TYPE'                              
         B     INDDSP2                                                          
         CLI   ACTN,ACTDIS                                                      
         BE    INDDSP2                                                          
         MVC   MSG+15(15),=C'WITH NEW STATUS'                                   
*                                                                               
INDDSP2  MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
GROUP    LA    R4,SRVP2H           GROUP FILE FUNCTIONS                         
         CP    QT,=P'0'                                                         
         BE    ERR8                NO FILES FOUND                               
         LA    R4,SRVP3H                                                        
         CLI   ACTN,ACTDIS                                                      
         BE    *+12                                                             
         CLI   IFSTAF,0            UPDATIVE ACTION MUST HAVE STATUS             
         BE    ERR9                                                             
         L     R6,AFILTAB          R6=A(NEXT FILE TABLE ENTRY)                  
*                                                                               
GROUP1   MVC   CISTAT,7(R6)        SAVE FILE STATUS AND ADDRESS                 
         MVC   CIADDR(2),8(R6)                                                  
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
         L     R5,ACXREC           READ FIRST CI REC INTO CXREC                 
         USING WKRECD,R5                                                        
         GOTO1 CDATAMGR,DMCB,(X'00',DMREAD),WKFILE,CIADDR,(R5)                  
         MVC   CIERR,DMCB+8                                                     
         CLI   CIERR,0             TEST FOR DISK ERROR                          
         BE    GROUP2                                                           
         XC    WKINDEX(48),WKINDEX SET FOR MINIMUM DISPLAY                      
         MVC   WKUSRID,0(R6)                                                    
         MVC   WKFILNO,5(R6)                                                    
         MVC   WKSTAT,7(R6)                                                     
         B     GRPDSP                                                           
*                                                                               
GROUP2   MVC   SAVE(64),WKINDEX                                                 
         GOTO1 CDATAMGR,DMCB,(X'00',=C'BUF'),,WKFNDX,AWKFREC,(R5)               
         MVC   WKINDEX(64),SAVE                                                 
         L     RE,DWKSAVE          POINT TO BUFFER SAVE AREA                    
         AR    RE,R5                                                            
         USING SKBUFFD,RE                                                       
         MVC   SKADDR,CIADDR       SET FILE ADDRESS                             
         MVC   SKFSTCI,CIADDR                                                   
         MVC   SKKEY,WKKEY         SET FILE KEY                                 
*                                                                               
GRPACT   CLI   ACTN,ACTACT         ACTIVATE FILE                                
         BNE   GRPACTX                                                          
GRPACT1  TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    GRPACT2                                                          
         SP    QH,=P'1'                                                         
         NI    CISTAT,255-WKSTHO                                                
         B     GRPACTA                                                          
GRPACT2  TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    GRPNOP                                                           
         SP    QD,=P'1'                                                         
         NI    CISTAT,255-WKSTUS                                                
         B     GRPACTA                                                          
GRPACTA  OI    CISTAT,WKSTAC                                                    
         AP    QA,=P'1'                                                         
         LA    R0,=C'RES'                                                       
         B     GRPUPD                                                           
GRPACTX  EQU   *                                                                
*                                                                               
GRPDEL   CLI   ACTN,ACTDEL         DELETE FILE                                  
         BNE   GRPDELX                                                          
GRPDEL1  TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    GRPDEL2                                                          
         SP    QA,=P'1'                                                         
         NI    CISTAT,255-WKSTAC                                                
         B     GRPDELA                                                          
GRPDEL2  TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    GRPNOP                                                           
         SP    QH,=P'1'                                                         
         NI    CISTAT,255-WKSTHO                                                
         B     GRPDELA                                                          
GRPDELA  OI    CISTAT,WKSTUS                                                    
         AP    QD,=P'1'                                                         
         LA    R0,=C'DEL'                                                       
         B     GRPUPD                                                           
GRPDELX  EQU   *                                                                
*                                                                               
GRPHOL   CLI   ACTN,ACTHOL         HOLD FILE                                    
         BNE   GRPHOLX                                                          
GRPHOL1  TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    GRPHOL2                                                          
         SP    QA,=P'1'                                                         
         NI    CISTAT,255-WKSTAC                                                
         B     GRPHOLA                                                          
GRPHOL2  TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    GRPNOP                                                           
         SP    QD,=P'1'                                                         
         NI    CISTAT,255-WKSTUS                                                
         B     GRPHOLA                                                          
GRPHOLA  OI    CISTAT,WKSTHO                                                    
         AP    QH,=P'1'                                                         
         LA    R0,=C'HOL'                                                       
         B     GRPUPD                                                           
GRPHOLX  EQU   *                                                                
*                                                                               
GRPKEE   CLI   ACTN,ACTKEE         KEEP FILE                                    
         BNE   GRPKEEX                                                          
         TM    CISTAT,WKSTKE       MUST BE UNKEEP                               
         BO    GRPNOP                                                           
GRPKEEA  OI    CISTAT,WKSTKE                                                    
         AP    QK,=P'1'                                                         
         LA    R0,=C'KEE'                                                       
         B     GRPUPD                                                           
GRPKEEX  EQU   *                                                                
*                                                                               
GRPPUR   CLI   ACTN,ACTPUR         PURGE FILE                                   
         BNE   GRPPURX                                                          
         TM    CISTAT,WKSTAC       CAN BE ACTIVE                                
         BZ    *+10                                                             
         SP    QA,=P'1'                                                         
         TM    CISTAT,WKSTHO       CAN BE HOLD                                  
         BZ    *+10                                                             
         SP    QH,=P'1'                                                         
         TM    CISTAT,WKSTUS       CAN BE DELETED                               
         BZ    *+10                                                             
         SP    QD,=P'1'                                                         
         TM    CISTAT,WKSTKE       CAN BE KEEP                                  
         BZ    *+10                                                             
         SP    QK,=P'1'                                                         
GRPPURA  MVI   CISTAT,WKSTPU                                                    
         SP    QF,=P'1'                                                         
         LA    R0,=C'PUR'                                                       
         B     GRPUPD                                                           
GRPPURX  EQU   *                                                                
*                                                                               
GRPUNK   CLI   ACTN,ACTUNK         UNKEEP FILE                                  
         BNE   GRPUNKX                                                          
         TM    CISTAT,WKSTKE       MUST BE KEEP                                 
         BZ    GRPNOP                                                           
GRPUNKA  NI    CISTAT,255-WKSTKE                                                
         SP    QK,=P'1'                                                         
         LA    R0,=C'UNK'                                                       
         B     GRPUPD                                                           
GRPUNKX  EQU   *                                                                
*                                                                               
GRPDIS   CLI   ACTN,ACTDIS         DISPLAY FILE                                 
         BE    GRPDSP                                                           
         DC    H'0'                                                             
*                                                                               
GRPUPD   GOTO1 CDATAMGR,DMCB,(X'40',(R0)),WKFILE,WKFNDX,AWKFREC,(R5)            
         MVC   CIERR,DMCB+8                                                     
         CLI   CIERR,0                                                          
         BNE   GRPDSP                                                           
         AP    QS,=P'1'                                                         
*                                                                               
GRPDSP   BAS   RE,FILDSP           DISPLAY FILE DATA                            
*                                                                               
GRPNOP   LA    R6,10(R6)           BUMP TO NEXT FILE                            
         CLC   0(2,R6),FFS                                                      
         BNE   GROUP1                                                           
         BAS   RE,SCREND           TRUNCATE SCREEN                              
         BAS   RE,CNTDSP           DISPLAY FILE COUNTERS                        
*                                                                               
GRPEND   MVC   MSG(34),=C'First NN of NNNN files displayed  '                   
         TM    DDS,X'40'                                                        
         BZ    *+10                                                             
         MVC   MSG+17(5),=C'Users'                                              
         EDIT  (P3,QO),(2,MSG+6)                                                
         OI    MSG+7,X'F0'                                                      
         EDIT  (P3,QT),(4,MSG+12)                                               
         CP    QX,=P'0'                                                         
         BE    GRPEND1                                                          
         MVC   MSG+17(17),=C'+NNNN files dspld'                                 
         EDIT  (P3,QX),(4,MSG+18)                                               
*                                                                               
GRPEND1  CLI   ACTN,ACTDIS                                                      
         BE    GRPEND2                                                          
         MVC   MSG+34(22),=C' -   No status changes'                            
         CP    QS,=P'1'                                                         
         BL    GRPEND2                                                          
         BH    *+8                                                              
         MVI   MSG+55,C' '                                                      
         EDIT  (P3,QS),(4,MSG+37)                                               
*                                                                               
GRPEND2  GOTO1 CSQUASH,DMCB,MSG,60                                              
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
SIZE     SR    RF,RF                                                            
         ICM   RF,3,CICITOT        DISPLAY FILE SIZE INFO                       
         SH    RF,CICINDX                                                       
         SH    RF,CIK1                                                          
         SH    RF,CIX1                                                          
         STCM  RF,3,CIA1           SET PART1 AVAIL CIS                          
         LH    RF,CJCITOT                                                       
         SH    RF,CIK2                                                          
         SH    RF,CIX2                                                          
         STH   RF,CIA2             SET PART2 AVAIL CIS                          
         BAS   RE,CNTDSP           DISPLAY FILE TYPE COUNTERS                   
*                                                                               
SIZE1    MVC   SRVEH1,SLINE        PART1 CI COUNTS                              
         OI    SRVEH1H+6,X'80'                                                  
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         LA    RF,SRVEH1+15                                                     
         BAS   RE,SIZEOUT                                                       
         SR    R0,R0                                                            
         ICM   R0,3,CIA1                                                        
         LA    RF,SRVEH1+28                                                     
         BAS   RE,SIZEOUT                                                       
         SR    R0,R0                                                            
         ICM   R0,3,CIX1                                                        
         LA    RF,SRVEH1+41                                                     
         BAS   RE,SIZEOUT                                                       
         SR    R0,R0                                                            
         ICM   R0,3,CIK1                                                        
         LA    RF,SRVEH1+53                                                     
         BAS   RE,SIZEOUT                                                       
         LH    R0,CICINDX                                                       
         LA    RF,SRVEH1+66                                                     
         BAS   RE,SIZEOUT                                                       
*                                                                               
SIZE2    MVC   SRVEH2,SLINE        PART2 CI COUNTS                              
         OI    SRVEH2H+6,X'80'                                                  
         MVI   SRVEH2+4,C'2'                                                    
         MVC   SRVEH2+59(13),SPACES                                             
         LH    R0,CJCITOT                                                       
         LA    RF,SRVEH2+15                                                     
         BAS   RE,SIZEOUT                                                       
         LH    R0,CIA2                                                          
         LA    RF,SRVEH2+28                                                     
         BAS   RE,SIZEOUT                                                       
         LH    R0,CIX2                                                          
         LA    RF,SRVEH2+41                                                     
         BAS   RE,SIZEOUT                                                       
         LH    R0,CIK2                                                          
         LA    RF,SRVEH2+53                                                     
         BAS   RE,SIZEOUT                                                       
*                                                                               
SIZE3    ZAP   QO,=P'0'            CLEAR AND TRANSMIT REST OF SCREEN            
*        BAS   RE,SCREND                                                        
*                                                                               
SIZE4    LA    R4,SRVED2H          DISPLAY FILE ATTRIBUTES                      
         USING WKSLD,R4                                                         
         MVC   WKSLDATA(13),=C'Trks/part1 CI'                                   
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CITRKS                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(13),=C'Trks/part2 CI'                                   
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CJTRKS                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Trks for index'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Trks for part1'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CICITOT                                                       
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Trks for part2'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CJCITOT                                                       
         MH    R0,CJTRKS                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(13),=C'Record length'                                   
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CIBLKLN                                                       
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(13),=C'Records/Track'                                   
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CIHIREC                                                       
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Ndx entrys/rec'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CIENTRYS                                                      
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Ndx total recs'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CIPAGES                                                       
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Ndx part1 recs'                                  
         OI    WKSLHDR+6,X'80'                                                  
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,86(R4)                                                        
         MVC   WKSLDATA(14),=C'Ndx part2 recs'                                  
         OI    WKSLHDR+6,X'80'                                                  
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         LA    RF,WKSLDATA+15                                                   
         BAS   RE,SIZEOUT                                                       
*                                                                               
SIZE5    MVC   MSG(30),=C'File size attributes displayed'                       
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
SIZEOUT  CVD   R0,DUB              SUBROUTINE TO EDIT NUMBERS                   
         UNPK  0(6,RF),DUB+5(3)                                                 
         OI    5(RF),X'F0'                                                      
         BR    RE                                                               
*IZEOUT  EDIT  R0,(6,0(RF)),FILL=0                                              
*        BR    RE                                                               
         EJECT                                                                  
ERR1     MVC   MSG(14),=C'File not found'                                       
         B     ERRX                                                             
ERR2     MVC   MSG(19),=C'File already active'                                  
         B     ERRX                                                             
ERR3     MVC   MSG(26),=C'Missing filter keyword=KEY'                           
         B     ERRX                                                             
ERR4     MVC   MSG(20),=C'File already deleted'                                 
         B     ERRX                                                             
ERR5     MVC   MSG(17),=C'File already hold'                                    
         B     ERRX                                                             
ERR6     MVC   MSG(17),=C'File already keep'                                    
         B     ERRX                                                             
ERR7     MVC   MSG(19),=C'File already unkeep'                                  
         B     ERRX                                                             
ERR8     MVC   MSG(14),=C'No files found'                                       
         B     ERRX                                                             
ERR9     MVC   MSG(29),=C'Missing filter keyword=STATUS'                        
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(8),=C'ED/9999 '                                           
         MVC   SRVMSG+2(1),SYSCH                                                
         MVC   SRVMSG+8(48),MSG                                                 
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR TO ERROR FIELD                   
         LA    R4,SRVECAH                                                       
         LH    RE,2(R4)            TRUNCATE SCREEN WITH TAB FIELD               
         LA    RE,9(RE)                                                         
         MVC   0(12,R4),=X'090000000000800100000100'                            
         STH   RE,2(R4)                                                         
         SPACE 2                                                                
EXIT     CLI   LOCKT,0             UNLOCK WORK FILE IF LOCKED                   
         BE    EXITX                                                            
         CLC   WKFILE,=CL8'FACWRK'                                              
         BNE   *+12                                                             
         BAS   RE,FWUNLK                                                        
         B     EXITX                                                            
         BAS   RE,WKUNLK                                                        
EXITX    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST                                                                  
***********************************************************************         
FILDSP   NTR1                                                                   
         LA    R4,SRVED1H          R4=A(SCR DISP LINE)                          
         USING WKSLD,R4                                                         
         L     R5,ACXREC           R5=A(FILE RECORD)                            
         USING WKRECD,R5                                                        
         CP    QO,DSPMAX           EXIT IF SCR FULL                             
         BNL   FILDX                                                            
         ZAP   DUB,QO              CALC DISP OF NEXT LINE                       
         CVB   RE,DUB                                                           
         LA    RF,8+L'WKSLDATA                                                  
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         AP    QO,=P'1'            BUMP NUMBER OF LINES USED                    
         OI    WKSLHDR+6,X'80'                                                  
         XC    WKSLDATA,WKSLDATA                                                
*                                                                               
FILD0    XC    FIDEFN,FIDEFN       USER ID NAME                                 
         CLC   WKUSRID,LUSRID                                                   
         BE    FILD0A              SAME AS LAST TIME                            
         MVC   FIUSRID,WKUSRID                                                  
         L     RF,AFIDXPND         GET NEW USER ID NAME                         
         BASR  RE,RF                                                            
         XC    FIUSRID,FIUSRID                                                  
         MVC   WKSLUID(L'WKSLUID+1),FILIDA                                      
         B     *+10                                                             
FILD0A   MVC   WKSLUID(L'WKSLUID+1),LUSRIDA                                     
*                                                                               
FILD1    MVC   WKSLTYPE,DOTS       FILE TYPE                                    
         TM    WKFTYPE,X'02'                                                    
         BZ    *+8                                                              
         MVI   WKSLTYPE,C'*'                                                    
*                                                                               
FILD2    MVC   FISYSPRG(7),WKSYSPRG FILE ID NAME                                
         CLI   ACTN,ACTCHA                                                      
         BNE   *+10                                                             
         MVC   FISYSPRG(7),IFKEYV+2                                             
         OC    FISYSPRG(7),FISYSPRG                                             
         BZ    FILD2A                                                           
         L     RF,AFIDXPND                                                      
         BASR  RE,RF                                                            
         MVC   WKSLFID,FILIDA                                                   
         B     *+10                                                             
FILD2A   MVC   WKSLFID,DOTS                                                     
*                                                                               
FILD3    LH    R0,WKFILNO          FILE SEQUENCE NUMBER                         
         CVD   R0,DUB                                                           
         UNPK  WKSLSEQN,DUB                                                     
         OI    WKSLSEQN+3,X'F0'                                                 
*                                                                               
FILD4    BAS   RE,STATOUT          FILE STATUS                                  
         MVC   WKSLSTAT,STATA                                                   
*                                                                               
FILD5    CLI   WKAGED+1,X'01'      CREATED DATE AND TIME                        
         BL    FILD5A                                                           
         CLI   WKAGED+1,X'12'                                                   
         BH    FILD5A                                                           
         CLI   WKAGED+2,X'01'                                                   
         BL    FILD5A                                                           
         CLI   WKAGED+2,X'31'                                                   
         BH    FILD5A                                                           
         GOTO1 CDATCON,DMCB,(1,WKAGED),(8,WKSLDATE)                             
         B     *+10                                                             
FILD5A   MVC   WKSLDATE(7),DOTS                                                 
         UNPK  DUB(5),WKAGET(3)                                                 
         MVC   WKSLDATE+9(4),DUB                                                
         OC    WKRETN,WKRETN                                                    
         BZ    FILD5B                                                           
         MVC   DUB(2),WKRETN                                                    
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  WKSLRETN,DUB                                                     
         OI    WKSLRETN+3,X'F0'                                                 
         B     *+10                                                             
FILD5B   MVC   WKSLRETN,DOTS                                                    
*                                                                               
FILD6    OC    WKRECS,WKRECS       NUMBER OF RECORDS                            
         BZ    FILD6A                                                           
         EDIT  (B4,WKRECS),(5,WKSLRECS)                                         
         B     *+10                                                             
FILD6A   MVC   WKSLRECS,DOTS                                                    
*                                                                               
         CLI   IFFMTV,1            COMMENT FOR FORMAT 1                         
         BH    FILD7                                                            
         TM    WKFTYPE,X'80'                                     ******         
         BO    *+14                                              ******         
         MVC   WKSLCOMN,DOTS                                     ******         
         B     *+10                                              ******         
         MVC   WKSLCOMN,WKCOMNT                                                 
         B     FILDA                                                            
*                                                                               
FILD7    OC    WKRECL,WKRECL       AVERAGE RECORD LEN                           
         BZ    FILD7A                                                           
         EDIT  (B2,WKRECL),(4,WKSLRECL)                                         
         B     *+10                                                             
FILD7A   MVC   WKSLRECL,DOTS                                                    
*                                                                               
FILD8    ZIC   R0,WKAGES           NUMBER OF CONTROL INTERVALS                  
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         TM    WKFTYPE,X'02'       LIBRARY FILES HAVE ONE CI                    
         BZ    *+8                                                              
         LA    R0,1                                                             
         EDIT  (R0),(3,WKSLNCI)                                                 
FILD8A   EQU   *                                                                
*                                                                               
FILD9    ZIC   RF,WKAGES           PERCENTAGE UTILISATION                       
         LTR   RF,RF                                                            
         BNZ   *+16                                                             
         TM    WKFTYPE,X'02'       LIBRARY FILES HAVE ONE CI                    
         BZ    *+8                                                              
         LA    RF,1                                                             
         SH    RF,=H'1'                                                         
         BM    FILD9A                                                           
         MH    RF,CJTRKS                                                        
         AH    RF,CITRKS                                                        
         MH    RF,CIHIREC                                                       
         TM    WKFTYPE,X'02'       LIBRARY FILES HAVE INDEX REC                 
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         LH    RE,CIBLKLN                                                       
         SH    RE,=H'34'                                                        
         MR    RE,RE               RE&RF=MAX CAPACITY                           
         LH    R1,WKRECL                                                        
         L     R0,WKRECS                                                        
         MH    R0,=H'100'                                                       
         MR    R0,R0               R0&R1=ACT CAPACITY * 100                     
         LTR   R1,R1                                                            
         BZ    FILD9A                                                           
         DR    R0,RF                                                            
         SLDL  R0,32                                                            
         CH    R0,=H'100'          ADJUST FOR APPROX                            
         BNH   *+8                                                              
         LH    R0,=H'100'                                                       
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LH    R0,=H'1'                                                         
         EDIT  (R0),(3,WKSLPCT)                                                 
FILD9A   EQU   *                                                                
*                                                                               
FILDA    CLI   ACTN,ACTDIS         TOTAL NUMBER OF FILES OPTION                 
         BNE   FILDB                                                            
         TM    DDS,X'40'                                                        
         BZ    FILDB                                                            
         EDIT  (B2,2(R6)),(4,WKSLX)                                             
         B     FILDX                                                            
*                                                                               
FILDB    CLI   CIERR,0             DISPLAY DISK ERROR                           
         BE    FILDC                                                            
         MVC   WKSLX(4),=C'*  *'                                                
         GOTO1 CHEXOUT,DMCB,CIERR,WKSLX+1,1,=C'MIX'                             
         GOTO1 (RF),(R1),8(R6),WKSLX+4,2                                        
         B     FILDX                                                            
*                                                                               
FILDC    CLI   ACTN,ACTDIS         DISPLAY OLD STATUS                           
         BE    FILDX                                                            
         CLI   ACTN,ACTCHA                                                      
         BE    FILDD                                                            
         MVC   CISTAT,WKSTAT                                                    
         BAS   RE,STATOUT                                                       
         MVC   WKSLX,STATA                                                      
         B     FILDX                                                            
*                                                                               
FILDD    XC    FIDEFN,FIDEFN       DISPLAY OLD FILE ID                          
         MVC   FISYSPRG(7),WKSYSPRG                                             
         L     RF,AFIDXPND                                                      
         BASR  RE,RF                                                            
         MVC   WKSLX,FILIDA                                                     
         B     FILDX                                                            
*                                                                               
FILDX    XIT1                                                                   
         EJECT                                                                  
SCREND   NTR1                      TRUNCATE SCREEN WITH TAB FIELD               
         CP    QO,DSPMAX                                                        
         BNL   SCRENDX             EXIT IF SCREEN FULL                          
         LA    R4,SRVED1H                                                       
         USING WKSLD,R4                                                         
         ZAP   DUB,QO                                                           
         CVB   RE,DUB                                                           
         LA    RF,8+L'WKSLDATA                                                  
         MR    RE,RE                                                            
         AR    R4,RF               R4=A(FIRST EMPTY SCREEN LINE)                
         LH    RE,2(R4)                                                         
         LA    RE,9(RE)                                                         
         MVC   0(12,R4),=X'090000000000800100000100'                            
         STH   RE,2(R4)                                                         
SCRENDX  XIT1                                                                   
         EJECT                                                                  
CNTDSP   NTR1                      DISPLAY FILE COUNTERS                        
         LA    R5,SAVE                                                          
         MVI   0(R5),C' '                                                       
         MVC   1(99,R5),0(R5)                                                   
*                                                                               
CNTD2    CP    QF,=P'0'            TOTAL                                        
         BE    CNTD4                                                            
         MVC   0(4,R5),=C'Totl'                                                 
         MVI   10(R5),C','                                                      
         EDIT  (P3,QF),(5,5(R5))                                                
         LA    R5,11(R5)                                                        
*                                                                               
CNTD4    CP    QA,=P'0'            ACTIVE                                       
         BE    CNTD6                                                            
         MVC   0(4,R5),=C'Actv'                                                 
         MVI   10(R5),C','                                                      
         EDIT  (P3,QA),(5,5(R5))                                                
         LA    R5,11(R5)                                                        
*                                                                               
CNTD6    CP    QH,=P'0'            HOLD                                         
         BE    CNTD8                                                            
         MVC   0(4,R5),=C'Hold'                                                 
         MVI   10(R5),C','                                                      
         EDIT  (P3,QH),(5,5(R5))                                                
         LA    R5,11(R5)                                                        
*                                                                               
CNTD8    CP    QK,=P'0'            KEEP                                         
         BE    CNTDA                                                            
         MVC   0(4,R5),=C'Keep'                                                 
         MVI   10(R5),C','                                                      
         EDIT  (P3,QK),(5,5(R5))                                                
         LA    R5,11(R5)                                                        
*                                                                               
CNTDA    CP    QD,=P'0'            DELETED                                      
         BE    CNTDC                                                            
         MVC   0(4,R5),=C'Deld'                                                 
         MVI   10(R5),C','                                                      
         EDIT  (P3,QD),(5,5(R5))                                                
         LA    R5,11(R5)                                                        
*                                                                               
CNTDC    MVC   0(4,R5),=C'Seqn'    HIGH SEQ NUM                                 
         EDIT  (B2,SEQH),(5,4(R5))                                              
*                                                                               
         LA    R5,SAVE             SQUASH AND PUT IN EQUAL SIGNS                
         LA    R6,99                                                            
         GOTO1 CSQUASH,DMCB,(R5),(C'=',(R6))                                    
*                                                                               
         MVC   SRVEC,0(R5)         MOVE TO TWA FIELD                            
         OI    SRVECH+6,X'80'                                                   
*                                                                               
CNTDSPX  XIT1                                                                   
         EJECT                                                                  
STATOUT  NTR1                      OUTPUT FILE STATUS                           
         MVI   STATA,C' '                                                       
         MVC   STATA+1(L'STATA-1),STATA                                         
         LA    RE,STATA            RE=A(NEXT OUTPUT BYTE)                       
         LA    RF,3                RF=L'OUTPUT-1                                
*                                                                               
         CLI   CISTAT,WKSTPU       PURGED STATUS                                
         BNE   *+14                                                             
         MVC   0(6,RE),=C'PURGED'                                               
         B     STATOUTX                                                         
*                                                                               
         TM    CISTAT,WKSTAC       MAIN STATUS = ACTV/HOLD/DELD                 
         BZ    *+10                                                             
         MVC   DUB(4),=C'ACTV'                                                  
         TM    CISTAT,WKSTHO                                                    
         BZ    *+10                                                             
         MVC   DUB(4),=C'HOLD'                                                  
         TM    CISTAT,WKSTUS                                                    
         BZ    *+10                                                             
         MVC   DUB(4),=C'DELD'                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DUB                                                      
         LA    RE,1(RE,RF)                                                      
*                                                                               
         SR    RF,RF               SUB-STATUS = KEEP                            
         TM    CISTAT,WKSTKE                                                    
         BZ    STATOUTX                                                         
         MVI   0(RE),C','                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),=C'KEEP'                                                 
         LA    RE,2(RE,RF)                                                      
*                                                                               
STATOUTX XIT1                                                                   
         EJECT                                                                  
*DMWRKRR                                                                        
       ++INCLUDE DMWRKRR                                                        
         EJECT                                                                  
DSPMAX   DC    PL2'14'                                                          
QTMAX    DC    PL2'401'                                                         
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
FFS      DC    16X'FF'                                                          
DOTS     DC    16C'.'                                                           
SPACES   DC    16C' '                                                           
*                                                                               
HLINE    DC    CL78'Userid  File-Id  Seq# Status Created  Time Retn #reX        
               cs Comment         '                                             
ULINE    DC    CL78'------  -------  ---- ------ ------------- ---- ---X        
               -- ----------------'                                             
*                                                                               
SLINE    DC    CL78'Part1 CI Total=123456,Avail=123456,Inuse=123456,KeeX        
               p=123456,Index=123456     '                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WKSLD    DSECT                     SCREEN DISPLAY LINE                          
WKSLHDR  DS    CL8                                                              
WKSLDATA DS    0CL78                                                            
WKSLUID  DS    CL6                                                              
WKSLTYPE DS    CL1                                                              
         DS    CL1                                                              
WKSLFID  DS    CL8                                                              
         DS    CL1                                                              
WKSLSEQN DS    CL4                                                              
         DS    CL1                                                              
WKSLSTAT DS    CL6                                                              
         DS    CL1                                                              
WKSLDATE DS    CL13                                                             
         DS    CL1                                                              
WKSLRETN DS    CL4                                                              
         DS    CL1                                                              
WKSLRECS DS    CL5                                                              
         DS    CL1                                                              
WKSLCOMN DS    0CL16                                                            
WKSLRECL DS    CL4                                                              
         DS    CL1                                                              
WKSLNCI  DS    CL3                                                              
         DS    CL1                                                              
WKSLPCT  DS    CL3                                                              
         DS    CL4                                                              
WKSLX    DS    CL8                                                              
         EJECT                                                                  
*SRWRKWK                                                                        
       ++INCLUDE SRWRKWK                                                        
         EJECT                                                                  
SRWRKFFD DSECT                                                                  
         DS    CL64                                                             
*SRWRKFFD                                                                       
       ++INCLUDE SRWRKFFD                                                       
*                                                                               
         ORG   SRVFCAH                                                          
*SRWRKFED                                                                       
       ++INCLUDE SRWRKFED                                                       
*                                                                               
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DMWRKRS                                                                        
       ++INCLUDE DMWRKRS                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FASRPARM                                                                       
       ++INCLUDE FASRPARM                                                       
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SRWRK01   10/30/13'                                      
         END                                                                    
