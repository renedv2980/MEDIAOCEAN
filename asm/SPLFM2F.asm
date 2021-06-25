*          DATA SET SPLFM2F    AT LEVEL 076 AS OF 05/01/02                      
*PHASE T2192FA,+0                                                               
         TITLE 'SPLFM2F - SPILL DEFINITION RECORD'                              
T2192F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2192F                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING SDEFRECD,R8                                                      
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         TWAXC SPLAM1H,PROT=Y      CLEAR SCREEN                                 
         LA    R6,MKTNUM                                                        
         LA    R5,SPLAM1H                                                       
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         USING SDEFEL05,R2                                                      
         CLI   0(R2),X'05'                                                      
         BE    FMT4                                                             
         CLI   0(R2),0                                                          
         BE    FMT10                                                            
FMT2     BAS   RE,NEXTEL                                                        
         BNE   FMT10                                                            
FMT4     LA    R4,8(R5)                                                         
         TM    SDEFCEX,X'80'       SEE IF MKT NOT FOR ALL CLTS                  
         BNO   FMT6                                                             
         MVI   0(R4),C'*'          PRECEED MKT WITH *                           
         LA    R4,1(R4)                                                         
FMT6     EDIT  SDEFAMKT,(4,0(R4)),0,ALIGN=LEFT                                  
         AR    R4,R0                                                            
*        OC    SDEFKCLT,SDEFKCLT                                                
*        BNZ   FMT7                                                             
         OC    SDEFBKTY,SDEFBKTY                                                
         BZ    FMT7                                                             
         MVI   0(R4),C'='                                                       
         MVC   1(1,R4),SDEFBKTY                                                 
FMT7     FOUT  (R5)                                                             
*                                  READ MKT REC AND DISPLAY NAME                
*                                                                               
         MVC   KEY(17),ZEROS                                                    
         MVC   KEY(2),=C'MT'                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB        SINCE EDIT USES DUB                          
         MVC   KEY+6(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 STA                                                              
         USING MKTRECD,R7                                                       
         LA    R5,LEN1(R5)         BUMP TO NAME FLDHDR                          
         FOUT  (R5),MKTNAME,20                                                  
         LA    R5,LEN2(R5)         BUMP TO OFFSET FLDHDR                        
         EDIT  SDEFOSET,(4,8(R5)),0,FLOAT=-,ZERO=BLANK                          
         LA    R5,LEN3(R5)         BUMP TO RSM FLDHDR                           
         OC    SDEFBKTY,SDEFBKTY                                                
         BNZ   FMT8                                                             
         EDIT  SDEFRMKT,(4,8(R5)),0,ALIGN=LEFT                                  
         MVC   WORK(2),SDEFRMKT                                                 
         BAS   RE,FNDMKT                                                        
*                                                                               
FMT8     FOUT  (R5)                                                             
         LA    R5,LEN4(R5)         NEXT MKT # FLDHDR                            
         BCT   R6,FMT2                                                          
         BAS   RE,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                TOO MANY ELEMS                               
*                                                                               
FMT10    LTR   R6,R6                                                            
         BNP   FMTX                                                             
FMT11    FOUT  (R5),SPACES,4       CLEAR REST OF SCREEN                         
         LA    R5,LEN1(R5)                                                      
         FOUT  (R5),SPACES,20                                                   
         LA    R5,LEN2(R5)                                                      
         FOUT  (R5),SPACES,4                                                    
         LA    R5,LEN3(R5)                                                      
         FOUT  (R5),SPACES,5                                                    
         LA    R5,LEN4(R5)                                                      
         BCT   R6,FMT11                                                         
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
* *********************************************************************         
*EDT -   VALIDATE RECORD INPUT FIELDS                                           
* *********************************************************************         
         SPACE 1                                                                
EDT      DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BE    EDT1                                                             
         CLI   SVACT,C'R'                                                       
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVC   KEY,SVKEY                                                        
         GOTO1 READ                NEEDED FOR DMWRT IN WRITE                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'R'                                                       
         BNE   EDT2                                                             
         MVI   KEY+13,0           CLEAR DELETE FLAG                             
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVI   REC+15,0                                                         
         GOTO1 PUTREC                                                           
         B     FMT                                                              
*                                                                               
EDT1     MVC   SDEFLEN,=H'24'      INITIALIZE REC LENGHT                        
         B     EDT10                                                            
*                                                                               
EDT2     MVI   ELCODE,X'05'                                                     
EDT2A    LA    R2,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         CLI   0(R2),X'05'                                                      
         BE    EDT4                                                             
         CLI   0(R2),0                                                          
         BE    EDT10                                                            
         BAS   RE,NEXTEL                                                        
         BNE   EDT10                                                            
EDT4     GOTO1 VRECUP,DMCB,(0,REC),0(R2)                                        
         B     EDT2A                                                            
*                                                                               
*                                                                               
EDT10    LA    R6,MKTNUM           SET R6 FOR BCT                               
         LA    R2,SPLAM1H                                                       
         LA    R7,ELEM                                                          
         MVI   ACTSW,0                                                          
         USING SDEFEL05,R7                                                      
*                                                                               
EDT12    XC    ELEM(20),ELEM                                                    
         MVC   ELEM(2),=X'050A'                                                 
         CLI   5(R2),0             NO INPUT IN AGY MKT FIELD                    
         BNE   EDT12B                                                           
EDT12A   LA    R2,LEN1(R2)         CLEAR OLD MKT NAME                           
         FOUT  (R2),SPACES,20                                                   
         LA    R2,LEN2+LEN3+LEN4(R2)       BUMP TO NEXT MKT                     
         B     NEXTMKT                                                          
*                                                                               
EDT12B   LA    R4,8(R2)                                                         
         ZIC   R5,5(R2)            FLD LENGHT                                   
*                                                                               
         CLC   8(3,R2),=C'DEL'     CHK FOR SPECIAL ELEM DELETE                  
         BE    EDT12A              YES JUST SKIP TO NEXT MKT                    
*                                                                               
         OI    ACTSW,X'01'         SET ON ACTIVITY SWITCH                       
EDT12C   CLI   0(R4),C'*'          IF PRECEED BY * CLT EXCP MKT                 
         BNE   EDT12F                                                           
         OI    SDEFCEX,X'80'                                                    
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
EDT12F   LTR   R5,R5                                                            
         BZ    EDTERR                                                           
         LR    RE,R5                                                            
         LR    RF,R4                                                            
         SR    R1,R1               COUNTS DIGITS IN NUMBER FOR EXPACK           
         XC    SDEFBKTY,SDEFBKTY   MOVE IN BOOK TYPE                            
*                                                                               
EDT12G   CLI   0(R4),C'='          END OF NUMBER                                
         BNE   EDT12GA                                                          
         CR    R5,RE               IS 1ST CHAR THE '=' SIGN                     
         BE    EDTERR              YES, MKT #MISSING                            
         B     EDT12GB                                                          
*                                                                               
EDT12GA  CLI   0(R4),C'0'          CHECK FOR NUMERIC MKT #                      
         BL    EDTERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    EDTERR                                                           
         LA    R4,1(R4)            NEXT POSN IN FLD                             
         LA    R1,1(R1)            BUMP DIGIT COUNTER                           
         BCT   R5,EDT12G                                                        
*                                                                               
EDT12GB  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R0,DUB                                                           
         STH   R0,SDEFAMKT                                                      
         LR    R5,RE               R5 SAVES LENGTH OF MKT FLD                   
*                                                                               
         CLC   SVMKT,SDEFAMKT      COMPARE STAT MKT SAVED IN SVMKT              
         BE    EDTERR              CAN'T SPILL TO ITSELF                        
         BAS   RE,READMKT          VALIDATE MKT #                               
*                                                                               
         LA    R1,1(R1)            REAL LENGTH OF MKT #                         
         CR    R1,R5               IS THERE A BKTYPE DEFINED?                   
         BNL   EDIT2GC                                                          
*        OC    SDEFKCLT,SDEFKCLT                                                
*        BNZ   EDTERR              BOOK TYPE ONLY VALID FOR CLT=0               
         LA    R1,2(R1)                                                         
         CR    R1,R5                                                            
         BNE   EDTERR              INVALID BOOK TYPE                            
         CLI   1(R4),C'A'          ALPHA BOOK TYPE                              
         BL    EDTERR                                                           
         CLI   1(R4),C'Z'                                                       
         BH    EDTERR                                                           
         MVC   SDEFBKTY,1(R4)      MOVE IN BOOK TYPE                            
         XC    SDEFRMKT,SDEFRMKT                                                
*                                                                               
EDIT2GC  LA    R2,LEN1+LEN2(R2)                                                 
         XC    SDEFOSET,SDEFOSET                                                
         CLI   5(R2),0             NOT INPUT                                    
         BE    EDT12H                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R5)                                         
         MVI   ERRCD,INVERR                                                     
         CLI   DMCB,0                                                           
         BNE   LFMERR                                                           
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'21000'          3.5 HOURS MAX                              
         BH    LFMERR                                                           
         C     R5,=F'-21000'                                                    
         BL    LFMERR                                                           
         CVD   R5,DUB                                                           
         DP    DUB,=P'1500'                                                     
         CP    DUB+5(3),=P'0'      MUST BE 15 MIN MULTIPLES                     
         BNE   LFMERR                                                           
         M     R4,=F'1'                                                         
         D     R4,=F'100'                                                       
         STH   R5,SDEFOSET                                                      
EDT12H   BAS   RE,ADDELEM                                                       
         LA    R2,LEN3+LEN4(R2)                                                 
*                                                                               
NEXTMKT  BCT   R6,EDT12                                                         
         B     WRITE                                                            
*                                                                               
EDTERR   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
WRITE    DS    0H                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADDNDEF                                                          
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC              REREAD REC                                   
         ST    R8,AREC                                                          
         CLI   ACTSW,X'01'         CHK FOR ACTIVITY                             
         BE    WRITE5                                                           
         MVI   KEY+13,X'80'       NO - DELETE RECORD WITH NO ELEMS              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVI   REC+15,X'80'                                                     
         GOTO1 PUTREC                                                           
         XC    LFMKEXP,LFMKEXP                                                  
         FOUT  LFMKEXPH,=C'** RECORD DELETED **',21                             
         B     EXXMOD                                                           
*                                                                               
WRITE5   GOTO1 PUTREC                                                           
         B     REQREC                                                           
*                                                                               
ADDNDEF  CLI   ACTSW,X'01'         MUST HAVE ACTIVITY ON ADD                    
         BE    ADDN5                                                            
         LA    R2,SPLAM1H                                                       
         MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
ADDN5    GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     REQREC                                                           
         EJECT                                                                  
REQREC   DS    0H                                                               
         XC    REC(110),REC                                                     
         MVI   REC+10,44                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'44'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+31(3),=C'ALL'                                                
         OC    SVCLT,SVCLT                                                      
         BZ    *+10                                                             
         MVC   REC+31(3),SVEBCCLT                                               
         MVC   REC+40(3),=C'NSI'                                                
         CLI   SVKEY+4,C'0'                                                     
         BE    *+10                                                             
         MVC   REC+40(3),=C'BBM'                                                
         MVC   REC+44(4),SVKEY+5                                                
         MVI   REC+87,C'L'                                                      
         MVC   REC+94(7),=C'CONTROL'                                            
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     FMT                                                              
         EJECT                                                                  
**********************************************************************          
* FNDMKT  -    FIND MKT NAME IN DEMAND                                          
*              R5       = FLDHDR OF RTG SVC MKT FIELD                           
**********************************************************************          
*                                                                               
FNDMKT   NTR1                                                                   
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELRMK,WORK          SAVE THE RATING SRVC MKT #                
         LA    R1,REC2                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
*                                                                               
         MVI   DBSELSRC,C'N'       NSI                                          
         CLI   SDEFKRSV,C'1'                                                    
         BNE   *+8                                                              
         MVI   DBSELSRC,C'A'       ARB                                          
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,SDEFKAGY                                                
         MVC   DBSELSTA,SDEFKSTA                                                
         L     RF,VCOMFACS         VCOMFACS                                     
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,X'10'       NOT FOUND                                    
         BNO   FNDMK10                                                          
*        MVC   8+5(20,R5),=C'UNDEFINED MKT NUMBER'                              
*                                                                               
FNDMK10  MVC   WORK2,SPACES                                                     
         L     RF,DBAREC                                                        
         USING DMKEY,RF                                                         
         LA    RF,DMFRSTEL                                                      
         DROP  RF                                                               
         USING DMELEM,RF                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8+5(0,R5),DMMNAME     GET MKT NAME                               
         DROP  RF                                                               
*                                                                               
FNDMKTX  XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
*READMKT                                                                        
* ********************************************************************          
READMKT  NTR1                                                                   
*                                  DUB HAS PACKED MKT NUMBER                    
*                                  R2 IS AT MKT FIELD                           
         MVC   KEY(17),ZEROS                                                    
         MVC   KEY(2),=C'MT'                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         LA    R5,REC2                                                          
         ST    R5,AREC                                                          
         USING MKTRECD,R5                                                       
         GOTO1 STA                                                              
         CLC   KEY(9),REC2                                                      
         BE    RDMK2                                                            
         MVI   ERRCD,NOFNDERR                                                   
         B     LFMERR                                                           
*                                                                               
RDMK2    LA    R2,LEN1(R2)                                                      
         FOUT  (R2),MKTNAME,20                                                  
*                                  NOW BE SURE MARKET HAS BEEN                  
*                                  ASSIGNED TO A RATING SERVICE MARKET          
         LA    R6,MKTRSM1                                                       
         CLC   SVKEY+4(1),MKTRS1                                                
         BE    RDMK4                                                            
         LA    R6,MKTRSM2                                                       
         CLC   SVKEY+4(1),MKTRS2                                                
         BE    RDMK4                                                            
RDMKT3   SH    R2,=H'12'           BACK UP TO MKT FIELD   8 + 4                 
         MVI   ERRCD,NORSMKT       MKT HAS NOT BEEN ASSIGNED A                  
         B     LFMERR              RARING SRVC MKT NUMBER                       
*                                                                               
RDMK4    DS    0H                                                               
         OC    0(2,R6),0(R6)       RAT SRVC MKT REQUIRED                        
         BZ    RDMKT3                                                           
         MVC   SDEFRMKT,0(R6)          STORE RS MKT                             
         LA    R2,LEN2+LEN3(R2)        DISPLAY RAT SRVC MKT                     
         EDIT  (B2,0(R6)),(4,8(R2)),0,ALIGN=LEFT                                
         LR    R5,R2               FLDHDR FOR RATING SRV MKT                    
         MVC   WORK(2),SDEFRMKT                                                 
         BAS   RE,FNDMKT           GET MKT NAME FROM DEMAND                     
         FOUT  (R2)                                                             
         XIT1                                                                   
         EJECT                                                                  
* *******************************************************************           
ADDELEM  NTR1                                                                   
         LR    R5,R2               SAVE R2                                      
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         CLI   0(R2),X'05'                                                      
         BE    ADDE4                                                            
         CLI   0(R2),0                                                          
         BE    ADDE6                                                            
*                                                                               
ADDE2    BAS   RE,NEXTEL                                                        
         BNE   ADDE6                                                            
ADDE4    CLC   2(2,R2),SDEFAMKT                                                 
         BL    ADDE2                                                            
         BH    ADDE6                                                            
         MVI   ERRCD,DUPENTRY                                                   
         LR    R2,R5                                                            
         S     R2,BACKUP                                                        
         B     LFMERR                                                           
*                                                                               
ADDE6    GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R2)                                   
         XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF REC                                   
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
MKTNUM   EQU   9                                                                
LEN1     EQU   SPLAMN1H-SPLAM1H    DISPL FROM:MKT# TO MKT NAME FLDHDR           
LEN2     EQU   SPLOS1H-SPLAMN1H    DISPL FROM:MKT NAME TO OFFSET FLDHDR         
LEN3     EQU   SPLRM1H-SPLOS1H     DISPL FROM:OFFSET TO RMS FLDHDR              
LEN4     EQU   SPLAM2H-SPLRM1H     DISPL FROM:RMS TO NEXT MKT# FLDHDR           
*                                                                               
NORSMKT  EQU   249                                                              
*                                                                               
*                                                                               
ACTSW    DC    X'00'                                                            
BACKUP   DC    F'56'                                                            
ZEROS    DC    20C'0'                                                           
         LTORG                                                                  
*DEMAND                                                                         
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
*DEDEMFILE                                                                      
       ++INCLUDE DEDEMFILE                                                      
*                                                                               
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMEF                                                                        
       ++INCLUDE SPLFMEFD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076SPLFM2F   05/01/02'                                      
         END                                                                    
