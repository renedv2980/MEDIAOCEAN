*          DATA SET SPLFM30    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T21930A,+0                                                               
         TITLE 'SPLFM30 - XSPILL DEFINITION RECORD'                             
T21930   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21930                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING XSDFRECD,R8                                                      
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
         LA    R2,SPLAM1H                                                       
         MVI   ELCODE,X'05'                                                     
         LA    R3,REC+24                                                        
         USING XSDFEL05,R3                                                      
         CLI   0(R3),X'05'                                                      
         BE    FMT20                                                            
         CLI   0(R3),0                                                          
         BE    FMT60                                                            
*                                                                               
FMT10    BAS   RE,NEXTEL                                                        
         BNE   FMT60                                                            
*                                                                               
FMT20    EDIT  XSDFAMKT,(4,8(R2)),0,ALIGN=LEFT                                  
         FOUT  (R2)                                                             
*                                  READ MKT REC AND DISPLAY NAME                
*                                                                               
         MVC   KEY(15),ZEROS                                                    
         MVC   KEY(2),=C'MT'                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB        SINCE EDIT USES DUB                          
         MVC   KEY+6(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 STA                                                              
         USING MKTRECD,R7                                                       
         BAS   RE,BUMP             BUMP TO NAME FLDHDR                          
         FOUT  (R2),MKTNAME,20                                                  
         DROP  R7                                                               
         BAS   RE,BUMP             BUMP TO OFFSET FLDHDR                        
         EDIT  XSDFOSET,(4,8(R2)),0,FLOAT=-,ZERO=BLANK                          
         FOUT  (R2)                                                             
         BAS   RE,BUMP             BUMP TO RSM FLDHDR                           
*                                                                               
         CLC   XSDFRMKT,=X'FFFF'                                                
         BNE   FMT30                                                            
         MVC   8(4,R2),=C'HOME'                                                 
         FOUT  (R2)                                                             
         BAS   RE,BUMP             BUMP TO RMS NAME                             
         FOUT  (R2),SPACES,30                                                   
         B     FMT50                                                            
*                                                                               
FMT30    EDIT  XSDFRMKT,(4,8(R2)),0,ALIGN=LEFT                                  
         FOUT  (R2)                                                             
         MVC   WORK(2),XSDFRMKT                                                 
         BAS   RE,BUMP             PASS RMS NAME                                
         BAS   RE,FNDMKT                                                        
         FOUT  (R2)                                                             
*                                                                               
FMT50    BAS   RE,BUMP             NEXT MKT # FLDHDR                            
         BCT   R6,FMT10                                                         
         BAS   RE,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                TOO MANY ELEMS                               
*                                                                               
FMT60    DS    0H                                                               
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
* *********************************************************************         
*EDT -   VALIDATE RECORD INPUT FIELDS                                           
* *********************************************************************         
         SPACE 1                                                                
EDT      DS    0H                                                               
         MVI   HOMEFLG,C'N'                                                     
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
*                                                                               
         CLI   SVACT,C'A'                                                       
         BE    EDT10                                                            
         CLI   SVACT,C'R'                                                       
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVC   KEY,SVKEY                                                        
         GOTO1 READ                NEEDED FOR DMWRT IN WRITE                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'R'                                                       
         BNE   EDT50                                                            
         MVI   KEY+13,0           CLEAR DELETE FLAG                             
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVI   REC+15,0                                                         
         GOTO1 PUTREC                                                           
         B     FMT                                                              
*                                                                               
EDT10    MVC   XSDFLEN,=H'24'      INITIALIZE REC LENGTH                        
         B     EDT70                                                            
*                                                                               
EDT50    MVI   ELCODE,X'05'                                                     
         LA    R3,REC+24                                                        
         CLI   0(R3),X'05'                                                      
         BE    EDT60                                                            
         CLI   0(R3),0                                                          
         BE    EDT70                                                            
         BAS   RE,NEXTEL                                                        
         BNE   EDT70                                                            
*                                                                               
EDT60    GOTO1 VRECUP,DMCB,(0,REC),0(R3)                                        
         B     EDT50                                                            
*                                                                               
EDT70    LA    R6,MKTNUM           SET R6 FOR BCT                               
         LA    R2,SPLAM1H                                                       
         LA    R3,ELEM                                                          
         USING XSDFEL05,R3                                                      
         MVI   ACTSW,0                                                          
*                                                                               
EDT80    XC    ELEM(20),ELEM                                                    
         MVC   ELEM(2),=X'050A'                                                 
         CLI   5(R2),0             NO INPUT IN AGY MKT FIELD                    
         BNE   EDT100                                                           
*                                                                               
EDT90    BAS   RE,BUMP             CLEAR OLD MKT NAME                           
         FOUT  (R2),SPACES,20                                                   
         LA    R5,4                                                             
*                                                                               
EDT95    BAS   RE,BUMP             BUMP TO NEXT MKT                             
         BCT   R5,EDT95                                                         
         B     NEXTMKT                                                          
*                                                                               
EDT100   ZIC   R5,5(R2)            FLD LENGHT                                   
         LTR   R5,R5                                                            
         BZ    EDTERR                                                           
*                                                                               
         CLC   8(3,R2),=C'DEL'     CHK FOR SPECIAL ELEM DELETE                  
         BE    EDT90               YES JUST SKIP TO NEXT MKT                    
         OI    ACTSW,X'01'         SET ON ACTIVITY SWITCH                       
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC INPUT                          
         BNO   EDTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,XSDFAMKT                                                      
*                                                                               
         CLC   SVMKT,XSDFAMKT      COMPARE STAT MKT SAVED IN SVMKT              
         BE    EDTERR              CAN'T SPILL TO ITSELF                        
         BAS   RE,READMKT          VALIDATE MKT #                               
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         XC    XSDFOSET,XSDFOSET                                                
         CLI   5(R2),0             NOT INPUT                                    
         BE    EDT160                                                           
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
         STH   R5,XSDFOSET                                                      
*                                                                               
EDT160   BAS   RE,BUMP             POINT TO RMS FLDHDR                          
         CLI   SVACT,C'A'                                                       
         BNE   EDT165                                                           
         OC    XSDFRMKT,XSDFRMKT                                                
         BZ    EDT165                                                           
         CLC   XSDFRMKT,=X'FFFF'                                                
         BNE   EDT162                                                           
         MVC   8(4,R2),=C'HOME'                                                 
         LH    R0,=H'4'                                                         
         B     EDT164                                                           
*                                                                               
EDT162   EDIT  XSDFRMKT,(4,8(R2)),0,ALIGN=LEFT                                  
         OI    4(R2),X'08'         VALID NUMERIC INPUT                          
*                                                                               
EDT164   STC   R0,5(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
EDT165   ZIC   R1,5(R2)            IF NOTHING INPUT                             
         LTR   R1,R1                                                            
         BNZ   EDT172                                                           
         OC    SVRMKT,SVRMKT       USE RSMKT FROM MARKET RECORD                 
         BZ    MISSERR                                                          
         MVC   XSDFRMKT,SVRMKT                                                  
         B     EDT162                                                           
*                                                                               
EDT172   CLC   =C'HOME',8(R2)                                                   
         BNE   EDT170                                                           
         CLI   HOMEFLG,C'Y'        ONLY ONE HOME ALLOWED                        
         BE    EDTERR                                                           
         MVC   XSDFRMKT,=X'FFFF'                                                
         MVI   HOMEFLG,C'Y'                                                     
         BAS   RE,BUMP             BUMP PAST RMS NAME                           
         B     EDT200                                                           
*                                                                               
EDT170   TM    4(R2),X'08'         VALID NUMERIC INPUT                          
         BNO   EDTERR                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,XSDFRMKT                                                      
         MVC   WORK(2),XSDFRMKT                                                 
         BAS   RE,BUMP                                                          
         BAS   RE,FNDMKT           GET MKT NAME FROM DEMAND                     
*                                                                               
EDT200   BAS   RE,ADDELEM                                                       
         BAS   RE,BUMP                                                          
*                                                                               
NEXTMKT  BCT   R6,EDT80                                                         
         B     WRITE                                                            
*                                                                               
WRITE    DS    0H                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
*                                                                               
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
*              R2       = FLDHDR OF RTG SVC MKT NAME FIELD                      
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
         CLI   XSDFKRSV,C'1'                                                    
         BNE   *+8                                                              
         MVI   DBSELSRC,C'A'       ARB                                          
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,XSDFKAGY                                                
         MVC   DBSELSTA,XSDFKSTA                                                
         MVI   DBSELMED,C'C'       CANDIAN                                      
         L     RF,VCOMFACS         VCOMFACS                                     
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         TM    DBERROR,X'10'       NOT FOUND                                    
         BNO   FNDMK10                                                          
         CLI   SVFMTSW,0           IF EDIT GIVE ERROR MESSAGE                   
         BNE   EDTERR                                                           
*                                                                               
FNDMK10  MVC   WORK2,SPACES                                                     
         L     RF,DBAREC                                                        
         USING DMKEY,RF                                                         
         LA    RF,DMFRSTEL                                                      
         DROP  RF                                                               
         USING DMELEM,RF                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         CH    R1,=H'29'                                                        
         BNH   *+8                                                              
         LA    R1,29                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DMMNAME     GET MKT NAME                                 
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
         MVC   KEY(15),ZEROS                                                    
         MVC   KEY(2),=C'MT'                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         USING MKTRECD,R7                                                       
         GOTO1 STA                                                              
         CLC   KEY(9),REC2                                                      
         BE    RDMK2                                                            
         MVI   ERRCD,NOFNDERR                                                   
         B     LFMERR                                                           
*                                                                               
RDMK2    BAS   RE,BUMP                                                          
         FOUT  (R2),MKTNAME,20                                                  
         XC    SVRMKT,SVRMKT       GET RATING SERVICE MARKET                    
         LA    R6,MKTRSM1                                                       
         CLC   SVKEY+4(1),MKTRS1                                                
         BE    RDMK4                                                            
         LA    R6,MKTRSM2                                                       
         CLC   SVKEY+4(1),MKTRS2                                                
         BNE   RDMKX                                                            
*                                                                               
RDMK4    MVC   SVRMKT,0(R6)            STORE RS MKT                             
*                                                                               
RDMKX    XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
* *******************************************************************           
ADDELEM  NTR1                                                                   
         LR    R7,R3               A(ELEMENT JUST BUILT)                        
         LA    R3,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         CLI   0(R3),X'05'                                                      
         BE    ADDE4                                                            
         CLI   0(R3),0                                                          
         BE    ADDE6                                                            
*                                                                               
ADDE2    BAS   RE,NEXTEL                                                        
         BNE   ADDE6                                                            
*                                                                               
ADDE4    CLC   2(2,R3),2(R7)                                                    
         BL    ADDE2                                                            
         BH    ADDE6                                                            
         MVI   ERRCD,DUPENTRY                                                   
         B     LFMERR                                                           
*                                                                               
ADDE6    GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R3)                                   
         XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             END OF REC                                   
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R3,R3                                                            
         BR    RE                                                               
         SPACE 3                                                                
BUMP     ZIC   R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
EDTERR   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
MISSERR  MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
MKTNUM   EQU   9                                                                
ACTSW    DC    X'00'                                                            
HOMEFLG  DC    C'N'                                                             
ZEROS    DC    20C'0'                                                           
SVRMKT   DS    XL2                                                              
         EJECT                                                                  
*DEMAND                                                                         
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*SPLFMD0                                                                        
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMD0                                                                        
       ++INCLUDE SPLFMD0D                                                       
         EJECT                                                                  
*                                                                               
*SPGENXSDF                                                                      
       ++INCLUDE SPGENXSDF                                                      
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*DEDEMFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
*                                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPLFM30   05/01/02'                                      
         END                                                                    
