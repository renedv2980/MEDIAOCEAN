*          DATA SET NESFM42    AT LEVEL 064 AS OF 10/31/05                      
*PHASE T31C42A,+0                                                               
         TITLE 'NESFM42 -  SECTIONAL MAPS'                                      
         PRINT NOGEN                                                            
T31C42   CSECT                                                                  
         NMOD1 0,T31C42                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         CLC   =C'CHANGE',CONACT                                                
         BE    ACTERR                                                           
         CLC   =C'DELETE',CONACT                                                
         BE    ACTERR                                                           
         CLC   =C'RESTORE',CONACT                                               
         BE    ACTERR                                                           
         OI    GENSTAT4,NODELLST   CAN'T DELETE FROM LIST                       
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   VKCHK                                                            
         BAS   RE,VK                                                            
         B     RESET                                                            
VKCHK    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   DKCHK                                                            
         BAS   RE,VR                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         BAS   RE,DK                                                            
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   DLCHK                                                            
         BAS   RE,DR                                                            
         B     RESET                                                            
DLCHK    CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
         B     RESET                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   EXIT1                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
*                                                                               
RESET    MVC   KEY,SAVEKEY                                                      
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
EXIT1    OI    CONSERVH+6,X'01'                                                 
         XIT1                                                                   
*                                                                               
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING SMAPRECD,R4                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SMKYEAR                                                   
         MVC   DUB+2(2),=X'0101'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(5,CHARYEAR)                                 
*                                                                               
         MVC   SMMYEAR,CHARYEAR+6                                               
         OI    SMMYEARH+6,X'80'                                                 
*                                                                               
         MVC   SMMCODE,SMKCODE                                                  
         OI    SMMCODEH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
DR       NTR1                                                                   
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R4,AIO                                                           
         LA    R2,SMMREG1H                                                      
         LA    R3,6                                                             
*                                                                               
         GOTO1 VSETSPT                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    DR20                                                             
         DC    H'00'                                                            
*                                                                               
DR10     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
DR20     DS    0H                                                               
         USING SMREGD,R4                                                        
*                                                                               
         MVC   8(2,R2),SMREREG     REGION                                       
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
*                                                                               
         EDIT  SMREPCT,(6,DUB),2,ALIGN=LEFT                                     
         MVC   8(6,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
         BCT   R3,DR10                                                          
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
DL       NTR1                                                                   
         B     ACTERR                                                           
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,SMMREG1H                                                      
         LA    R3,6                                                             
*                                                                               
CLRSC10  DS    0H                                                               
         XC    8(L'SMMREG1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
*                                                                               
         XC    8(L'SMMPER1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
*                                                                               
         BCT   R3,CLRSC10                                                       
*                                                                               
         B     EXIT                                                             
*                                                                               
VK       NTR1                      VALIDATE KEY                                 
         XC    WORK2,WORK2                                                      
         MVC   WORK2,=XL9'0900000000010000D5'                                   
         LA    R2,WORK2                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    BINYEAR,BINYEAR                                                  
         XC    MAPCODE,MAPCODE                                                  
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST RECORDS                                 
         BNE   VK50                                                             
*                                                                               
         LA    R2,SMLYEARH                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         CLI   5(R2),2                                                          
         BNE   INVERR                                                           
*                                                                               
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVERR                                                           
         CLC   8(2,R2),=C'33'                                                   
         BH    INVERR                                                           
*                                                                               
         BAS   RE,GETBYR           GET BINARY YEAR                              
*                                                                               
         LA    R2,SMLCODEH                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         MVC   MAPCODE,SMLCODE                                                  
         OC    MAPCODE,SPACES                                                   
         B     VK100                                                            
*                                                                               
VK50     DS    0H                                                               
         LA    R2,SMMYEARH                                                      
         CLI   5(R2),0                                                          
         BE    MISERR                                                           
*                                                                               
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVERR                                                           
         BAS   RE,GETBYR           GET BINARY YEAR                              
*                                                                               
         LA    R2,SMMCODEH                                                      
         CLI   5(R2),0                                                          
         BE    MISERR                                                           
         MVC   MAPCODE,SMMCODE                                                  
         OC    MAPCODE,SPACES                                                   
*                                                                               
VK100    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SMAPRECD,R4                                                      
*                                                                               
         MVC   SMKEY(2),=X'0D3D'                                                
         MVC   SMKAGY,BAGYMD       AGENCY/MEDIA                                 
         MVC   SMKYEAR,BINYEAR     YEAR                                         
         MVC   SMKCODE,MAPCODE     MAP CODE                                     
*                                                                               
VKX      DS    0H                                                               
         MVC   SAVEKEY(13),KEY                                                  
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
VR       NTR1                                                                   
         CLI   ACTNUM,ACTSEL       CAN'T CHANGE REC FROM LIST                   
         BE    ACTERR                                                           
*                                                                               
         LA    R2,SMMREG1H                                                      
         LA    R3,6                UP TO 6 REGIONS                              
         L     R4,AIO                                                           
         XC    TOTALPCT,TOTALPCT                                                
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR10                                                             
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R4,AIO                                                           
         MVC   0(13,R4),SAVEKEY                                                 
         MVC   13(2,R4),=X'0018'                                                
*                                                                               
         MVI   ELCODE,X'01'                                                     
         B     VR30                                                             
*                                                                               
VR10     DS    0H                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,DELEL            CLEAR OUT THE REGIONS/PCTS                   
*                                                                               
VR30     DS    0H                                                               
         CLI   5(R2),0             ANYMORE TO PROCESS?                          
         BE    VR100                                                            
         CLI   5(R2),2                                                          
         BNE   REGERR                                                           
         MVC   REGION,8(R2)        REGION                                       
*                                                                               
         BAS   RE,BUMPFLD                                                       
         CLI   5(R2),0             MUST HAVE PERCENTAGE                         
         BE    MISERR                                                           
*                                                                               
         MVC   CHARPCT,8(R2)                                                    
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,CHARPCT,(R0)                                        
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
*                                                                               
         MVC   PERCENT,6(R1)                                                    
         ICM   R0,3,PERCENT                                                     
         C     R0,=F'10000'  MUST BE LESS THAN 100%                             
         BH    INVERR                                                           
*                                                                               
         L     RF,TOTALPCT                                                      
         SR    R0,R0                                                            
         ICM   R0,3,PERCENT                                                     
         AR    RF,R0                                                            
         ST    RF,TOTALPCT                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING SMREGD,R5                                                        
*                                                                               
         MVI   SMRETYPE,X'01'                                                   
         MVI   SMRELEN,SMREGLNQ                                                 
         MVC   SMREREG,REGION      REGION                                       
         MVC   SMREPCT,PERCENT     PERCENT                                      
*                                                                               
         BAS   RE,PUTEL                                                         
*                                                                               
         BAS   RE,BUMPFLD                                                       
         BCT   R3,VR30                                                          
*                                                                               
VR100    DS    0H                                                               
         L     R0,TOTALPCT                                                      
         C     R0,=F'10000'  MUST EQUAL 100%                                    
         BNE   PCTERR                                                           
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
LR       NTR1                                                                   
         OC    KEY(13),KEY         IS KEY ALL NULLS?                            
         BNZ   *+10                NO, DO A READ HIGH                           
         MVC   KEY,SAVEKEY         YES, MOVE IN SAVED KEY                       
*                                                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(3),SAVEKEY              ID/AM                                
         BNE   LRX                                                              
*                                                                               
LR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING SMAPRECD,R4                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SMKYEAR                                                   
         MVC   DUB+2(2),=X'0101'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(5,CHARYEAR)                                 
*                                                                               
         LA    R5,LISTAR           PREPARE A LIST LINE                          
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         MVC   LRYEAR,CHARYEAR+6                                                
         MVC   LRCODE,SMKCODE                                                   
*                                                                               
LR40     GOTO1 LISTMON                                                          
         B     LR10                GOTO READ SEQ                                
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
PR       DS    0H                  PRINT THE LINE                               
         B     EXIT                                                             
*                                                                               
BUMPFLD  DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
* GET BINARY YEAR                                                               
*                                                                               
GETBYR   NTR1                                                                   
         XC    CHARYEAR,CHARYEAR                                                
         MVC   CHARYEAR,=C'20  0101'                                            
         MVC   CHARYEAR+2(2),8(R2)                                              
*                                                                               
         GOTO1 DATCON,DMCB,(9,CHARYEAR),(3,DUB)                                 
         MVC   BINYEAR,DUB                                                      
         B     EXIT                                                             
*                                                                               
* GET ALPHA YEAR                                                                
*                                                                               
GETAYR   NTR1                                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(3),=X'000101'                                                
         MVC   DUB(1),BINYEAR                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(5,CHARYEAR)                                 
         B     EXIT                                                             
*                                                                               
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R4)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R4),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISERR   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
REGERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'REGION MUST BE 2 CHARACERS     '                  
         B     TRAPERR2                                                         
*                                                                               
ACTERR   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'CHANGE/DELETE/RESTORE NOT VALID'                  
         B     TRAPERR2                                                         
*                                                                               
PCTERR   DS    0H                                                               
         LA    R2,SMMPER1H                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'PERCENTAGES MUST ADD UP TO 100'                   
         B     TRAPERR2                                                         
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
                                                                                
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
*                                                                               
LLINED   DSECT                                                                  
         DS    CL2                                                              
LRYEAR   DS    CL2                                                              
         DS    CL2                                                              
LRCODE   DS    CL8                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENSMAP                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
*                                                                               
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMACD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMADD                                                       
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NEGENUSER                                                      
       ++INCLUDE NESFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
*                           *******  T31C42 WORK AREA  *******                  
WORK2    DS    CL64                                                             
DBLOCKA  DS    CL256                                                            
DEMOVAL  DS    A                                                                
CHARYEAR DS    CL8                                                              
BINYEAR  DS    XL1                 BINARY YEAR                                  
MAPCODE  DS    CL8                 MAP CODE                                     
REGION   DS    CL2                 REGION                                       
PERCENT  DS    XL2                 PERCENT                                      
CHARPCT  DS    CL6                 PERCENT (CHAR)                               
SAVEKEY  DS    XL48                                                             
TOTALPCT DS    F                   TOTAL % MUST EQUAL 100                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064NESFM42   10/31/05'                                      
         END                                                                    
