*          DATA SET SPCSO0B    AT LEVEL 026 AS OF 07/22/02                      
*PHASE T2180BA                                                                  
         TITLE 'T2180B - CHILD SPOT MASTER EST LIST MAINTENANCE'                
T2180B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2180B                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MYOVNUM,X'0B'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    MESMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'0B'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,MESMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    MESCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,MESCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKDATE                                                           
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKDATE   LA    R2,MESDATEH         VALIDATE START DATE                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
*                                                                               
VKPASS   LA    R2,MESPASSH         VALIDATE PASSWORD                            
         CLC   8(3,R2),=C'MEL'                                                  
         BE    VK50                                                             
         CLC   8(4,R2),=C'ALAN'                                                 
         BNE   INVERR                                                           
*                                                                               
VK50     LA    R6,KEY              BUILD KEY                                    
         USING MASKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   MASKTYPE,MASKTYPQ   MAS RECORD TYPE                              
         MVI   MASKSTYP,MASKSTPQ   MAS RECORD SUB-TYPE                          
         MVC   MASKAM,BAGYMD                                                    
         MVC   MASKCLT,BCLT                                                     
         MVC   MASKDATE,THISDATE                                                
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION = ADD?                                
         BE    VKXX                YES...DON'T READHI                           
*                                                                               
         MVC   SAVEKEY,KEY         SAVE OFF KEY                                 
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      MATCH ON CLIENT?                             
         BNE   VKX                 NO...LET GENCON GIVE REC NOT FOUND           
         CLC   KEY(11),KEYSAVE     MATCH ON START DATE?                         
         BE    VKX                 YES                                          
         XC    MESDATE,MESDATE     CLEAR START DATE INPUT BY USER               
         MVC   TEMPDATE,KEY+5      DISPLAY DATE FOUND IN REC                    
         GOTO1 DATCON,DMCB,(0,TEMPDATE),(11,MESDATE)                            
         OI    MESDATEH+6,X'80'    XMIT                                         
         B     VKXX                DON'T REBUILD KEY!                           
*                                                                               
VKX      MVC   KEY,SAVEKEY                                                      
VKXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R6,AIO              BUILD RECORD FROM SCRATCH                    
         USING MASRECD,R6                                                       
         XC    0(256,R6),0(R6)                                                  
         MVC   MASKEY,KEY                                                       
         MVC   MASLEN,DATADISP     INSERT RECORD LENGTH                         
         MVC   MASAGYA,AGENCY      AND ALPHA AGENCY CODE                        
         DROP  R6                                                               
*                                                                               
         LA    R6,ELEM             BUILD BUY SPLIT DATE ELEMENT                 
         XC    ELEM,ELEM                                                        
         USING MSDELEM,R6                                                       
         MVI   MSDCODE,MSDCODEQ                                                 
         MVI   MSDLEN,MSDLENQ                                                   
         LA    R2,MESSPLTH         VALIDATE SPLIT DATE                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),MSDDATE                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         MVC   DUB(6),MSDDATE      MAKE SURE SPLIT IS A MONDAY                  
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLI   0(R1),1                                                          
         BNE   ERRMON                                                           
*                                                                               
         CLC   MSDDATE,KEY+5       SPLIT DATE AFTER START DATE?                 
         BNH   ERRHIGH             NO...ERROR                                   
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         LA    R6,ELEM             BUILD MASTER ESTIMATE LIST ELEMENT           
         XC    ELEM,ELEM                                                        
         USING MELELEM,R6                                                       
         MVI   MELCODE,MELCODEQ                                                 
         MVI   MELLEN,MELLENQ                                                   
         LA    R7,MELLIST          POINT R7 TO MASTER EST LIST                  
         USING MESTLSTD,R7                                                      
         DROP  R6                                                               
*                                                                               
         LA    R2,MESMESTH                                                      
*                                                                               
VR20     CLI   0(R2),0             TEST END OF SCREEN                           
         BE    VRX                                                              
         CLI   5(R2),0             TEST NO MORE MASTER ESTIMATES                
         BNE   VK25                                                             
*                                                                               
         LA    R3,4                EMPTY SLOT....COULD BE DATA AFTER            
VR21     ZIC   R0,0(R2)            THIS, JUST BUMP TO NEXT FIELD                
         AR    R2,R0                                                            
         BCT   R3,VR21                                                          
         B     VR20                                                             
*                                                                               
VK25     GOTO1 VALIREF             VALIDATE MASTER ESTIMATE NUMBER              
         CVB   R3,DUB              VREF IN CSO00 PUT ESTIMATE IN DUB            
         CHI   R3,255                                                           
         BH    ERRHI                                                            
*                                                                               
         OC    BREF,BREF                                                        
         BZ    INVERR              ZERO NOT VALID                               
*                                                                               
         BAS   RE,RDEST            READ EST & SEE IF VALID                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         BAS   RE,STARTEQU         EST START MUST =  MASTER START               
         MVI   LASTEST,0           RESET FOR SUB-EST                            
         MVC   MESTNUM,BREF        ESTIMATE NUMBER                              
*                                                                               
         ZIC   R0,0(R2)            VALIDATE T/C FIELD                           
         AR    R2,R0                                                            
         MVC   MESTTYPE,8(R2)                                                   
         CLI   8(R2),C'T'                                                       
         BE    VR30                                                             
         CLI   8(R2),C'C'                                                       
         BNE   INVERR                                                           
*                                                                               
VR30     ZIC   R0,0(R2)            VALIDATE SPTLN FIELD                         
         AR    R2,R0                                                            
                                                                                
         GOTO1 VALIREF                                                          
         CVB   R3,DUB              VREF IN CSO00 PUT ESTIMATE IN DUB            
         CHI   R3,30               SPOT LENGTH CAN BE 30 OR 60                  
         BE    *+12                                                             
         CHI   R3,60                                                            
         BNE   ERRSPOT                                                          
*                                                                               
         MVC   MESTSPLN,BREF                                                    
*                                                                               
         ZIC   R0,0(R2)            VALIDATE SUBEST LIST FIELD                   
         AR    R2,R0                                                            
         XC    SCANOUT,SCANOUT                                                  
         GOTO1 SCANNER,DMCB,(R2),SCANOUT                                        
         CLI   4(R1),1             ONE TO FIVE SUBESTS ALLOWED ONLY             
         BL    INVERR                                                           
         CLI   4(R1),5                                                          
         BH    ERRMAX5                                                          
         ZIC   R4,4(R1)                                                         
         LA    R5,SCANOUT                                                       
         LA    R3,MESTSUBS                                                      
*                                                                               
VR40     CLI   1(R5),0             NO SECOND HALF OF FIELD ALLOWED              
         BNE   INVERR                                                           
         TM    2(R5),X'80'         TEST VALID NUMBERIC                          
         BZ    INVERR                                                           
         OC    4(4,R5),4(R5)       TEST NON-ZERO                                
         BZ    INVERR                                                           
         ICM   R6,15,4(R5)                                                      
         CHI   R6,255              ESTIMATE MAX = 255                           
         BH    ERRHI                                                            
         MVC   BREF,7(R5)          ESTIMATE NUMBER                              
         BAS   RE,RDEST            READ EST AND SEE IF VALID                    
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   LASTEST,0           FIRST TIME THROUGH?                          
         BNE   *+12                NO                                           
         BAS   RE,STARTEQU         FIRST EST-SUB START = MASTER START           
         B     *+8                                                              
         BAS   RE,ECONTIG          LAST END DATE MUST BE CONTIGUOUS             
         MVC   0(1,R3),7(R5)       ADD SUBEST TO LIST                           
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R5,32(R5)           BUMP TO NEXT FIELD                           
         BCT   R4,VR40                                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         LA    R7,MESTLSTL(R7)     POINT TO NEXT MASTER ESTIMATE                
         B     VR20                                                             
*                                                                               
VRX      GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,MESSPLTH),MESLAST                                 
*                                                                               
         LA    R2,MESMESTH         POINT TO FIRST DISPLAY LINE                  
*                                                                               
         L     R6,AIO              POINT TO BUY SPLIT DATE ELEMENT              
         MVI   ELCODE,MSDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MSDELEM,R6                                                       
         GOTO1 DATCON,DMCB,(0,MSDDATE),(5,MESSPLT)                              
*                                                                               
         L     R6,AIO              POINT TO MASTER EST LIST ELEMENT             
         MVI   ELCODE,MELCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MELELEM,R6                                                       
*                                                                               
         LA    R7,MELLIST          POINT TO MASTER EST LIST                     
         USING MESTLSTD,R7                                                      
*                                                                               
DR10     CLI   0(R7),0             TEST END OF LIST                             
         BE    DRX                                                              
*                                  DISPLAY MASTER EST NUMBER                    
         EDIT  (1,MESTNUM),(3,8(R2)),ALIGN=LEFT                                 
*                                                                               
         ZIC   R0,0(R2)            DISPLAY T/C                                  
         AR    R2,R0                                                            
         MVC   8(1,R2),MESTTYPE                                                 
*                                                                               
         ZIC   R0,0(R2)            DISPLAY SPTLN                                
         AR    R2,R0                                                            
         EDIT  (1,MESTSPLN),(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         ZIC   R0,0(R2)            DISPLAY SUBEST LIST                          
         AR    R2,R0                                                            
         LA    R4,8(R2)                                                         
         LA    R3,MESTSUBS                                                      
         LA    R5,5                MAX SUBSETS                                  
*                                                                               
*                                  DISPLAY SUBEST                               
DR20     EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AR    R4,R0                                                            
         LA    R3,1(R3)                                                         
*                                                                               
** *** HOLY SHIT IS THIS WRONG (WITHOUT THE BCT)*****************               
**       CLI   0(R3),0             IF NO MORE SUBESTS THEN LEAVE*               
**       BE    DR30                                             *               
** *** HOLY SHIT IS THIS WRONG (WITHOUT THE BCT)*****************               
*                                                                               
         CLI   0(R3),0             IF NO MORE SUBESTS THEN LEAVE                
         BE    DR30                                                             
         MVI   0(R4),C','          ELSE DISPLAY NEXT ONE                        
         LA    R4,1(R4)                                                         
         BCT   R5,DR20                                                          
         BCTR  R4,0                                                             
         MVI   0(R4),0             NO F@CK*NG "," AFTER LAST SUBEST             
*                                                                               
DR30     ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         LA    R7,MESTLSTL(R7)     POINT TO NEXT MASTER ESTIMATE                
         B     DR10                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* MAKE SURE ESTIMATE EXISTS                                           *         
***********************************************************************         
RDEST    NTR1                                                                   
         MVC   SAVEKEY,KEY         SAVE OFF KEY                                 
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTRECD,R6                                                       
         MVI   EKEYTYPE,EKEYTYPQ                                                
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BREF                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BNE   ERRESTNF            ESTIMATE NOT FOUND!                          
         MVC   AIO,AIO3            DONT CLOBBER AIO1!                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* NOW MAKE SURE ESTIMATE START DATE = START DATE IN MASTER EST        *         
***********************************************************************         
STARTEQU DS    0H                                                               
         USING ESTRECD,R6                                                       
         CLC   ESTART,SAVEKEY+5    START DATE                                   
         BNE   ERRESTDT            EST START DATE != MASTER START DATE          
         MVC   LASTEND,EEND        SAVE END DATE                                
         MVC   LASTEST,BREF        LAST ESTIMATE                                
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO AREA USED FOR MASTER REC         
         MVC   KEY,SAVEKEY         RESTORE KEY OF MASTER REC                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MAKE SURE LAST EST END DATE IS CONTIGUOUS WITH THIS EST START DATE  *         
***********************************************************************         
ECONTIG  NTR1                                                                   
         USING ESTRECD,R6                                                       
         GOTO1 ADDAY,DMCB,LASTEND,LASTEND,F'1'                                  
         CLC   ESTART,LASTEND      START DATE = LASTDATE+1?                     
         BNE   ERRCONT             NO                                           
         MVC   LASTEND,EEND        SAVE END DATE                                
         MVC   LASTEST,BREF        LAST ESTIMATE                                
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO AREA USED FOR MASTER REC         
         MVC   KEY,SAVEKEY         RESTORE KEY OF MASTER REC                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRMON   BAS   RE,SETCUR                                                        
         MVC   8(32,R2),=C'SPLIT DATE MUST FALL ON A MONDAY'                    
         GOTO1 ERREX2                                                           
*                                                                               
ERRHIGH  BAS   RE,SETCUR                                                        
         MVC   8(35,R2),=C'SPLIT DATE MUST BE AFTER START DATE'                 
         GOTO1 ERREX2                                                           
*                                                                               
ERRESTNF BAS   RE,SETCUR                                                        
         MVC   8(8,R2),=C'ESTIMATE'                                             
         EDIT  (1,BREF),(3,17(R2)),ALIGN=LEFT                                   
         BAS   RE,SETCUR2                                                       
         MVC   21(9,R2),=C'NOT FOUND'                                           
         GOTO1 ERREX2                                                           
*                                                                               
ERRESTDT BAS   RE,SETCUR                                                        
         MVC   8(8,R2),=C'ESTIMATE'                                             
         EDIT  (1,BREF),(3,17(R2)),ALIGN=LEFT                                   
         BAS   RE,SETCUR2                                                       
         MVC   21(38,R2),=C'START DATE DOES NOT MATCH MASTER START'             
         GOTO1 ERREX2                                                           
*                                                                               
ERRCONT  BAS   RE,SETCUR                                                        
         MVC   8(8,R2),=C'ESTIMATE'                                             
         EDIT  (1,LASTEST),(3,17(R2)),ALIGN=LEFT                                
         BAS   RE,SETCUR3                                                       
         MVC   21(28,R2),=C'NOT CONTIGUOUS WITH ESTIMATE'                       
         EDIT  (1,BREF),(3,50(R2)),ALIGN=LEFT                                   
         GOTO1 ERREX2                                                           
*                                                                               
ERRSPOT  BAS   RE,SETCUR                                                        
         MVC   8(28,R2),=C'SPOT LENGTH MUST BE 30 OR 60'                        
         GOTO1 ERREX2                                                           
*                                                                               
ERRHI    BAS   RE,SETCUR                                                        
         MVC   8(30,R2),=C'ESTIMATE MUST BE BETWEEN 1-255'                      
         GOTO1 ERREX2                                                           
*                                                                               
ERRMAX5  BAS   RE,SETCUR                                                        
         MVC   8(27,R2),=C'CAN HAVE MAX 5 SPOT LENGTHS'                         
         GOTO1 ERREX2                                                           
*                                                                               
SETCUR   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         BR    RE                                                               
*                                                                               
SETCUR2  DS    0H                                                               
         CLI   BREF,99                                                          
         BH    SCUR2X                                                           
         BCTR  R2,0                                                             
         CLI   BREF,9                                                           
         BH    SCUR2X                                                           
         BCTR  R2,0                                                             
SCUR2X   BR    RE                                                               
*                                                                               
SETCUR3  DS    0H                                                               
         CLI   LASTEST,99                                                       
         BH    SCUR3X                                                           
         BCTR  R2,0                                                             
         CLI   LASTEST,9                                                        
         BH    SCUR3X                                                           
         BCTR  R2,0                                                             
SCUR3X   BR    RE                                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOFBD                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
TEMPDATE DS    CL6                                                              
LASTEND  DS    CL6                                                              
LASTEST  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPCSO0B   07/22/02'                                      
         END                                                                    
