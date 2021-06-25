*          DATA SET NESFM09    AT LEVEL 148 AS OF 06/08/09                      
*PHASE T31C09A,*                                                                
*INCLUDE GETBROAD                                                               
***********************************************************************         
*                                                                               
*  TITLE: T31C09 - AGENCY RECORD                                                
*                                                                               
***********************************************************************         
         TITLE 'T31C09 AGENCY RECORD'                                           
T31C09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,AGENCY,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,AGYSAGYH                                                      
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         CLI   5(R2),2                                                          
         JNE   INVLFLD                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         MVI   0(R6),X'06'         AGENCY HEADER RECORD                         
         MVC   AGYKAGY,8(R2)       AGENCY                                       
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                                                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RECERR                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR10                                                             
*                                                                               
         LA    R2,AGYSAGYH                                                      
         MVI   0(R6),X'06'         BUILD AGENCY HEADER RECORD                   
         MVC   AGYKAGY,8(R2)       AGENCY                                       
*                                                                               
         MVC   AGYLEN,=AL2(150)                                                 
         MVC   AGYEL(2),=X'017E'                                                
         XC    AGYPROF,AGYPROF                                                  
*                                                                               
VR10     DS    0H                                                               
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
         LA    R2,AGYSNMEH         AGENCY NAME                                  
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         MVC   AGYNAME,8(R2)                                                    
*                                                                               
         LA    R2,AGYSADDH         AGENCY ADDRESS                               
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         MVC   AGYADDR,8(R2)                                                    
*                                                                               
         LA    R2,AGYSIDH          AGENCY ID                                    
         CLI   5(R2),2                                                          
         JL    INVLFLD                                                          
         MVC   AGYID,8(R2)                                                      
*                                                                               
         LA    R2,AGYALPHH         AGENCY ALPHA                                 
         CLI   ACTNUM,ACTADD                                                    
         BE    VR20                                                             
         CLC   AGYPROF+17(2),8(R2)                                              
         BE    VR20                                                             
         TM    4(R2),X'80'         CAN'T CHANGE THIS FIELD                      
         JO    CHGERR                                                           
*                                                                               
VR20     DS    0H                                                               
         CLI   8(R2),X'40'                                                      
         JE    INVLFLD                                                          
         CLI   9(R2),X'40'                                                      
         JE    INVLFLD                                                          
         MVC   AGYPROF+17(2),8(R2)                                              
*                                                                               
         LA    R2,AGYHEXH          AGENCY HEX                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    *+12                                                             
         TM    4(R2),X'80'         CAN'T CHANGE THIS FIELD                      
         JO    CHGERR                                                           
*                                                                               
         MVI   SVAGYHEX,0                                                       
         LA    R3,AGYTABL                                                       
         BAS   RE,TABLOOK2                                                      
         MVC   SVAGYHEX,BYTE                                                    
*                                                                               
         CLI   8(R2),C'0'                                                       
         BNE   *+12                                                             
         CLI   SAVEKEY+1,C'Z'       SPECIAL DDS AGYHEADERS                      
         JNE   INVLFLD              ELSE 0 IS INVALID                           
*                                                                               
         MVC   AGYPROF+19(1),8(R2)                                              
*                                                                               
         LA    R2,AGYSACCH         ACCOUNT NUMBER                               
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         MVC   AGYACCT,8(R2)                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'03',(R6))                           
*                                                                               
         LA    R2,AGYACCH          ACC AGYS                                     
         CLI   5(R2),0                                                          
         BE    VR70                                                             
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING AGYACCEL,R6                                                      
*                                                                               
         MVC   ELEM(2),=X'031A'                                                 
*                                                                               
         LA    R3,AGYACC                                                        
         LA    R4,AGYACCAG                                                      
         DROP  R6                                                               
*                                                                               
VR50     DS    0H                                                               
         CLI   0(R3),C' '                                                       
         BNH   VR60                GO UPDATE RECORD                             
         MVC   0(2,R4),0(R3)                                                    
         LA    R3,3(R3)                                                         
         LA    R4,2(R4)                                                         
         B     VR50                                                             
*                                                                               
VR60     DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',SPTFILE),(X'03',(R6)),ELEM,0                    
*                                                                               
VR70     DS    0H                                                               
         XC    SVLKDATE,SVLKDATE                                                
         LA    R2,AGYOPTSH         AGENCY OPTIONS                               
         CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         BRAS  RE,VALOPTS          VALIDATE OPTIONS                             
*                                                                               
VR100    DS    0H                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         LA    R2,AGYRATSH         RATING SERVICE                               
         CLI   5(R2),0                                                          
         BE    VR110                                                            
*                                                                               
         LA    R3,RATSTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF(1),8(R2)                                                 
*                                                                               
VR110    DS    0H                                                               
         LA    R2,AGYCLRGH         CLIENT REGIONS                               
         CLI   5(R2),0                                                          
         BE    VR120                                                            
*                                                                               
         LA    R3,CLRGTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+1(1),8(R2)                                               
*                                                                               
VR120    DS    0H                                                               
         LA    R2,AGYBLPCH         BILLING PERCENTAGE                           
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         CLI   5(R2),2                                                          
         JNE   INVLFLD                                                          
*                                                                               
         TM    4(R2),X'08'                                                      
         JZ    NUMERR                                                           
         MVC   AGYPROF+2(2),8(R2)                                               
*                                                                               
VR130    DS    0H                                                               
         LA    R2,AGYEXDMH         EXTENDED DEMOS                               
         CLI   5(R2),0                                                          
         BE    VR140                                                            
*                                                                               
         LA    R3,EXDMTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+4(1),8(R2)                                               
*                                                                               
VR140    DS    0H                                                               
         LA    R2,AGYBOTOH         BUY PERIOD OTO'S                             
         CLI   5(R2),0                                                          
         BE    VR150                                                            
*                                                                               
         LA    R3,BOTOTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+5(1),8(R2)                                               
*                                                                               
VR150    DS    0H                                                               
         LA    R2,AGYCANH          CANADIAN?                                    
         CLI   5(R2),0                                                          
         BE    VR160                                                            
*                                                                               
         LA    R3,CANTABL                                                       
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+7(1),8(R2)                                               
*                                                                               
VR160    DS    0H                                                               
         LA    R2,AGYTMSHH         OLD POOL TSHEETS                             
         CLI   5(R2),0                                                          
         BE    VR170                                                            
*                                                                               
         LA    R3,NWTATABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+8(1),8(R2)                                               
*                                                                               
VR170    DS    0H                                                               
         LA    R2,AGYBILLH         BILLING                                      
         CLI   5(R2),0                                                          
         BE    VR180                                                            
*                                                                               
         LA    R3,BILLTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+16(1),8(R2)                                              
*                                                                               
VR180    DS    0H                                                               
         LA    R2,AGYBIRQH         BUY ID REQUIRED?                             
         CLI   5(R2),0                                                          
         BE    VR190                                                            
*                                                                               
         LA    R3,BIDTABL                                                       
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+9(1),8(R2)                                               
*                                                                               
VR190    DS    0H                                                               
         MVI   AGYPROF+10,C'0'                                                  
         LA    R2,AGYCBLH          CREDIT BUY LIMIT                             
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         TM    4(R2),X'08'                                                      
         JZ    NUMERR                                                           
*                                                                               
         MVC   AGYPROF+10(1),8(R2)                                              
*                                                                               
VR200    DS    0H                                                               
         MVI   AGYPROF+11,C'0'                                                  
         LA    R2,AGYRPOLH         BRAND POL RADIO                              
         CLI   5(R2),0                                                          
         BE    VR210                                                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'0'                                                       
         JNE   INVLFLD                                                          
*                                                                               
         MVC   AGYPROF+11(1),8(R2)                                              
*                                                                               
VR210    DS    0H                                                               
         MVI   AGYPROF+15,C'N'                                                  
         LA    R2,AGYXOTOH         SPCL -OTO CODE                               
         CLI   5(R2),0                                                          
         BE    VR220                                                            
*                                                                               
         MVC   AGYPROF+15(1),8(R2)                                              
*                                                                               
VR220    DS    0H                                                               
         MVI   AGYPROF+12,C'N'                                                  
         LA    R2,AGYMGDMH         MKGDS IN MISSED MTH                          
         CLI   5(R2),0                                                          
         BE    VR230                                                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         JNE   INVLFLD                                                          
*                                                                               
         MVC   AGYPROF+12(1),8(R2)                                              
*                                                                               
VR230    DS    0H                                                               
         MVI   AGYPROF+6,C'N'                                                   
         LA    R2,AGYSAUTH         -S AUTH REQUIRED                             
         CLI   5(R2),0                                                          
         BE    VR240                                                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         JNE   INVLFLD                                                          
*                                                                               
         MVC   AGYPROF+6(1),8(R2)                                               
*                                                                               
VR240    DS    0H                                                               
         MVI   AGYPROF+13,C'N'                                                  
         LA    R2,AGYOFRQH         OFFICE REQUIRED                              
         CLI   5(R2),0                                                          
         BE    VR250                                                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         JNE   INVLFLD                                                          
*                                                                               
         MVC   AGYPROF+13(1),8(R2)                                              
*                                                                               
VR250    DS    0H                                                               
         MVI   AGYPROF+14,C'N'                                                  
         LA    R2,AGYBYBLH         BUYER/BILLER                                 
         CLI   5(R2),0                                                          
         BE    VR260                                                            
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         JNE   INVLFLD                                                          
*                                                                               
         MVC   AGYPROF+14(1),8(R2)                                              
*                                                                               
VR260    DS    0H                                                               
         MVI   AGYOFC2,C'N'                                                     
         LA    R2,AGYACOCH         ACC OFFICE CODE                              
         CLI   8(R2),C'Y'                                                       
         BNE   VR270                                                            
*                                                                               
         MVI   AGYOFC2,C'Y'                                                     
*                                                                               
VR270    DS    0H                                                               
         NI    AGYFLAG2,X'FF'-AGYFLAG2_2DP                                      
         NI    AGYFLAG2,X'FF'-AGYFLAG2_BDP                                      
*                                                                               
         LA    R2,AGYPRECH         2 DEC PRECISION                              
         CLI   8(R2),0                                                          
         BE    VR280                                                            
*                                                                               
         CLI   8(R2),C'Y'          2 DECIMAL PRECISION?                         
         BNE   *+12                                                             
         OI    AGYFLAG2,AGYFLAG2_2DP                                            
         B     VR280                                                            
*                                                                               
         CLI   8(R2),C'B'          USER DEFINED PRECISION?                      
         BNE   *+12                                                             
         OI    AGYFLAG2,AGYFLAG2_BDP                                            
         B     VR280                                                            
*                                                                               
         CLI   8(R2),C'N'                                                       
         JNE   INVLFLD                                                          
*                                                                               
VR280    DS    0H                                                               
         NI    AGYFLAG2,X'FF'-AGYFLAG2_XPR                                      
*                                                                               
         LA    R2,AGYXPRDH         2 DEC PRECISION                              
         CLI   8(R2),0                                                          
         BE    VR285                                                            
*                                                                               
         CLI   8(R2),C'Y'          2 DECIMAL PRECISION?                         
         BNE   *+8                                                              
         OI    AGYFLAG2,AGYFLAG2_XPR                                            
         DROP  R6                                                               
*                                                                               
VR285    L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'70',(R6))                           
*                                                                               
         LA    R2,AGYTTLEH         ID TITLE                                     
         CLI   5(R2),0             IF NO INPUT, CHECK PROFILE TO SEE            
         BE    VR290               IF REQUIRED                                  
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING AGYIDEL,R6                                                       
*                                                                               
         MVC   ELEM(2),=X'700C'                                                 
         MVC   AGYTITLE,8(R2)                                                   
         OC    AGYTITLE,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',SPTFILE),(X'70',(R6)),ELEM,0                    
         B     VR300                                                            
*                                                                               
VR290    DS    0H                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         CLI   AGYPROF+9,C'Y'                                                   
         JE    MISSFLD             ID TITLE REQUIRED                            
         CLI   AGYPROF+9,C'A'                                                   
         JE    MISSFLD                                                          
         DROP  R6                                                               
*                                                                               
VR300    DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'71',(R6))                           
*                                                                               
         LA    R2,AGYRFPIH         RFP ID                                       
         CLI   5(R2),0                                                          
         BE    VR350                                                            
*                                                                               
         BAS   RE,VALRID           VALIDATE RFP ID                              
         JNE   INVLFLD                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR310                                                            
*                                                                               
         XC    KEY,KEY             IF ACTION CHANGE, THEN RESTORE               
         MVC   KEY(13),SAVEKEY     GETREC/PUTREC SEQUENCE                       
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VR310    DS    0H                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING AGYEXTEL,R6                                                      
*                                                                               
         MVC   ELEM(2),=X'7120'                                                 
         MVC   AGYLOCK,SVLKDATE    LOCKED BUY DATE                              
         MVC   AGYPRNID,WORK       ID NUMBER                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',SPTFILE),(X'71',(R6)),ELEM,0                    
*                                                                               
VR350    DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'02',(R6))                           
*                                                                               
         LA    R2,AGYMEDH          MEDIA TYPES                                  
VR360    CLI   5(R2),0                                                          
         BE    VR500                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING AGYMEDEL,R6                                                      
*                                                                               
         MVI   ELEM,X'02'                                                       
         MVI   ELEM+1,AGYMEDLQ                                                  
*                                                                               
         LA    R3,MEDTABL                                                       
         BAS   RE,TABLOOK2                                                      
*                                                                               
         MVC   AGYMEDBT,BYTE       CREATE AGY/MEDIA CODE                        
         OC    AGYMEDBT,SVAGYHEX   OR IT WITH THE SAVE AGENCY HEX               
         MVC   AGYMEDCD,8(R2)      MEDIA CODE                                   
*                                                                               
         ZIC   RF,0(R2)            BUMP TO MEDIA NAME FIELD                     
         AR    R2,RF                                                            
         CLI   5(R2),0             MUST HAVE MEDIA NAME                         
         BE    MISSFLD                                                          
*                                                                               
         MVC   AGYMEDEX,8(R2)      MEDIA NAME                                   
*                                                                               
         ZIC   RF,0(R2)            BUMP TO DESCRIPTION                          
         AR    R2,RF                                                            
         CLI   5(R2),0             MUST HAVE DESCRIPTION                        
         BE    MISSFLD                                                          
*                                                                               
         MVC   AGYVENEX,8(R2)      DESCRIPTION                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',SPTFILE),(X'02',(R6)),ELEM,0                    
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VR360                                                            
*                                                                               
VR500    DS    0H                                                               
*                                                                               
VR1000   DS    0H                  ACTION CHANGE, SO PUTREC                     
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR1010                                                           
         GOTO1 ADDREC                                                           
         B     VRX                                                              
*                                                                               
VR1010   DS    0H                  ACTION CHANGE, SO PUTREC                     
         GOTO1 PUTREC                                                           
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
*                                                                               
SPTFILE  DC    CL8'SPTFILE'                                                     
***********************************************************************         
*                                                                               
***********************************************************************         
TABLOOK1 DS    0H                                                               
TAB1A    CLI   0(R3),0             TABLE END                                    
         JE    INVLFLD                                                          
         CLC   0(1,R3),8(R2)                                                    
         BER   RE                                                               
         LA    R3,1(R3)                                                         
         B     TAB1A                                                            
         EJECT                                                                  
*                                                                               
RATSTABL DC    C'012',X'00'        RATING SERVICE                               
CLRGTABL DC    C'0RC',X'00'        CLIENT REGIONS                               
EXDMTABL DC    C'0E',X'00'         EXTENDED DEMOS                               
BOTOTABL DC    C'01',X'00'         BUY PERIOD OTO'S                             
CANTABL  DC    C'0C',X'00'         CANADIAN SPOTPAK                             
NWTATABL DC    C'0Y',X'00'         OLD POOL TIMESHEETS                          
BILLTABL DC    C'0123',X'00'       BILLING                                      
BIDTABL  DC    C'NAY',X'00'        BUY ID REQUIRED                              
*                                                                               
***********************************************************************         
* NOTE F0 ONLY FOR AGENCIES THAT START WITH 'Z' - DDS SPECIALS                  
***********************************************************************         
AGYTABL  DC    X'F010F110F220F330F440F550F660F770F880F990C1A0C2B0C3C0C4*        
               D0C5E0C6F00000'                                                  
MEDTABL  DC    X'E301D902D503E704C3080000'                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LOOK UP AGYTABL                                                               
***********************************************************************         
TABLOOK2 DS    0H                  CALLER SETS ERROR CODE                       
TAB2A    CLI   0(R3),0                                                          
         JE    INVLFLD             TABLE END                                    
         CLC   0(1,R3),8(R2)                                                    
         BE    TAB2B                                                            
         LA    R3,2(R3)                                                         
         B     TAB2A                                                            
TAB2B    MVC   BYTE,1(R3)          RETURN HEX CODE                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RFP ID                                                               
***********************************************************************         
VALRID   NTR1                                                                   
         XC    WORK,WORK                                                        
*                                                                               
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,8(R2)        ID                                           
         OC    CTIKID,SPACES                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R3)                                              
         BNE   VIDERR                                                           
*                                                                               
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
VID10    CLI   0(R3),0                                                          
         BE    VIDERR              CAN'T FIND ID NUMBER                         
         CLI   0(R3),X'02'                                                      
         BE    VID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VID10                                                            
VID20    MVC   WORK(2),2(R3)       SAVE ID NUMBER                               
         SR    RC,RC                                                            
VIDERR   LTR   RC,RC                                                            
         B     EXIT                                                             
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
*                                                                               
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         LA    R2,AGYSAGYH                                                      
         XC    AGYSAGY,AGYSAGY                                                  
         MVC   8(2,R2),AGYKAGY         AGENCY                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYSNMEH                                                      
         XC    AGYSNME,AGYSNME                                                  
         MVC   8(33,R2),AGYNAME        NAME                                     
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYSADDH                                                      
         XC    AGYSADD,AGYSADD                                                  
         MVC   8(33,R2),AGYADDR        ADDRESS                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYSIDH                                                       
         XC    AGYSID,AGYSID                                                    
         MVC   8(3,R2),AGYID           ID                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYALPHH                                                      
         XC    AGYALPH,AGYALPH                                                  
         MVC   8(2,R2),AGYPROF+17      AGENCY ALPHA                             
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYHEXH                                                       
         XC    AGYHEX,AGYHEX                                                    
         MVC   8(1,R2),AGYPROF+19      AGENCY HEX                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYSACCH                                                      
         XC    AGYSACC,AGYSACC                                                  
         MVC   8(18,R2),AGYACCT        ACCOUNT NUMBER                           
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
         LA    R2,AGYACCH              ACC AGYS                                 
         XC    AGYACC,AGYACC                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR60                                                             
         USING AGYACCEL,R6                                                      
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,AGYACCAG                                                      
         LA    R1,8                                                             
         CLI   0(R3),C' '                                                       
         BNH   DR60                                                             
         DROP  R6                                                               
*                                                                               
DR50     DS    0H                                                               
         MVC   0(2,R2),0(R3)                                                    
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNH   DR60                                                             
*                                                                               
         LA    R2,2(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,DR50                                                          
*                                                                               
DR60     DS    0H                                                               
         L     R6,AIO                                                           
         LA    R2,AGYOPTSH        AGENCY OPTIONS                                
         BRAS  RE,DISOPTS                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DR80     DS    0H                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         LA    R2,AGYRATSH         RATING SERVICE                               
         MVC   8(1,R2),AGYPROF                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYCLRGH         CLIENT REGIONS                               
         MVC   8(1,R2),AGYPROF+1                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYBLPCH         BILLING PERCENTAGE                           
         MVC   8(2,R2),AGYPROF+2                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYEXDMH         EXTENDED DEMOS                               
         MVC   8(1,R2),AGYPROF+4                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYBOTOH         BUY PERIOD OTO'S                             
         MVC   8(1,R2),AGYPROF+5                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYCANH          CANADIAN                                     
         MVC   8(1,R2),AGYPROF+7                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYTMSHH         OLD POOL SHEETS                              
         MVC   8(1,R2),AGYPROF+8                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYBILLH         BILLING                                      
         MVC   8(1,R2),AGYPROF+16                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYCBLH          CREDIT BUY LIMIT                             
         MVC   8(1,R2),AGYPROF+10                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+9,C'Y'                                                   
         BE    DR90                                                             
         CLI   AGYPROF+9,C'A'                                                   
         BE    DR90                                                             
         MVI   AGYPROF+9,C'N'                                                   
*                                                                               
DR90     DS    0H                                                               
         LA    R2,AGYBIRQH         BUY ID REQUIRED                              
         MVC   8(1,R2),AGYPROF+9                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+11,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+11,C'0'                                                  
*                                                                               
         LA    R2,AGYRPOLH         BRAND POL RADIO                              
         MVC   8(1,R2),AGYPROF+11                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYXOTOH         SPCL -OTO CODE                               
         MVC   8(1,R2),AGYPROF+15                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+12,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+12,C'N'                                                  
*                                                                               
         LA    R2,AGYMGDMH         MKGDS IN MISSED MTH                          
         MVC   8(1,R2),AGYPROF+12                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+6,C'Y'                                                   
         BE    *+8                                                              
         MVI   AGYPROF+6,C'N'                                                   
*                                                                               
         LA    R2,AGYSAUTH         -S AUTH REQUIRED                             
         MVC   8(1,R2),AGYPROF+6                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+13,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+13,C'N'                                                  
*                                                                               
         LA    R2,AGYOFRQH         OFFICE REQUIRED                              
         MVC   8(1,R2),AGYPROF+13                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   AGYPROF+14,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+14,C'N'                                                  
*                                                                               
         LA    R2,AGYBYBLH         BUYER/BILLER                                 
         MVC   8(1,R2),AGYPROF+14                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYACOCH         ACC OFFICE CODE                              
         MVC   8(1,R2),AGYOFC2                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,AGYPRECH         2 DECIMAL PRECISION                          
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         TM    AGYFLAG2,AGYFLAG2_2DP     2 DECIMAL PRECISION?                   
         BZ    *+12                                                             
         MVI   8(R2),C'Y'                                                       
         B     DR95                                                             
*                                                                               
         TM    AGYFLAG2,AGYFLAG2_BDP     USER DEFINED DEC PRECISION?            
         BZ    *+8                                                              
         MVI   8(R2),C'B'                                                       
*                                                                               
DR95     LA    R2,AGYXPRDH          EXTENDED BRANDS?                            
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         TM    AGYFLAG2,AGYFLAG2_XPR                                            
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
DR100    DS    0H                                                               
         LA    R2,AGYTTLEH         ID TITLE                                     
         XC    AGYTTLE,AGYTTLE                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR120                                                            
         USING AGYIDEL,R6                                                       
*                                                                               
         MVC   AGYTTLE,AGYTITLE                                                 
         DROP  R6                                                               
*                                                                               
DR120    DS    0H                                                               
         LA    R2,AGYRFPIH         RFP ID                                       
         XC    AGYRFPI,AGYRFPI                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'71'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR130                                                            
         USING AGYEXTEL,R6                                                      
*                                                                               
         OC    AGYPRNID,AGYPRNID                                                
         BZ    DR130                                                            
         DROP  R6                                                               
*                                                                               
         BAS   RE,DISPRID          DISPLAY ID                                   
*                                                                               
         L     R6,AIO                                                           
         XC    KEY,KEY             IF ACTION CHANGE, THEN RESTORE               
         MVC   KEY(13),0(R6)       GETREC/PUTREC SEQUENCE                       
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
DR130    DS    0H                                                               
         LA    R2,AGYMEDH                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR150                                                            
         USING AGYMEDEL,R6                                                      
         B     DR140                                                            
*                                                                               
DR135    BRAS  RE,NEXTEL                                                        
         BNE   DR150                                                            
*                                                                               
DR140    DS    0H                                                               
         XC    8(1,R2),8(R2)                                                    
         MVC   8(1,R2),AGYMEDCD        MEDIA CODE                               
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         XC    8(10,R2),8(R2)                                                   
         MVC   8(10,R2),AGYMEDEX       MEDIA NAME                               
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         XC    8(7,R2),8(R2)                                                    
         MVC   8(7,R2),AGYVENEX        DESCRIPTION                              
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR135                                                            
         DROP  R6                                                               
*                                                                               
DR150    DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* DISPLAY RFP ID                                                                
*        INPUT - R6 POINTS TO 71 ELEMENT                                        
*                R2 POINTS TO RFP ID FIELD ON SCREEN                            
*******************************************************************             
DISPRID  NTR1                                                                   
         USING AGYEXTEL,R6                                                      
*                                                                               
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,AGYPRNID                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R3)                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
DRID10   CLI   0(R3),0                                                          
         BNE   *+6                 CAN'T FIND ID NAME                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R3),X'02'                                                      
         BE    DRID20                                                           
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DRID10                                                           
*                                                                               
DRID20   MVC   8(10,R2),2(R3)      DISPLAY ID                                   
*                                                                               
DRIDX    DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
INVLDAT  MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
RECERR   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
INVLPER  MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
CHGERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CAN NOT CHANGE THIS FIELD               '         
         OI    CONHEADH+6,X'80'    XMIT                                         
         B     MYERR                                                            
*                                                                               
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                              
***********************************************************************         
VALOPTS  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         MVI   AGYFLAG1,0                                                       
         MVI   AGYFLAG2,0                                                       
         MVI   AGYFLAG3,0                                                       
         XC    AGYCTAGY,AGYCTAGY                                                
         MVI   SVOPTNUM,0                                                       
*                                                                               
         LA    R2,AGYOPTSH                                                      
         GOTO1 SCANNER,DMCB,(R2),(12,AIO3)                                      
         CLI   DMCB+4,0                                                         
         JE    INVLFLD                                                          
*                                                                               
         L     R5,AIO3                                                          
*                                                                               
VOPT20   DS    0H                                                               
         CLI   0(R5),0             ANY MORE OPTIONS?                            
         BE    VOPT1000                                                         
*                                                                               
         ZIC   RF,SVOPTNUM                                                      
         AHI   RF,1                                                             
         STC   RF,SVOPTNUM                                                      
*                                                                               
         CLC   =C'ADDS',12(R5)                                                  
         BE    VOPT100                                                          
         CLC   =C'MEDNAME',12(R5)                                               
         BE    VOPT110                                                          
         CLC   =C'CTA',12(R5)                                                   
         BE    VOPT120                                                          
         CLC   =C'CTFILE',12(R5)                                                
         BE    VOPT130                                                          
         CLC   =C'LOCK',12(R5)                                                  
         BE    VOPT140                                                          
         CLC   =C'OFF',12(R5)                                                   
         BE    VOPT150                                                          
         CLC   =C'TEST',12(R5)                                                  
         BE    VOPT160                                                          
         CLC   =C'DDSB',12(R5)                                                  
         BE    VOPT170                                                          
         CLC   =C'TRD',12(R5)                                                   
         BE    VOPT180                                                          
         CLC   =C'COS2',12(R5)                                                  
         BE    VOPT190                                                          
         CLC   =C'PW',12(R5)                                                    
         BE    VOPT200                                                          
         CLC   =C'DIY',12(R5)                                                   
         BE    VOPT210                                                          
         CLC   =C'AA',12(R5)                                                    
         BE    VOPT220                                                          
*                                                                               
         B     VOPTERR                                                          
*                                                                               
VOPT100  DS    0H                  ADDS=Y/N                                     
         L     R6,AIO                                                           
         CLI   22(R5),C'N'                                                      
         BE    VNXTOPT                                                          
         CLI   22(R5),C'Y'                                                      
         JNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYFADDQ                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT110  DS    0H                  MEDNAME=Y/N                                  
         L     R6,AIO                                                           
         CLI   22(R5),C'N'                                                      
         BE    VNXTOPT                                                          
         CLI   22(R5),C'Y'                                                      
         JNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYFMDNQ                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT120  DS    0H                  CTA=Y/N                                      
         L     R6,AIO                                                           
         CLI   22(R5),C'N'                                                      
         BE    VNXTOPT                                                          
         CLI   22(R5),C'Y'                                                      
         JNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYFCTAQ                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT130  DS    0H                  CTFILE=XX                                    
         L     R6,AIO                                                           
         CLI   1(R5),2             MUST BE 2 BYTES                              
         BNE   VOPTERR                                                          
*                                                                               
         MVC   AGYCTAGY,22(R5)                                                  
         B     VNXTOPT                                                          
*                                                                               
VOPT140  DS    0H                  LOCK=DATE                                    
         L     R6,AIO                                                           
         XC    TEMP,TEMP                                                        
         MVC   TEMP(10),22(R5)                                                  
*                                                                               
         GOTO1 DATVAL,DMCB,(0,TEMP),(0,DATEE)                                   
         GOTO1 =V(GETBROAD),DMCB,(1,DATEE),WORK,GETDAY,ADDAY                    
         CLI   0(R1),X'FF'                                                      
         BE    VOPTERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,SVLKDATE)                              
         B     VNXTOPT                                                          
*                                                                               
VOPT150  DS    0H                  OFF=HEX                                      
         L     R6,AIO                                                           
         CLC   =C'HEX',22(R5)                                                   
         BNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYFHEXQ                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT160  DS    0H                  TEST=Y/N                                     
         L     R6,AIO                                                           
         CLI   22(R5),C'N'                                                      
         BE    VNXTOPT                                                          
         CLI   22(R5),C'Y'                                                      
         JNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYTESTQ                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT170  DS    0H                  DDSB=CLT/PRD                                 
         L     R6,AIO                                                           
         CLC   =C'CLT',22(R5)                                                   
         BE    VNXTOPT                                                          
         CLC   =C'PRD',22(R5)                                                   
         BNE   VOPTERR                                                          
*                                                                               
         OI    AGYFLAG1,AGYPRDQ                                                 
         B     VNXTOPT                                                          
*                                                                               
VOPT180  DS    0H                  TRD                                          
         L     R6,AIO                                                           
         OI    AGYFLAG1,AGYTRDQ                                                 
         B     VNXTOPT                                                          
*                                                                               
VOPT190  DS    0H                  COS2                                         
         L     R6,AIO                                                           
         OI    AGYFLAG1,AGYCOS2Q                                                
         B     VNXTOPT                                                          
*                                                                               
VOPT200  DS    0H                  PW                                           
         L     R6,AIO                                                           
         OI    AGYFLAG2,AGYFLAG2_PW                                             
         B     VNXTOPT                                                          
*                                                                               
VOPT210  DS    0H                  DIY                                          
         L     R6,AIO                                                           
***NOP   OI    AGYFLAG2,AGYFLAG2_DIY     NOT AT AGENCY LEVEL ANYMORE            
         B     VNXTOPT                                                          
*                                                                               
VOPT220  DS    0H                  AA=AUTO APPROVE                              
         L     R6,AIO                                                           
         OI    AGYFLAG3,AGYFLAG3_AA                                             
         B     VNXTOPT                                                          
*                                                                               
VNXTOPT  DS    0H                                                               
         LA    R5,32(R5)                                                        
         B     VOPT20                                                           
*                                                                               
VOPT1000 DS    0H                                                               
*                                                                               
VALOPTSX DS    0H                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
VOPTERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'** ERROR ** OPTION    NOT VALID'                  
         EDIT  SVOPTNUM,(2,CONHEAD+19),ALIGN=LEFT,ZERO=NOBLANK                  
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
***********************************************************************         
* DISPLAY OPTIONS                                                               
***********************************************************************         
DISOPTS  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVI   SCRNFLAG,0          INITIALIZE THE FLAG                          
*                                                                               
         LA    R2,AGYOPTSH                                                      
         XC    8(L'AGYOPTS,R2),8(R2)                                            
*                                                                               
         LA    R4,8(R2)                                                         
         TM    AGYFLAG1,AGYFADDQ                                                
         BZ    DA20                                                             
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA20                NO - SO CONTINUE                             
*                                                                               
         MVC   0(7,R4),=C'ADDS=Y,'                                              
         LA    R4,7(R4)                                                         
*                                                                               
DA20     DS    0H                                                               
         TM    AGYFLAG1,AGYFMDNQ                                                
         BZ    DA30                                                             
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA30                NO - SO CONTINUE                             
*                                                                               
         MVC   0(10,R4),=C'MEDNAME=Y,'                                          
         LA    R4,10(R4)                                                        
*                                                                               
DA30     DS    0H                                                               
         TM    AGYFLAG1,AGYFCTAQ                                                
         BZ    DA40                                                             
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA40                NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R4),=C'CTA=Y,'                                               
         LA    R4,6(R4)                                                         
*                                                                               
DA40     DS    0H                                                               
         CLC   AGYCTAGY,=C'  '                                                  
         BNH   DA50                                                             
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA50                NO - SO CONTINUE                             
*                                                                               
         MVC   0(10,R4),=C'CTFILE=XX,'                                          
         MVC   7(2,R4),AGYCTAGY                                                 
         LA    R4,10(R4)                                                        
*                                                                               
DA50     DS    0H                                                               
         TM    AGYFLAG1,AGYFHEXQ                                                
         BZ    DA60                                                             
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA60                NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R4),=C'OFF=HEX,'                                             
         LA    R4,8(R4)                                                         
*                                                                               
DA60     DS    0H                                                               
         TM    AGYFLAG1,AGYTESTQ                                                
         BZ    DA70                                                             
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA70                NO - SO CONTINUE                             
*                                                                               
         MVC   0(7,R4),=C'TEST=Y,'                                              
         LA    R4,7(R4)                                                         
*                                                                               
DA70     DS    0H                                                               
         TM    AGYFLAG1,AGYPRDQ    TEST DDS BILLING BY PRODUCT                  
         BZ    DA80                                                             
*                                                                               
         LA    R0,9                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA80                NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R4),=C'DDSB=PRD'                                             
         MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
*                                                                               
DA80     DS    0H                                                               
         TM    AGYFLAG2,AGYFLAG2_PW                                             
         BZ    DA90                                                             
         MVC   0(3,R4),=C'PW,'                                                  
         LA    R4,3(R4)                                                         
*                                                                               
DA90     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'71'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DA100                                                            
         USING AGYEXTEL,R6                                                      
*                                                                               
         OC    AGYLOCK,AGYLOCK                                                  
         BZ    DA100                                                            
*                                                                               
         LA    R0,14               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA100               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R4),=C'LOCK='                                                
         GOTO1 DATCON,DMCB,(3,AGYLOCK),(5,5(R4))                                
         MVI   13(R4),C','                                                      
         LA    R4,14(R4)                                                        
         DROP  R6                                                               
*                                                                               
DA100    DS    0H                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
*                                                                               
         TM    AGYFLAG1,AGYTRDQ    TRADE AGENCY?                                
         BZ    DA110               NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA110               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R4),=C'TRD,'    ELSE - MOVE OUT LITERAL                      
         LA    R4,4(R4)            INC FIELD POSITION                           
*                                                                               
DA110    DS    0H                                                               
         TM    AGYFLAG1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BZ    DA120               NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA500               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R4),=C'COS2,'   ELSE - MOVE OUT LITERAL                      
         LA    R4,5(R4)            INC FIELD POSITION                           
*                                                                               
DA120    DS    0H                                                               
         TM    AGYFLAG3,AGYFLAG3_AA  AUTO APPROVE AGENCY                        
         BZ    DA500               NO - SO CONTINUE                             
*                                                                               
         LA    R0,3                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DA500               NO - SO CONTINUE                             
*                                                                               
         MVC   0(3,R4),=C'AA,'     ELSE - MOVE OUT LITERAL                      
         LA    R4,3(R4)            INC FIELD POSITION                           
*                                                                               
DA500    DS    0H                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT?                          
         BE    DA530               YES - SO CONTINUE                            
*                                                                               
         MVI   0(R1),C'*'          ELSE - MOVE OUT 'DIDN'T FIT' FLAG            
         LA    R1,1(R1)            INC A(FIELD POSITION)                        
*                                                                               
DA530    DS    0H                                                               
         BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),0             REMOVE LAST COMMA                            
*                                                                               
DISOPTSX DS    0H                                                               
         J     EXIT                                                             
         DROP  R6                                                               
*******************************************************************             
* CHECK IF THERE'S MORE ROOM ON AGENCY OPTIONS LINE                             
*******************************************************************             
CHECKOPT DS    0H                                                               
         AR    R0,R4               A(NEW FIELD POSITION)                        
         LA    R3,AGYOPTS          A(SCREEN FIELD)                              
         LA    R3,L'AGYOPTS-1(R3)  A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
         B     CKOPEXIT                                                         
*                                                                               
CKOPERR  DS    0H                                                               
         ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
*                                                                               
CKOPEXIT DS    0H                                                               
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM9DD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C09 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
*                                                                               
SAVEKEY  DS    XL13                                                             
SVSPTKEY DS    XL13                                                             
SVUNTKEY DS    XL20                                                             
SVAREC   DS    A                   A(RECORD)                                    
*                                                                               
TEMP     DS    XL20                                                             
DATEE    DS    CL6                 DATE EBCDIC                                  
*                                                                               
SVAGYHEX DS    CL1                 AGENCY HEX                                   
SVLKDATE DS    XL3                 LOCKED BUY DATE                              
SVOPTNUM DS    XL1                 OPTION LOCATION ON FIELD                     
*                                                                               
SCRNFLAG DS    XL1                                                              
BYTE2    DS    XL1                                                              
*                                                                               
NOCHGERR EQU   120                 MAY NOT CHANGE THIS FIELD                    
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148NESFM09   06/08/09'                                      
         END                                                                    
