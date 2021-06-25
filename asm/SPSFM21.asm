*          DATA SET SPSFM21    AT LEVEL 024 AS OF 05/01/02                      
*PHASE T21721A,*                                                                
         TITLE 'T21721  MARKET CORRESPONDENCE RECORDS'                          
T21721   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21721                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA ADDRESSABILITY                 
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       CLI   ACTNUM,ACTDEL                                                    
         BE    VKACTERR                                                         
         LA    R6,SVKEY            POINT TO KEY AREA                            
         USING MKCKEY,R6           ESTABLISH ADDRESSABILITY                     
         XC    MKCKEY,MKCKEY       INITIALIZE KEY                               
         MVI   MKCKRID,MKCKMKCR    SET RECORD ID                                
         MVI   MKCKSID,MKCKMKCS    SET RECORD SUBID                             
*                                                                               
         XC    WORKFLD,WORKFLD     CLEAR WORKFLD AREA                           
         LA    R2,WORKFLD          SET UP DUMMY MEDIA INPUT FIELD               
         USING FLDHDRD,R2          ESTABLISH ADDRESSABILITY                     
         MVI   FLDILEN,1           SET INPUT LENGTH TO 1                        
         MVI   FLDDATA,C'T'        SET MEDIA AS TV                              
         GOTO1 VALIMED             VAL MEDIA TO GET BAGYMD BYTE                 
         MVC   MKCKAGMD,BAGYMD     SET AGENCY/MEDIA IN KEY                      
*                                                                               
         LA    R2,MKCMKTH          POINT TO KEYED MARKET                        
         XC    BMKT,BMKT           CLEAR PRIOR VALUE                            
         CLI   ACTNUM,ACTLIST      ACTION LIST DOESN'T NEED MARKET              
         BNE   *+12                                                             
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BE    VK010               NONE                                         
         GOTO1 ANY                 REQUIRED FIELD                               
         GOTO1 VALIMKT             VALIDATE INPUT                               
         MVC   MKCKMKT,BMKT        PUT MARKET IN RECORD KEY                     
*                                                                               
         LA    R2,MKCMKTNH         POINT TO MARKET NAME FIELD                   
         FOUT  (R2),MKTNM          DISPLAY MARKET NAME                          
*                                                                               
VK010    MVC   KEY(13),MKCKEY      SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKACTERR LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         USING MKCRECD,R6          ESTABLISH ADDRESSABILITY                     
         LA    R2,MKCMKTH          POINT TO FIELD'S SCREEN AREA                 
         MVC   BMKT,MKCKMKT        SAVE MARKET NUMBER                           
         ZICM  RF,MKCKMKT,2        CONVERT MARKET NUMBER TO                     
         CVD   RF,DUB              CHARACTER                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         FOUT  (R2),QMKT           DISPLAY ON SCREEN                            
         BAS   RE,GETMKTL          READ MARKET FOR NAME                         
         LA    R2,MKCMKTNH         POINT TO MARKET NAME ON SCREEN               
         FOUT  (R2),MKTNM          DISPLAY MARKET NAME                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       MVC   SVKEY,KEY           SAVE THE MKC RECORD KEY                      
         MVI   ELCODE,MKCCID       REMOVE ALL CORRESPONDENCE ELEMENTS           
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         USING MKCCELM,R6          ESTABLISH ADDRESSABILITY                     
         MVI   MKCCELID,MKCCID     SET ELEMENT ID                               
*                                                                               
         LA    R2,MKCRMKH          POINT TO FIRST CORRESPONDING MKT             
         USING FLDHDRD,R2          ESTABLISH ADDRESSABILITY                     
*                                                                               
VRMKLOOP DS    0H                                                               
         CLI   FLDILEN,0           TEST FOR NO FIELD                            
         BE    VRMKLP10            ENTERED                                      
         TM    FLDIIND,FINPNUM     ENTRY MUST BE NUMERIC                        
         BNO   VRINVMKT                                                         
         MVI   MKCCELID,MKCCID     SET ELEMENT ID                               
         MVI   MKCCELLN,MKCCLN     SET ELEMENT LENGTH                           
         MVI   MKCCMED,C'R'        FORCE MEDIA TO RADIO                         
         ZIC   RE,FLDILEN          CONVERT MARKET NUMBER TO                     
         BCTR  RE,0                BINARY                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA         EXECUTED INSTRUCTION                         
         CVB   RE,DUB              SAVE MARKET NUMBER                           
         STCM  RE,3,MKCCMKT        IN ELEMENT                                   
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SAVE MARKET NUMBER IN QMKT                   
         BAS   RE,GETMKT           GET THE MARKET NAME IN MKTNM                 
*                                                                               
         ZIC   R0,FLDLEN           BUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,R0               WHICH IS FOR MARKET NAME                     
         FOUT  FLDLEN,MKTNM        DISPLAY MARKET NAME                          
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     VRMKLPCN                                                         
*                                                                               
VRMKLP10 DS    0H                  MARKET FIELD NOT PRESENT                     
         ZIC   R0,FLDLEN           BUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,R0               WHICH IS FOR MARKET NAME                     
         CLC   FLDDATA(24),SPACES  TEST IF MARKET NAME ON SCREEN                
         BNH   VRMKLPCN                                                         
         MVC   MKTNM,SPACES        SET MARKET NAME TO SPACES                    
         FOUT  FLDLEN,MKTNM        DISPLAY MARKET NAME                          
         B     VRMKLPCN                                                         
*                                                                               
VRMKLPCN DS    0H                  CONTINUATION OF LOOP                         
         ZIC   R0,FLDLEN           BUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,R0               WHICH IS A MARKET NUMBER                     
         LA    RF,MKCLAST          POINT TO LAST FIELD ON SCREEN                
         CR    R2,RF               TEST FOR END OF SCREEN                       
         BL    VRMKLOOP                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
VRMISERR MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VRINVMKT MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,MKCRMKH          POINT TO FIRST DETAIL FIELD                  
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         L     R6,AIO              GET THE ELEMENTS                             
         MVI   ELCODE,MKCCID       SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BE    DR030                                                            
         B     DRNOTFND                                                         
*                                                                               
DR020    BAS   RE,NEXTEL                                                        
         BNE   DR900                                                            
*                                                                               
DR030    DS    0H                                                               
         USING MKCCELM,R6          ESTABLISH ADDRESSABILITY                     
         ZICM  R3,MKCCMKT,2        GET MARKET NUMBER                            
*                                                                               
         CVD   R3,DUB              CONVERT MARKET NUMBER TO                     
         OI    DUB+7,X'0F'         CHARACTER                                    
         UNPK  QMKT,DUB                                                         
         FOUT  (R2),QMKT DISPLAY MARKET NUMBER                                  
*                                                                               
         BAS   RE,GETMKT           GET MARKET NAME IN MKTNM                     
         ZIC   R0,0(R2)            BUMP UP TO MARKET NAME FIELD                 
         AR    R2,R0                                                            
         FOUT  (R2),MKTNM          DISPLAY MARKET NAME                          
         ZIC   R0,0(R2)            POINT TO NEXT MARKET FIELD                   
         AR    R2,R0                                                            
         B     DR020                                                            
*                                                                               
DR900    B     EXIT                                                             
*                                                                               
DRNOTFND LA    R2,MKCMKTH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       MVI   NLISTS,17           SET FOR 17 ELEMENTS IN LIST                  
         LA    R2,SELSELH                                                       
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
         LA    R6,KEY                                                           
         USING MKCRECD,R6                                                       
         MVC   KEYSAVE,KEY         SAVE CURRENT KEY                             
         OC    MKCKEY,MKCKEY       TEST IF FIRST LIST SCREEN                    
         BNZ   LR010               OR PAGING THROUGH FILE                       
         XC    MKCKEY,MKCKEY       INIT KEY AREA                                
         MVI   MKCKRID,MKCKMKCR    SET RECORD ID                                
         MVI   MKCKSID,MKCKMKCS    SET RECORD SUBID                             
         MVC   MKCKAGMD,BAGYMD     SET AGENCY/MEDIA                             
         MVC   MKCKMKT,BMKT        SET FOR NEXT OR ENTERED MKT                  
         MVC   KEYSAVE,MKCKEY      SAVE KEY                                     
         MVC   AIO,AIO1                                                         
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(3),KEYSAVE      TEST FOR END OF THIS RECORD TYPE             
         BNE   LR900                                                            
         GOTO1 GETREC              GET THE MKC RECORD                           
         L     R6,AIO                                                           
         MVC   BMKT,MKCKMKT        SAVE THE MARKET                              
         ZICM  RF,BMKT,2           GET MARKET NUMBER IN CHARACTER               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LISTAR(4),DUB       AND PUT IN LIST                              
         BAS   RE,GETMKTL          READ MARKET RECORD FOR NAME                  
         MVC   LISTAR+5(24),MKTNM  ADD MARKET NAME TO LIST                      
         GOTO1 LISTMON             DISPLAY THIS LINE                            
         B     LR020               GO GET NEXT RECORD                           
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
GETMKT   NTR1                                                                   
*                                                                               
*        ROUTINE TO GET MARKET RECORD                                           
*        OUTPUT: MKTNM                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'R'          MEDIA FORCED TO RADIO                        
         MVC   KEY+2(4),QMKT       SET MARKET NUMBER                            
         MVC   KEY+6(2),AGENCY                                                  
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         CLC   MKTKEY,KEY          TEST IF MARKET FOUND                         
         BNE   VRINVMKT                                                         
         MVC   MKTNM,MKTNAME                                                    
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
GETMKTL  NTR1                                                                   
*                                                                               
*        ROUTINE TO GET KEY MARKET RECORD                                       
*        OUTPUT: MKTNM                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       SET MEDIA                                    
         UNPK  KEY+2(4),DUB        SET MARKET NUMBER                            
         MVC   KEY+6(2),AGENCY     SET AGENCY                                   
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         CLC   MKTKEY,KEY          TEST IF MARKET FOUND                         
         BNE   VRINVMKT                                                         
         MVC   MKTNM,MKTNAME                                                    
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME3D                                                       
WORKFLD  DS    XL64                WORK INPUT FIELD                             
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENMKC                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPSFM21   05/01/02'                                      
         END                                                                    
