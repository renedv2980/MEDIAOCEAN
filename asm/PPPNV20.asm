*          DATA SET PPPNV20    AT LEVEL 102 AS OF 03/01/21                      
*PHASE T41D20A                                                                  
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 01/19/21 SPEC-52537  Match buy per IDK profile on Prisma update          
*                                                                               
* KWAN 01/28/19 IAPP-210631 RECON remains YES if previously reconciled          
*                                                                               
* KWAN 08/23/18 IAPP-196734 Adjust RECON status for Prisma invoices             
*                                                                               
* KWAN 09/15/15 Do not default Prisma invoice item amount to buy rate           
*                                                                               
* KWAN 03/30/15 Prevent PrintPak from changing Prisma invoices                  
*                                                                               
*Mar04/15      KWAN                Update Prisma buy RECON status               
*                                                                               
*Nov14/14      KWAN                Prisma invoice                               
*                                                                               
*Sep15/11      KWAN                Electronic invoice                           
*                                                                               
*MAR  /10      SMYE                ALLOW FOR RETENTION AND DISPLAY OF           
*                                  "S" COST INDICATOR IN "FREE" BUYS            
*                                                                               
*OCT  /08      BOBY                ADD DISCREPANCY COMMENT CODE TO              
*                                  DETAIL                                       
*                                                                               
*APR  /08      BOBY                PREVENT DETAILS WITH DIFFERENT               
*                                  BASE PUB                                     
*                                                                               
*MAR  /06      BOBY                INVOICE HISTORY                              
*                                                                               
*DEC  /05      BOBY                DELETE INVOICE LINE ITEMS FROM               
*                                    MATCHED BUY                                
*                                                                               
*JUN  /05      BOBY                TEST FOR LOCKED CLIENT BEFORE                
*                                    UPDATING BUY                               
*                                                                               
*APR  /05      BOBY                REMOVE MATCH STATUS FROM BUY WHEN            
*                                    LAST INVOICE UNLINKED                      
*                                                                               
*APR  /05      BOBY                STORE NUMBER OF DIGITS IN RATE               
*                                                                               
*APR  /05      BOBY                PREVENT 2 LINE ITEMS FROM SAME               
*                                  INVOICE BEING LINKED TO SAME BUY             
*                                                                               
*JAN10/05      BOBY                WHEN STORING BUY INFO FOR MONTHLY            
*                                    BUY THE YEAR WAS CLEARED                   
*                                                                               
*JUN30/03      BOBY                BIG BANG                                     
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D20 - DETAIL MAINT/LIST                            *         
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41D00 (PNV CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41DFC (MAINTENANCE)                           *         
*               SCREEN T4???? (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- FILED ON SCREEN                                 *         
*               R3 -- WORK                                            *         
*               R4 -- VARIOUS RECORDS                                 *         
*               R5 -- WORK                                            *         
*               R6 -- ELEMENTS IN RECORDS                             *         
*               R7 -- MINIO SET                                       *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- BASE REGISTER                                   *         
*               RC -- GEND                                            *         
*               RD -- REGISTER CHAIN                                  *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - INIT'                      
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41D20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41D20,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         ST    RE,RELO20           SAVE RELOCATION FACTOR                       
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVI   ERROR,0             CLEAR OLD STYLE ERROR CODE                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - CKMODE'                    
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   CKMODVKX                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST         LIST SCREEN                               
         BNE   *+12                                                             
         BRAS  RE,VKL                                                           
         B     CKMODEX                                                          
*                                                                               
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
*                                                                               
CKMODVKX DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        IF PFKEY HIT                                 
         BNE   CKMDPFKX                                                         
*                                                                               
         CLI   PFAID,12            IF PF KEY 12                                 
         BE    *+8                                                              
         CLI   PFAID,24            OR 24                                        
         BNE   CKMDPFKX                                                         
*                                                                               
         OI    GENSTAT2,NEXTSEL         GO TO NEXT SELECT                       
         NI    GENSTAT2,X'FF'-RETEQSEL  NOT SAME SCEEEN                         
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDPFKX DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     CKMODE10                                                         
*                                                                               
         CLI   MODE,LISTRECS       LIST SCREEN                                  
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPREC        DISREC?                                      
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     CKMODE10                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL         RECDEL?                                      
         BNE   *+12                                                             
         BRAS  RE,DL                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECREST        RESREC?                                      
         BNE   *+12                                                             
         BRAS  RE,RS                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     CKMODEX                                                          
*                                                                               
CKMODE10 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL       IF SELECTING                                 
         BNE   *+8                                                              
         OI    GENSTAT2,RETEQSEL      RETURN TO THIS SCREEN                     
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMODEX DS     0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - VK'                        
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY FIELDS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         CLI   ACTNUM,ACTUNLNK     IF ACTION UNLINK                             
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED                     
         BNE   *+8                                                              
         BRAS  RE,DK                  DISPLAY THE KEY FIRST                     
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,DTLMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         CLI   FLDILEN,0           MEDIA IS REQUIRED                            
         BE    VKMEDER                                                          
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
         BE    *+8                                                              
         OI    CHGSWTCH,CHGMED        MEDIA HAS CHANGED                         
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,DTLCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VKCLTER                                                          
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
         BE    *+8                                                              
         OI    CHGSWTCH,CHGCLT        CLIENT HAS CHANGED                        
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,DTLPUBH          POINT TO PUB FIELD                           
*                                                                               
         CLI   FLDILEN,0           PUB IS REQUIRED                              
         BE    VKPUBER                                                          
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
         BE    *+8                                                              
         OI    CHGSWTCH,CHGPUB        PUB HAS CHANGED                           
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE INVOICE NUMBER                                                
*                                                                               
         LA    R2,DTLINVH          POINT TO INVOICE FIELD                       
*                                                                               
         CLI   FLDILEN,0           INVOICE IS REQUIRED                          
         BE    VKINVER                                                          
         CLI   FLDILEN,11          MAX LENGTH IS 11                             
         BH    VKINV1ER                                                         
*                                                                               
*        FIND INVOICE MASTER MINIO KEY                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH INVOICE PASSIVE                    
         USING PNV3KEY,R4                                                       
*                                                                               
         MVC   PNV3AGY,QAGY        SET AGENCY                                   
         MVC   PNV3MED,QMED        SET MEDIA                                    
         MVI   PNV3RCD,PNV3RCDQ    SET RECORD CODE                              
         MVC   PNV3CLT,QCLT        SET CLIENT                                   
         MVC   PNV3PBCD,QPUB       SET PUB BASE CODE                            
*                                                                               
         MVC   PNV3INV#,SPACES     PRESET TO SPACES                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNV3INV#(0),FLDDATA SET INVOICE NUMBER                           
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR KEY                          
*                                                                               
         CLC   PNV3KEY,KEYSAVE     TEST IF KEY FOUND                            
         BNE   VKINV2ER            MUST FIND KEY                                
*                                                                               
VKINVFD  DS    0H                                                               
*                                                                               
*        IS ACTION COMPATIBLE WITH DELETE/LIVE STATUS                           
*                                                                               
         TM    PNV3CNTL,PNVCDELQ   IF DELETED                                   
         BNO   VKDELN                                                           
*                                                                               
         CLI   ACTNUM,ACTDIS          ACTION MUST BE DISPLAY                    
         BNE   VKDELER                                                          
*                                                                               
VKDELN   DS    0H                                                               
*                                                                               
         MVC   QDISK,PNV3DISK      SAVE MASTER RECORD DISK ADDRESS              
*                                                                               
*        READ IN INVOICE MASTER RECORD                                          
*                                                                               
         MVC   AIO,AIO2            READ INTO IOA2                               
         GOTOR GETREC              READ IN INVOICE MASTER RECORD                
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         USING PNVKEY,R4           ESTABLISH MASTER RECORD                      
*                                                                               
         MVC   QINVKEY,PNVKEY      SAVE MASTER KEY                              
         MVC   QSER#,PNVKSER#                                                   
*                                                                               
         LA    R2,DTLSER#H                                                      
*                                                                               
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),QSER#(L'QSER#+1)  UNPACK                       
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   FLDDATA(2*L'PNVKSER#),WORK   DISPLAY SERIAL NUMBER               
         MVI   FLDILEN,2*L'PNVKSER#  SET FIELD LENGTH                           
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKAGY,QAGY        SET AGENCY                                   
         MVC   PNVKMED,QMED        SET MEDIA                                    
         MVI   PNVKRCD,PNVKRCDQ    SET RECORD CODE                              
         MVC   PNVKSER#,QSER#      SET SERIAL NUMBER                            
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND HEADER ELEMENT AND DISPLAY PERIOD                                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING PNVHKEY,R6                                                       
*                                                                               
         MVI   PNVHKCDE,PNVHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,PNVHDRD    SAVE HEADER ELEMENT                          
*                                                                               
         LA    R2,DTLPERH          POINT TO PERIOD FIELD                        
*                                                                               
         GOTOR DISPER,DMCB,PNVHSTRT   DISPLAY PERIOD                            
*                                                                               
*        VALIDATE DETAIL SEQUENCE NUMBER                                        
*                                                                               
         XC    SVDTLELM,SVDTLELM   INIT SAVEAREA                                
*                                                                               
         LA    R2,DTLDTL#H         POINT TO DETAIL SERIAL #                     
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   *+8                                                              
         MVI   FLDOPT,C'Y'            FIELD IS OPTIONAL                         
*                                                                               
         GOTOR GETFLD              READ IN THE DATA                             
*                                                                               
         LA    R6,ELEMENT          BUILD ELEMENT KEY                            
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET DETAIL ELM CODE                          
         MVI   PNVDKLEN,PNVDTLLQ   SET ELEMENT LENGTH                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        IF  SEQUENCE NUMBER ENTERED                  
         BZ    VKDTL10                                                          
*                                                                               
         TM    FLDIIND,FINPNUM         INPUT MUST BE NUMERIC                    
         BNO   VKDTL3E                                                          
*                                                                               
         BCTR  RF,0                    DECREMENT FOR EXECUTE                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)          PACK INPUT                               
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   VKDTL05                CHECK PFKEY                               
*                                                                               
VKDTLPKL DS    0H                                                               
*                                                                               
         CLI   PFAID,8             IF PFKEY 8                                   
         BE    *+8                                                              
         CLI   PFAID,20            OR PFKEY 20 HIT                              
         BNE   *+10                                                             
         AP    DUB,=P'1'              BUMP SEQUENCE NUMBER                      
*                                                                               
         CLI   PFAID,7             IF PFKEY 7                                   
         BE    *+8                                                              
         CLI   PFAID,19            OR PFKEY 19 HIT                              
         BNE   *+10                                                             
         SP    DUB,=P'1'              DECREMENT SEQUENCE NUMBER                 
*                                                                               
         CP    DUB,=P'0'           IF TOP OF LIST FOUND                         
         BE    VKDTL2E                NO MORE TO FIND                           
*                                                                               
VKDTL05  DS    0H                                                               
*                                                                               
         CVB   R0,DUB                  CVB                                      
         STCM  R0,3,PNVDKSQN           SET SEQUENCE NUMBER                      
*                                                                               
         MVI   PNVDKTYP,PNVDKDSQ   SET FOR DETAIL DESCRIPTION                   
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BNE   VKDTLNF             ELEMENT NOT FOUND                            
*                                                                               
         CLI   ACTNUM,ACTADD       IF FOUND, ACTION CAN'T BE ADD                
         BE    VKDTL1E                                                          
*                                                                               
         L     RF,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVDTLELM,0(RF)      SAVE FOUND DETAIL                            
*                                                                               
         B     VKDTL30                                                          
*                                                                               
VKDTLNF  DS    0H                  ELM NOT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTADD          OKAY IF ACTION ADD                        
         BE    VKDTL30                                                          
*                                                                               
         CLI   ACTNUM,ACTDIS       ERROR IF NOT DISPLAYING                      
         BNE   VKDTL2E                                                          
*                                                                               
         CLI   PFAID,8             ERROR IF NOT PFKEY DOWN                      
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BE    *+8                                                              
         CLI   PFAID,7             OR PFKEY UP                                  
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BNE   VKDTL2E                                                          
*                                                                               
         B     VKDTLPKL                                                         
*                                                                               
VKDTL10  DS    0H                  NO SEQ# PROVIDED                             
*                                  FIND NEXT AVAILABLE                          
         MVI   PNVDKLEN,PNVDKLEN-PNVDKEY  SET FOR COMPARE ON CODE ONLY          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,PNVDKSQN       SAVE CURRENT SEQUENCE NUMBER                 
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  FIND FIRST DETAIL                           
*                                                                               
VKDTLLP  DS    0H                                                               
*                                                                               
         BNZ   VKDTLDN             END OF DETAILS FOUND                         
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         ICM   R0,3,PNVDKSQN       SAVE FOUND SEQUENCE NUMBER                   
*                                                                               
VKDTLCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT FIND NEXT DETAIL                             
*                                                                               
         B     VKDTLLP                                                          
*                                                                               
VKDTLDN  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          RE-POINT TO ELEMENT BUILD AREA               
*                                                                               
         AHI   R0,1                BUMP SEQUENCE NUMBER BY ONE                  
         STCM  R0,3,PNVDKSQN       SET AS NEW SEQUENCE NUMBER                   
*                                                                               
VKDTL30  DS    0H                                                               
*                                                                               
         CLC   QDSQN,PNVDKSQN      IF SEQUENCE NUMBER HAS CHANGED               
         BE    *+14                                                             
         OI    CHGSWTCH,CHGDTL        INDICATE DETAIL CHANGED                   
         MVC   QDSQN,PNVDKSQN         SAVE DETAIL SEQUENCE NUMBER               
*                                                                               
*        RE-DISPLAY DETAIL SEQUENCE NUMBER                                      
*                                                                               
         EDIT  PNVDKSQN,DTLDTL#,0,ALIGN=LEFT                                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'DTLSER#   MAX OUTPUT                                   
*                                                                               
*        GENCON NEEDS A VALUE IN KEY                                            
*                                                                               
         LA    R4,PNVKEY           SET KEY AS MASTER PNV KEY                    
         USING PNVKEY,R4                                                        
*                                                                               
*        GENCON NEEDS A KEY THAT CAN BE FOUND                                   
*                                                                               
         MVC   PNVKELMK,=8X'FF'    SET FOR MASTER MINIO KEY                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   *+10                                                             
         XC    PNVKELMK,PNVKELMK      RECORD CAN'T BE FOUND                     
*                                                                               
         MVC   KEY,MINMKEY         SET MASTER PART OF KEY                       
*                                                                               
         CLC   SVVKLKEY,QINVKEY    IF INVOICE KEY HAS CHANGED                   
         BE    *+16                                                             
         XC    SVDTLELM,SVDTLELM      INIT DETAIL ELM SAVEAREA                  
         MVC   SVVKLKEY,QINVKEY       UPDATE KEY SAVEAREA                       
*                                                                               
*        IS ACTION COMPATIBLE WITH DELETE/LIVE STATUS                           
*                                                                               
         LA    R6,SVDTLELM         POINT TO FOUND ELEMENT                       
*                                                                               
         TM    PNVDSTAT,PNVDDLQ   IF DELETED                                    
         BNO   VKDLN                                                            
*                                                                               
         CLI   ACTNUM,ACTDIS          ACTION MUST BE DISPLAY                    
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL          OR SELECT                                 
         BE    *+8                                                              
         CLI   ACTNUM,ACTREST         OR RESTORE                                
         BNE   VKDELER                                                          
*                                                                               
         B     VKDACT1                                                          
*                                                                               
VKDLN    DS    0H                  RECORD FOUND AND NOT DELETED                 
*                                                                               
         CLI   ACTNUM,ACTREST      ACTION CANNOT BE RESTORE                     
         BE    VKDEL1ER                                                         
*                                                                               
VKDACT1  DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTUNLNK     IF ACTION UNLINK                             
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED                     
         BNE   *+8                                                              
         BRAS  RE,DR                  DISPLAY RECORD                            
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - VKERR '                
***********************************************************************         
*                                                                     *         
*        VALKEY ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKMEDER  LHI   RF,PPEFLDNE        MEDIA REQUIRED                                
         J     VKERR                                                            
*                                                                               
VKCLTER  LHI   RF,PPEFLDNE        CLIENT REQUIRED                               
         J     VKERR                                                            
*                                                                               
VKPUBER  LHI   RF,PPEFLDNE        PUB    REQUIRED                               
         J     VKERR                                                            
*                                                                               
VKINVER  LHI   RF,PPEFLDNE        INVOICE  REQUIRED                             
         J     VKERR                                                            
*                                                                               
VKINV1ER LHI   RF,PPEINVBG        INVOICE  NUMBER TOO LARGE                     
         J     VKERR                                                            
*                                                                               
VKINV2ER LHI   RF,PPEINVNF        INVOICE  NOT ON FILE                          
         J     VKERR                                                            
*                                                                               
VKDELER  LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR                                                            
*                                                                               
VKDEL1ER LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR1                                                           
*                                                                               
*        DETAIL ERROR MESSAGES TO BE FORMULATED                                 
*                                                                               
VKDTL1E  LHI   RF,PPEDTLFD         RECORD ALREADY ON FILE                       
         J     VKERR1                                                           
*                                                                               
VKDTL2E  LHI   RF,PPEDTLNF         INVOICE LINE ITEM NOT ON FILE                
         J     VKERR1                                                           
*                                                                               
VKDTL3E  LHI   RF,PPEDTLNF         INVOICE LINE ITEM MUST BE NUMERIC            
         J     VKERR1                                                           
*                                                                               
VKERR    DS    0H                  INVOICE RECORD DOES NOT EXIST                
*                                    CLEAR SERIAL # AND PERIOD                  
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,DTLSER#H         POINT TO SERIAL NUMBER FIELD                 
         BRAS  RE,CLRFLD           CLEAR INVOICE SERIAL NUMBER FIELD            
*                                                                               
         LA    R2,DTLPERH          POINT TO PERIOD FIELD                        
         BRAS  RE,CLRFLD           CLEAR INVOICE PERIOD FIELD                   
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VKERR1   DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - VKL'                       
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY FIELDS - LIST SCREEN                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
*                                                                               
*        SET NUMBER OF LINES AVAILABLE FOR LIST                                 
*                                                                               
         MVI   NLISTS,(LSTLINLH-LSTLIN1H)/(LSTLIN2H-LSTLIN1H)+1                 
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,LSTMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         CLI   FLDILEN,0           MEDIA IS REQUIRED                            
         BE    VKLMEDER                                                         
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,LSTCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VKLCLTER                                                         
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,LSTPUBH          POINT TO PUB FIELD                           
*                                                                               
         CLI   FLDILEN,0           PUB IS REQUIRED                              
         BE    VKLPUBER                                                         
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE INVOICE NUMBER                                                
*                                                                               
         LA    R2,LSTINVH          POINT TO INVOICE FIELD                       
*                                                                               
         CLI   FLDILEN,0           INVOICE IS REQUIRED                          
         BE    VKLIVER                                                          
         CLI   FLDILEN,11          MAX LENGTH IS 11                             
         BH    VKLIV1ER                                                         
*                                                                               
*        FIND INVOICE MASTER MINIO KEY                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH INVOICE PASSIVE                    
         USING PNV3KEY,R4                                                       
*                                                                               
         MVC   PNV3AGY,QAGY        SET AGENCY                                   
         MVC   PNV3MED,QMED        SET MEDIA                                    
         MVI   PNV3RCD,PNV3RCDQ    SET RECORD CODE                              
         MVC   PNV3CLT,QCLT        SET CLIENT                                   
         MVC   PNV3PBCD,QPUB       SET PUB BASE CODE                            
*                                                                               
         MVC   PNV3INV#,SPACES     PRESET TO SPACES                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNV3INV#(0),FLDDATA SET INVOICE NUMBER                           
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR KEY                          
*                                                                               
         CLC   PNV3KEY,KEYSAVE     TEST IF KEY FOUND                            
         BNE   VKLIV2ER            MUST FIND KEY                                
*                                                                               
VKLIVFD DS     0H                                                               
*                                                                               
*        READ IN INVOICE MASTER RECORD                                          
*                                                                               
         MVC   AIO,AIO2            READ INTO IOA2                               
         GOTOR GETREC              READ IN INVOICE MASTER RECORD                
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         USING PNVKEY,R4           ESTABLISH MASTER RECORD                      
*                                                                               
         MVC   QINVKEY,PNVKEY      SAVE MASTER KEY                              
         MVC   QSER#,PNVKSER#                                                   
*                                                                               
         LA    R2,LSTSER#H                                                      
*                                                                               
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),QSER#(L'QSER#+1)  UNPACK                       
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   FLDDATA(2*L'PNVKSER#),WORK  DISPLAY SERIAL NUMBER                
         MVI   FLDILEN,2*L'PNVKSER# SET FIELD LENGTH                            
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKAGY,QAGY        SET AGENCY                                   
         MVC   PNVKMED,QMED        SET MEDIA                                    
         MVI   PNVKRCD,PNVKRCDQ    SET RECORD CODE                              
         MVC   PNVKSER#,QSER#      SET SERIAL NUMBER                            
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND HEADER ELEMENT AND DISPLAY PERIOD                                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING PNVHKEY,R6                                                       
*                                                                               
         MVI   PNVHKCDE,PNVHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,PNVHDRD    SAVE HEADER ELEMENT                          
*                                                                               
         LA    R2,LSTPERH          POINT TO PERIOD FIELD                        
*                                                                               
         GOTOR DISPER,DMCB,PNVHSTRT   DISPLAY PERIOD                            
*                                                                               
*        VALIDATE RUN PERIOD                                                    
*                                                                               
VKRPER   DS    0H                                                               
*                                                                               
         XC    BSTART,BSTART       CLEAR BINARY START                           
         XC    BEND,BEND           CLEAR BINARY END                             
*                                                                               
         LA    R2,LSTRPERH         POINT TO RUN PERIOD                          
*                                                                               
         GOTOR VALPER              VALIDATE AS A PERIOD                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR RUN PERIOD FIELD                       
*                                                                               
         OC    BSTART,BSTART       SKIP IF NO PERIOD FILTER                     
         BZ    VKRPERX                                                          
*                                                                               
         GOTOR DISPER,DMCB,BSTART     DISPLAY PERIOD                            
*                                                                               
VKRPERX  DS    0H                                                               
*                                                                               
         CLC   SVVKLKEY,QINVKEY    IF INVOICE KEY HAS CHANGED                   
         BE    *+16                                                             
         XC    SVDTLELM,SVDTLELM      INIT DETAIL ELM SAVEAREA                  
         MVC   SVVKLKEY,QINVKEY       UPDATE KEY SAVEAREA                       
*                                                                               
VKLX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - VKLERR '               
***********************************************************************         
*                                                                     *         
*        VALKEY ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLMEDER LHI   RF,PPEFLDNE        MEDIA REQUIRED                                
         J     VKLERR                                                           
*                                                                               
VKLCLTER LHI   RF,PPEFLDNE        CLIENT REQUIRED                               
         J     VKLERR                                                           
*                                                                               
VKLPUBER LHI   RF,PPEFLDNE        PUB    REQUIRED                               
         J     VKLERR                                                           
*                                                                               
VKLIVER LHI    RF,PPEFLDNE        INVOICE  REQUIRED                             
         J     VKLERR                                                           
*                                                                               
VKLIV1ER LHI   RF,PPEINVBG        INVOICE  NUMBER TOO LARGE                     
         J     VKLERR                                                           
*                                                                               
VKLIV2ER LHI   RF,PPEINVNF        INVOICE  NOT ON FILE                          
         J     VKLERR                                                           
*                                                                               
VKLDELER LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKLERR                                                           
*                                                                               
VKLDEL1ER LHI  RF,PPERECDL        RECORD IS DELETED                             
         J     VKLERR1                                                          
*                                                                               
*        DETAIL ERROR MESSAGES TO BE FORMULATED                                 
*                                                                               
VKLDTL1E LHI   RF,PPEDTLFD         RECORD ALREADY ON FILE                       
         J     VKLERR1                                                          
*                                                                               
VKLDTL2E LHI   RF,PPEDTLNF         INVOICE LINE ITEM NOT ON FILE                
         J     VKLERR1                                                          
*                                                                               
VKLDTL3E LHI   RF,PPEDTLNF         INVOICE LINE ITEM MUST BE NUMERIC            
         J     VKLERR1                                                          
*                                                                               
VKLERR   DS    0H                  INVOICE RECORD DOES NOT EXIST                
*                                    CLEAR SERIAL # AND PERIOD                  
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,LSTSER#H         POINT TO SERIAL NUMBER FIELD                 
         BRAS  RE,CLRFLD           CLEAR INVOICE SERIAL NUMBER FIELD            
*                                                                               
         LA    R2,LSTPERH          POINT TO PERIOD FIELD                        
         BRAS  RE,CLRFLD           CLEAR INVOICE PERIOD FIELD                   
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VKLERR1  DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - CLRFLD'                
***********************************************************************         
*                                                                     *         
*        CLEARS A FIELD ON SCREEN AND FORCES RE-TRANSMITTAL           *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    FIELD CLEARED TO NULLS                                       *         
*        FIELD SET TO BE RE-TRANSMITTED                               *         
*        OUTPUT DATA LENGTH SET TO MAXIMUM                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRFLD   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
CLRFLDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - SETLEN'                
***********************************************************************         
*                                                                     *         
*        SETS TRUE INPUT LENGTH                                       *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    LENGTH OF ACTUAL LENGTH SET IN FIELD HEADER                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETLEN   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         LA    R1,FLDDATA-1(RF)    POINT TO LAST BYTE OF INPUT                  
*                                                                               
SETLENLP DS    0H                  FIND LAST NON-BLANK IN FIELD                 
*                                                                               
         CLI   0(R1),C' '          DONE IF NOT BLANK                            
         BH    SETLENDN                                                         
*                                                                               
SETLENCN DS    0H                                                               
*                                                                               
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   RF,SETLENLP                                                      
*                                                                               
SETLENDN DS    0H                                                               
*                                                                               
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
SETLENX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV20 - PRINT NEW INVOICE DETAILS - BUMP'                     
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUMP TO NEXT FIELD ON SCREEN                      *         
*                                                                     *         
*              BUMP -  NEXT FIELD                                     *         
*              BUMPU - NEXT UNPROTECTED FIELD                         *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - NOT END OF SCREEN                                *         
*              EQ  - END OF SCREEN                                    *         
*                                                                     *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT FIELD                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          GET LENGTH OF TWA FIELD                      
         AR    R2,RF               POINT TO NEXT FIELD                          
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
*        THIS VERSION BUMPS TO NEXT UNPROTECTED FIELD                           
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         JNZ   BUMPU                  GO TO NEXT FIELD                          
*                                                                               
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - LR'                        
***********************************************************************         
*                                                                     *         
*        BUILD LIST OF INVOICE DETAILS                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
*                                                                               
         OI    GLSTSTAT,RETEXTRA   1 MORE LINE THAN SCREEN HOLDS                
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         MVC   ELEMENT,SVDTLELM    INIT ELEMENT WORKAREA                        
*                                                                               
         LA    R6,ELEMENT                                                       
         USING PNVDTLD,R6          ESTABLISH DETAIL ELEMENT                     
*                                                                               
*        READ IN DETAIL ELEMENT                                                 
*                                                                               
         CLI   LRLASTSW,C'Y'       IF END OF LIST LAST TIME                     
         BNE   *+14                                                             
         MVI   LRLASTSW,0             CLEAR SWITCH                              
         XC    ELEMENT,ELEMENT        CLEAR LAST KEY                            
*                                                                               
         OC    PNVDKEY,PNVDKEY     SKIP IF PRIOR KEY KNOWN                      
         BNZ   LRKEY10                                                          
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET ELEMENT ID                               
         XC    PNVDKSQN,PNVDKSQN   SET FOR FIRST DETAIL                         
         MVI   PNVDKTYP,PNVDKDSQ   SET ELEMENT TYPE                             
*                                                                               
LRKEY10  DS    0H                                                               
*                                                                               
         MVI   PNVDKLEN,PNVDKSQN-PNVDKEY  MATCH ON DETAIL ID                    
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
         LA    R3,LISTAR           ESTABLISH LIST LINE                          
         USING LISTLIND,R3                                                      
*                                                                               
         LA    R5,SVLSTDTL         POINT TO DETAIL KEYS                         
         XC    SVLSTDTL(SVLSTLNQ),SVLSTDTL CLEAR TABLE                          
*                                                                               
LRKEYLP  DS    0H                                                               
*                                                                               
         XC    LISTLIN,LISTLIN     INIT PRINT AREA                              
*                                                                               
         L     R6,MINELEM          POINT TO FOUND RECORD                        
         MVC   SVDTLELM,0(R6)      SAVE FOUND ELEMENT                           
*                                                                               
         OC    PNVDKEY,PNVDKEY     DONE IF NO ELEMENT FOUND                     
         BNZ   *+12                                                             
         MVI   LRLASTSW,C'Y'       INDICATE END OF DETAILS                      
         B     LRKEYDN                                                          
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   LOOKING FOR DETAIL DESCRIPTION               
         BNE   LRKEYCN                                                          
*                                                                               
*        FILTER ON RUN DATE                                                     
*                                                                               
LRRPER   DS    0H                                                               
*                                                                               
         CLC   PNVDDTE,BSTART      MUST BE AFTER PERIOD START                   
         BL    LRKEYCN                                                          
*                                                                               
         OC    BEND,BEND           IF PERIOD GIVEN                              
         BZ    LRRPERX                                                          
*                                                                               
         CLC   PNVDDTE,BEND           MUST BE BEFORE PERIOD END                 
         BH    LRKEYCN                                                          
*                                                                               
LRRPERX  DS    0H                                                               
*                                                                               
*        DISPLAY DETAIL SEQUENCE NUMBER                                         
*                                                                               
         MVI   LSSTAT,C' '                                                      
*                                                                               
         TM    PNVDSTAT,PNVDDLQ    FLAG IF DELETED                              
         BNO   *+8                                                              
         MVI   LSSTAT,C'D'                                                      
*                                                                               
         TM    PNVDSTAT,PNVDMTQ+PNVDDLQ FLAG IF DELETED & MATCHED               
         BNO   *+8                                                              
         MVI   LSSTAT,C'C'                                                      
*                                                                               
         EDIT  PNVDKSQN,LSDTL#,0,ALIGN=LEFT                                     
*                                                                               
*        DISPLAY RUN DATE                                                       
*                                                                               
LRDTE    DS    0H                                                               
*                                                                               
         OC    PNVDDTE,PNVDDTE     SKIP IF NO DATE GIVEN                        
         BZ    LRDTEX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PNVDDTE),(17,LSRUNDT)  DISP DATE                  
*                                                                               
LRDTEX   DS    0H                                                               
*                                                                               
*        DISPLAY SPACE ENTRY                                                    
*                                                                               
LRSPC    DS    0H                                                               
*                                                                               
         OC    PNVDSPC,PNVDSPC     SKIP IF NO SPACE GIVEN                       
         BZ    LRSPCX                                                           
*                                                                               
         MVC   LSSPACE(L'PNVDSPC),PNVDSPC DISPLAY SPACE                         
*                                                                               
LRSPCX   DS    0H                                                               
*                                                                               
*        DISPLAY AD CAPTION                                                     
*                                                                               
LRACAP   DS    0H                                                               
*                                                                               
         OC    PNVDACAP,PNVDACAP   SKIP IF NO AD CAPTION                        
         BZ    LRACAPX                                                          
*                                                                               
         MVC   LSCAP1(L'PNVDACAP),PNVDACAP DISPLAY AD CAPTION                   
*                                                                               
LRACAPX  DS    0H                                                               
*                                                                               
LRSER#   DS    0H                                                               
*                                                                               
         XC    LSSER#,LSSER#       INIT DISPLAY AREA                            
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF NO BUY SERIAL #                      
         BZ    LRSER#X                                                          
*                                                                               
         EDIT  PNVDSER#,LSSER#,0,ALIGN=LEFT                                     
*                                                                               
LRSER#X  DS    0H                                                               
*                                                                               
         MVC   0(L'SVLSTDTL,R5),PNVDKEY SAVE DETAIL'S KEY                       
         LA    R5,L'SVLSTDTL(R5)    BUMP TO NEXT ENTRY IN TABLE                 
*                                                                               
         GOTOR LISTMON             PASS BACK TO GENCON                          
*                                                                               
LRKEYCN  DS    0H                                                               
*                                                                               
         GOTO1 NXTELM,DMCB,ELEMENT    FIND NEXT  ELEMENT                        
*                                                                               
         B     LRKEYLP                                                          
*                                                                               
LRKEYDN  DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - VR'                        
***********************************************************************         
*                                                                     *         
*        VALIDATE INVOICE DETAIL FIELDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
*        IF ACTION ABDELETE                                                     
*              DELETE DETAIL                                                    
*                                                                               
         GOTOR TSTLOK              MAKE SURE CLIENT IS NOT LOCKED               
         BNE   VRBYLKER            CLIENT LOCKED                                
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED                     
         BNE   *+12                                                             
         BRAS  RE,DL                  DELETE LINE ITEM                          
         B     VRX                    ALL DONE                                  
*                                                                               
*        IF ACTION UNLINK                                                       
*              CLEAR BUY SERIAL NUMBER                                          
*              CLEAR BUYLINE NUMBER                                             
*                                                                               
         CLI   ACTNUM,ACTUNLNK     IF ACTION UNLINK                             
         BNE   VRUNLNKX                                                         
*                                                                               
         XC    DTLBS#,DTLBS#          CLEAR BUY SERIAL #                        
         MVI   DTLBS#H+5,0         SET INPUT LENGTH TO 0                        
*                                                                               
         XC    DTLLIN,DTLLIN          CLEAR BUY LINE NUMBER                     
         MVI   DTLLINH+5,0         SET INPUT LENGTH TO 0                        
*                                                                               
VRUNLNKX DS    0H                                                               
*                                                                               
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
*                                                                               
*        READ IN DETAIL ELEMENT                                                 
*                                                                               
         USING PNVDTLD,ELEMENT     ESTABLISH DETAIL ELEMENT KEY                 
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET ELEMENT ID                               
         MVC   PNVDKSQN,QDSQN      SET SEQUENCE NUMBER                          
         MVI   PNVDKTYP,PNVDKDSQ   SET ELEMENT TYPE                             
*                                                                               
         OC    PNVDKSQN,PNVDKSQN   SKIP IF UNKNOWN SEQ #                        
         BZ    VRGETX                                                           
*                                                                               
         GOTO1 GETELM,DMCB,PNVDKEY FIND ELEMENT                                 
         BZ    VRGET10             ELEMENT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTADD       OKAY IF ADDING                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PNVDKLEN,PNVDTLLQ   SET LENGTH FOR NEW ELM                       
*                                                                               
         B     VRGETX                                                           
*                                                                               
VRGET10  DS    0H                                                               
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVDTLD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVDTLOLD(0),0(R1)   SAVE FOR ACTIVITY ANALYSIS                   
*                                                                               
         CLI   SVINVSRC,INVPRM_Q    Prisma invoice?                             
         JE    VRGETX                                                           
         CLI   SVINVSRC,INVRAD_Q    Radia invoice?                              
         JE    VRGETX                                                           
         CLI   PNVDIVSR,INVPRM_Q    Prisma invoice?                             
         JE    *+12                                                             
         CLI   PNVDIVSR,INVRAD_Q    Radia invoice?                              
         JNE   VRGETX                                                           
         LHI   RF,PPPRSMER                                                      
         J     VRERR                                                            
*                                                                               
VRGETX   DS    0H                                                               
*                                                                               
         CLI   SVINVSRC,C' '        HAVE INVOICE SOURCE?                        
         JNH   *+10                                                             
         MVC   PNVDIVSR,SVINVSRC    STAMP IT IN RECORD                          
*                                                                               
*        VALIDATE RUN DATE - ANY VALID DATE                                     
*                                                                               
         LA    R2,DTLDTEH          POINT TO LINE ITEM DATE                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         CLI   FLDILEN,0           LINE ITEM RUN DATE MUST BE GIVEN             
         JE    VRDTENEE                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
*        SINGLE DATE ONLY                                                       
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),('PVINTOD+PVINSGLO',(R4))              
*                                                                               
         TM    4(R1),PVRCONE       SINGLE DATE OKAY                             
         BO    *+12                                                             
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VRDTENVE                                                         
*                                                                               
         MVC   PNVDDTE,PVALBSTA    SAVE BINARY FORMAT                           
*                                                                               
         TM    PVALASSM,PVALASD    IF START DAY ASSUMED                         
         BNO   *+8                                                              
         MVI   PNVDDTE+2,0            ASSUME MONTHLY BUY                        
*                                                                               
*        VALIDATE SPACE ENTRY - ACCEPT ANYTHINF FOR NOW                         
*                                                                               
         LA    R2,DTLSPCH          POINT TO SPACE FIELD                         
*                                                                               
         CLI   FLDILEN,0           IF SPACE ENTERED                             
         BE    *+10                                                             
         MVC   PNVDSPC,FLDDATA        COPY IT                                   
*                                                                               
*        SAVE AD CAPTION                                                        
*                                                                               
         LA    R2,DTLCAP1H         POINT TO FIRST CAPTION FIELD                 
         MVC   PNVDACAP,FLDDATA    COPY                                         
         LA    R2,DTLCAP2H         POINT TO SECOND CAPTION FIELD                
         MVC   PNVDACP2,FLDDATA    SOPY                                         
*                                                                               
*        VALIDATE NUMBER OF COLORS                                              
*                                                                               
         LA    R2,DTLCLRH          NEXT COLOURS FIELD                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET FIELD LENGTH                             
         BZ    VRCLRX              SKIP IF NOT ENTERED                          
*                                                                               
         TM    FLDIIND,FINPNUM     MUST BE NUMERIC                              
         JNO   VRCLRNNE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK NUMBER                                  
*                                                                               
         CVB   RF,DUB              CVB                                          
         STC   RF,PNVDNCL          SAVE NUMBER OF COLORS                        
*                                                                               
VRCLRX   DS    0H                                                               
*                                                                               
*        VALIDATE NUMBER OF LINES                                               
*                                                                               
         LA    R2,DTLLNSH          NEXT LINES FIELD                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET FIELD LENGTH                             
         BZ    VRLNSX              SKIP IF NOT ENTERED                          
*                                                                               
         TM    FLDIIND,FINPNUM     MUST BE NUMERIC                              
         JNO   VRLNSNNE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK NUMBER                                  
*                                                                               
         CVB   RF,DUB              CVB                                          
         STC   RF,PNVD#LIN         SAVE NUMBER OF LINES                         
*                                                                               
VRLNSX   DS    0H                                                               
*                                                                               
*        VALIDATE REP                                                           
*                                                                               
VRREP    DS    0H                                                               
*                                                                               
         MVC   SVREP,QREP          SAVE CURRENT MASTER REP                      
*                                                                               
         LA    R2,DTLREPH          POINT TO REP FIELD                           
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         GOTOR VALREP                                                           
*                                                                               
         MVC   PNVDSREP,QREP       SAVE FOUND REP                               
*                                                                               
         MVC   QREP,SVREP          RETORE CURRENT MASTER REP                    
*                                                                               
VRREPX   DS    0H                                                               
*                                                                               
*        VALIDATE CD                                                            
*                                                                               
VRCD     DS    0H                                                               
*                                                                               
         MVI   PNVDCDST,0          DEFAULT CD STATUS TO NULLS                   
*                                                                               
         LA    R2,DTLCDH           POINT TO CD FIELD                            
*                                                                               
         CLI   FLDILEN,0           SKIP IF NO DATA ENTERED                      
         BE    VRCDX                                                            
*                                                                               
         CLI   FLDDATA,C'N'        DONE IF C'N'                                 
         BE    VRCDX                                                            
*                                                                               
         CLI   FLDDATA,C'Y'        SAVE IF C'Y' ENTERED                         
         BNE   *+10                                                             
         MVC   PNVDCDST,FLDDATA                                                 
*                                                                               
VRCDX    DS    0H                                                               
*                                                                               
*        VALIDATE GST                                                           
*                                                                               
VRGST    DS    0H                                                               
*                                                                               
         MVI   PNVDGST,0           DEFAULT GST TO NULLS                         
*                                                                               
         LA    R2,DTLGSTH          POINT TO GST FIELD                           
*                                                                               
         CLI   FLDILEN,0           SKIP IF NO DATA ENTERED                      
         BE    VRGSTX                                                           
*                                                                               
         MVC   PNVDGST,FLDDATA     SAVE GST                                     
*                                                                               
VRGSTX   DS    0H                                                               
*                                                                               
*        VALIDATE PST                                                           
*                                                                               
VRPST    DS    0H                                                               
*                                                                               
         MVI   PNVDPST,0           DEFAULT PST TO NULLS                         
*                                                                               
         LA    R2,DTLPSTH          POINT TO PST FIELD                           
*                                                                               
         CLI   FLDILEN,0           SKIP IF NO DATA ENTERED                      
         BE    VRPSTX                                                           
*                                                                               
         MVC   PNVDPST,FLDDATA     SAVE PST                                     
*                                                                               
VRPSTX   DS    0H                                                               
*                                                                               
*        VALIDATE RATE                                                          
*                                                                               
VRRATE   DS    0H                                                               
*                                                                               
         XC    PNVDRATE,PNVDRATE   INIT RATE                                    
         LA    R2,DTLRTEH          POINT TO RATE FIELD                          
*                                                                               
         BRAS  RE,EDTRTE           EDIT RATE LIKE THE BUY DOES                  
*                                                                               
VRRATEX  DS    0H                                                               
*                                                                               
*        VALIDATE PREMIUM                                                       
*                                                                               
VRPREM   DS    0H                                                               
*                                                                               
         XC    PNVDPREM,PNVDPREM   INIT PREMIUM                                 
         LA    R2,DTLPREMH         POINT TO PREMIUM FIELD                       
*                                                                               
         BRAS  RE,EDTPREM          EDIT PREMIUM INPUT                           
*                                                                               
VRPREMX  DS    0H                                                               
*                                                                               
*        VALIDATE GROSS                                                         
*                                                                               
VRGRS    DS    0H                                                               
*                                                                               
         XC    PNVDGRS,PNVDGRS     INIT GROSS                                   
         LA    R2,DTLGRSH          POINT TO GROSS FIELD                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET LENGTH OF FIELD                          
         BZ    VRGRSX              SKIP IF GROSS NOT ENTERED                    
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',FLDDATA),(0,(RF)),0,0,0,0  VALIDATE          
*                                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         JNE   VRGRSNVE                                                         
*                                                                               
         ZAP   PNVDGRS,4(8,R1)       SAVE  GROSS                                
*                                                                               
VRGRSX   DS    0H                                                               
*                                                                               
*        VALIDATE NET                                                           
*                                                                               
VRNET    DS    0H                                                               
*                                                                               
         XC    PNVDNET,PNVDNET     INIT NET                                     
         LA    R2,DTLNETH          POINT TO NET FIELD                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET LENGTH OF FIELD                          
         BZ    VRNETX              SKIP IF NET NOT ENTERED                      
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',FLDDATA),(0,(RF)),0,0,0,0  VALIDATE          
*                                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         JNE   VRNETNVE                                                         
*                                                                               
         ZAP   PNVDNET,4(8,R1)       SAVE  NET                                  
*                                                                               
VRNETX   DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM CLIENT                                              
*                                                                               
VRDCL    DS    0H                                                               
*                                                                               
         MVC   SVDCL,PNVDCLT      SAVE CURRENT LINE ITEM CLIENT                 
         XC    PNVDCLT,PNVDCLT    INIT DETAIL CLIENT                            
*                                                                               
         LA    R2,DTLDCLH          POINT TO DETAIL CLIENT                       
*                                                                               
         CLI   FLDILEN,0           OKAY IF NOT ENTERED                          
         BE    VRDCLX                                                           
*                                                                               
         MVC   SVCLT,QCLT          SAVE CURRENT MASTER CLIENT                   
         MVC   SVCLTNM,CLTNM       SAVE CURRENT MASTER CLT NAME                 
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT CODE                         
*                                                                               
         MVC   PNVDCLT,QCLT        SAVE DETAIL CLIENT CODE                      
*                                                                               
         MVC   QCLT,SVCLT          RESTORE CLIENT CODE                          
         MVC   CLTNM,SVCLTNM       RESTORE CLIENT NAME                          
*                                                                               
         CLC   PNVDCLT,=C'***'     CLIENT VARIOUS NOT ALLOWED                   
         BE    VRDCL1E                                                          
*                                                                               
VRDCLX   DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM BUY SERIAL NUMBER                                   
*                                                                               
VRSER#   DS    0H                                                               
*                                                                               
         MVC   SVBSR#,PNVDSER#     SAVE CURRENT BUY SERIAL NUMBER               
         XC    PNVDSER#,PNVDSER#   INIT BUY SERIAL NUMBER                       
*                                                                               
         LA    R2,DTLBS#H          POINT TO SERIAL NUMBER                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BE    VRSER#X             OKAY IF NOT ENTERED                          
*                                                                               
         TM    FLDIIND,FINPNUM     SERIAL NUMBER MUST BE NUMERIC                
         BNO   VRSER#1E                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  PNVDSER#,FLDDATA(0) PACK SERIAL NUMBER                           
*                                                                               
*        READ BUY RECORD                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH BUY SER# PASSIVE                   
         USING PSERKEY,R4                                                       
*                                                                               
         MVC   PSERKAGY,QAGY       SET AGENCY                                   
         MVC   PSERKMED,QMED       SET MEDIA                                    
         MVI   PSERKRCD,PSERKIDQ   SET RECORD CODE                              
         MVC   PSERKCLT,PNVDCLT    SET CLIENT                                   
*                                                                               
         OC    PSERKCLT,PSERKCLT   IF CLIENT MISSING                            
         BNZ   *+10                                                             
         MVC   PSERKCLT,QCLT          USE MASTER CLIENT                         
*                                                                               
         CLC   =C'***',PSERKCLT    CLIENT CAN'T BE VARIOUS                      
         BE    VRSER#2E                                                         
*                                                                               
         ZAP   DUB,=P'1000000000'  COMPLEMENT SER#                              
         SP    DUB,PNVDSER#                                                     
         ZAP   PSERKNUM,DUB        SET SERIAL NUMBER                            
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     MUST FIND IT                                 
         BNE   VRSER#3E                                                         
*                                                                               
         MVC   SVPSERKY,PSERKEY    SAVE BUY SERIAL PASSIVE                      
*                                                                               
*        DETERMINE IF THIS BUY IS ALREADY LINKED TO ANOTHER                     
*              LINE ITEM OF THIS INVOICE                                        
*              NOT ALLOWED                                                      
*                                                                               
         MVC   AIO,AIO1            READ BUY REC INTO AIO1                       
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,PBNVELQ      SET TO FIND INVOICE ELEMENT                  
*                                                                               
         BRAS  RE,GETEL            FIND INVOICE ELEMENT                         
*                                                                               
VRSER#LP DS    0H                                                               
*                                                                               
         BNE   VRSER#DN            NONE FOUND                                   
*                                                                               
         USING PBNVELMD,R6         ESTABLISH INVOICE ELEMENT                    
*                                                                               
         CLC   QSER#,PBNVSER#      MATCH ON THIS INVOICE SER#                   
         BNE   VRSER#CN            OKAY IF NO MATCH                             
*                                                                               
         CLI   ACTNUM,ACTADD       ERROR IF ADDING                              
         BE    VRSER#4E            NO DUPLICATE INVOICE ALLOWED                 
*                                                                               
         CLC   PBNVDSQN,PNVDKSQN   ERROR IF NOT SAME DETAIL SQN                 
         BNE   VRSER#4E                                                         
*                                                                               
VRSER#CN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     VRSER#LP                                                         
*                                                                               
VRSER#DN DS    0H                  NO DUPLICATES FOUND                          
*                                                                               
VRSER#X  DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM PRODUCT                                             
*                                                                               
VRDPR    DS    0H                                                               
*                                                                               
         XC    PNVDPRD,PNVDPRD     INIT DETAIL PRODUCT                          
*                                                                               
         LA    R2,DTLDPRH          POINT TO DETAIL PRODUCT                      
*                                                                               
         MVI   FLDOPT,C'Y'         FIELD IS OPTIONAL                            
*                                                                               
         MVC   SVCLT,QCLT          SAVE CURRENT MASTER CLIENT                   
*                                                                               
         OC    PNVDCLT,PNVDCLT     IF THERE IS A DETAIL CLIENT                  
         BZ    *+10                                                             
         MVC   QCLT,PNVDCLT           USE DETAIL CLIENT                         
*                                                                               
         GOTOR VALPRD              VALIDATE PRODUCT CODE                        
*                                                                               
         MVC   SVCLT,QCLT          RESTORE CLIENT CODE                          
*                                                                               
         MVC   PNVDPRD,QPRD        SAVE DETAIL PRODUCT CODE                     
*                                                                               
VRDPRX   DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM PUB                                                 
*                                                                               
VRDPB    DS    0H                                                               
*                                                                               
         XC    PNVDPUB,PNVDPUB    INIT DETAIL PUB                               
*                                                                               
         MVC   SVPUB,QPUB         SAVE CURRENT MASTER PUB                       
         MVC   SVPUBNM,PUBNM      SAVE CURRENT MASTER PUB NAME                  
*                                                                               
         LA    R2,DTLDPBH          POINT TO DETAIL PUB                          
*                                                                               
         MVI   FLDOPT,C'Y'         FIELD IS OPTIONAL                            
*                                                                               
         GOTOR VALPUB              VALIDATE PRODUCT CODE                        
*                                                                               
         MVC   PNVDPUB,QPUB        SAVE DETAIL PUB CODE                         
*                                                                               
         MVC   QPUB,SVPUB          RESTORE MASTER PUB                           
         MVC   PUBNM,SVPUBNM                                                    
*                                                                               
         OC    PNVDPUB,PNVDPUB     IF DETAIL PUB GIVEN                          
         BZ    VRDPBX                                                           
         CLC   PNVDPBCD,PNVHPBCD      BASE PUBS MUST MATCH                      
         BNE   VRDPUB1E                                                         
*                                                                               
VRDPBX   DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM ESTIMATE                                            
*                                                                               
VRDES    DS    0H                                                               
*                                                                               
         MVC   SVDES,PNVDEST       SAVE CURRENT LINE ITEM ESTIMATE              
         XC    PNVDEST,PNVDEST     INIT DETAIL ESTIMATE                         
*                                                                               
         LA    R2,DTLDESH          POINT TO DETAIL ESTIMATE                     
*                                                                               
         MVI   FLDOPT,C'Y'         FIELD IS OPTIONAL                            
*                                                                               
         OC    PNVDCLT,PNVDCLT     IF THERE IS A DETAIL CLIENT                  
         BZ    *+10                                                             
         MVC   QCLT,PNVDCLT           USE DETAIL CLIENT                         
*                                                                               
         GOTOR VALEST              VALIDATE ESTIMATE CODE                       
*                                                                               
         MVC   SVCLT,QCLT          RESTORE CLIENT CODE                          
*                                                                               
         MVC   PNVDEST,BEST        SAVE DETAIL ESTIMATE CODE                    
*                                                                               
VRDESX   DS    0H                                                               
*                                                                               
*        VALIDATE INSERTION DATE - ANY VALID DATE                               
*                                                                               
VRDDT    DS    0H                                                               
*                                                                               
         XC    PNVDBYDT,PNVDBYDT   INIT DETAIL DATE                             
*                                                                               
         LA    R2,DTLDDTH          POINT TO INSERTION DATE                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         CLI   FLDILEN,0           LINE ITEM RUN DATE MUST BE GIVEN             
         JE    VRDDTX                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
*        SINGLE DATE ONLY                                                       
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),('PVINTOD+PVINSGLO',(R4))              
*                                                                               
         TM    4(R1),PVRCONE       SINGLE DATE OKAY                             
         BO    *+12                                                             
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VRDTENVE                                                         
*                                                                               
         MVC   PNVDBYDT,PVALBSTA   SAVE BINARY FORMAT                           
*                                                                               
         TM    PVALASSM,PVALASD    IF START DAY ASSUMED                         
         BNO   *+8                                                              
         MVI   PNVDBYDT+2,0           ASSUME MONTHLY BUY                        
*                                                                               
VRDDTX   DS    0H                                                               
*                                                                               
*        VALIDATE LINE ITEM LINE #                                              
*                                                                               
VRLIN#   DS    0H                                                               
*                                                                               
         XC    PNVDLIN#,PNVDLIN#  INIT DETAIL ESTIMATE                          
*                                                                               
         LA    R2,DTLLINH          POINT TO LINE NUMBER                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INOUT LENGTH                             
         BZ    VRLIN#X             OKAY IF NOT ENTERED                          
*                                                                               
         TM    FLDIIND,FINPNUM     MUST BE NUMERIC                              
         BNO   VRLIN1E                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK NUMBER                                  
*                                                                               
         CP    DUB,=P'256'         MUST BE LESS THAN 256                        
         BNL   VRLIN2E                                                          
*                                                                               
         CVB   RF,DUB              CVB                                          
         STC   RF,PNVDLIN#         SAVE LINE NUMBER                             
*                                                                               
VRLIN#X  DS    0H                                                               
*                                                                               
*        VALIDATE IMPRESSIONS                                                   
*                                                                               
VRIMPS   DS    0H                                                               
*                                                                               
         MVC   SVAIMP,PNVDAIMP     SAVE CURRENT IMPS                            
         XC    PNVDAIMP,PNVDAIMP   INIT IMPS                                    
         LA    R2,DTLIMPSH         POINT TO IMPS FIELD                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET LENGTH OF FIELD                          
         BZ    VRIMPSX             SKIP IF IMPS NOT ENTERED                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',FLDDATA),(0,(RF)),0,0,0,0  VALIDATE           
*                                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         JNE   VRIMPNVE                                                         
*                                                                               
         ZAP   PNVDAIMP,4(8,R1)      SAVE  IMPS                                 
*                                                                               
VRIMPSX  DS    0H                                                               
*                                                                               
*        VALIDATE COST PER THOUSAND                                             
*                                                                               
VRCPMS   DS    0H                                                               
*                                                                               
         MVC   SVACPM,PNVDACPM     SAVE CURRENT CPMS                            
         XC    PNVDACPM,PNVDACPM   INIT CPMS                                    
         LA    R2,DTLCPMSH         POINT TO CPMS FIELD                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET LENGTH OF FIELD                          
         BZ    VRCPMSX             SKIP IF CPMS NOT ENTERED                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',FLDDATA),(0,(RF)),0,0,0,0  VALIDATE          
*                                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         JNE   VRCPMNVE                                                         
*                                                                               
         ZAP   PNVDACPM,4(8,R1)      SAVE  CPMS                                 
*                                                                               
VRCPMSX  DS    0H                                                               
*                                                                               
*        VALIDATE DISCREPANCY COMMENT CODE                                     
*                                                                               
VRDCM    DS    0H                                                               
*                                                                               
         MVC   SVDCM,PNVDDCM       SAVE CURRENT DISCREPANCY CODE                
         XC    PNVDDCM,PNVDDCM     INIT DCOMM CODE                              
*                                                                               
         LA    R2,DTLDCMH          POINT TO DISCREPANCY COMMENT CODE            
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET LENGTH OF ENTERED CODE                   
         BZ    VRDCMX              NO ENTRY IS OKAY                             
*                                                                               
         CLC   =C'***',8(R2)       *** SAME AS NO ENTRY                         
         BE    VRDCMX                                                           
*                                                                               
*        READ FOR DCM RECORD                                                    
*                                                                               
         XC    KEY,KEY             ESTABLISH DCM KEY                            
         LA    R4,KEY                                                           
         USING DCMKEY,R4                                                        
*                                                                               
         MVC   DCMKAGY,QAGY        SET AGENCY                                   
         MVI   DCMKMED,C'A'        SLWAYS 'A' FOR ALL MEDIA                     
         MVI   DCMKRCD,DCMKR1Q     SET FIRST RECORD ID                          
         MVI   DCMKRC2,DCMKR2Q     SET SECONDARY RECORD ID                      
*                                                                               
         MVC   DCMKDCM,SPACES      INIT CODE TO SPACES                          
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DCMKDCM(0),8(R2)    FILL IN DISCREP CODE                         
*                                                                               
         GOTOR HIGH                READ FOR RECORD                              
*                                                                               
         CLC   DCMKEY,KEYSAVE      ERROR IF CODE NOT FOUND                      
         BNE   VRDCMER1                                                         
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,DCMFRST          POINT TO FIRST ELEMENT IN RECORD             
         USING DCMHELD,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         OC    DCMHCTDT,DCMHCTDT   SKIP IF NO CUT OFF  DATE                     
         BZ    *+14                                                             
         CLC   DCMHCTDT,BTODAY     CUT OFF DATE MUST BE LATER THAN              
         BL    VRDCMER2               TODAY                                     
*                                                                               
*        MAKE SURE CODE VALID FOR THIS MEDIA                                    
*                                                                               
         CLC   =C'ALL',DCMHMEDS    OKAY IF CODE FOR ALL MEDIA                   
         BE    VRDCMMDX                                                         
         OC    DCMHMEDS,DCMHMEDS                                                
         BZ    VRDCMMDX                                                         
*                                                                               
         LA    R1,DCMHMEDS         POINT TO VALID MEDIAS                        
         LA    R0,L'DCMHMEDS       MAX NUMBER OF MEDIAS                         
*                                                                               
         CLI   0(R1),0             ERROR IF END OF LIST                         
         BE    VRDCMER3                                                         
         CLC   QMED,0(R1)          OKAY IF MEDIA IN LIST                        
         BE    VRDCMMDX                                                         
         LA    R1,1(R1)            BUMP POINTER                                 
         BCT   R0,*-22                                                          
*                                                                               
VRDCMMDX DS    0H                                                               
*                                                                               
         MVC   PNVDDCM,DCMKDCM     SAVE DCOMM CODE                              
*                                                                               
VRDCMX   DS    0H                                                               
*                                                                               
*        ANALYZE WHETHER BUY SERIAL # CONSISTENT                                
*        WITH LINE ITEM CLIENT/PRODUCT/ESTIMATE/LINE NUMBER                     
*                                                                               
VRBUY    DS    0H                                                               
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF BUY SERIAL NUMBER GIVEN              
         BNZ   VRBSER#                                                          
*                                                                               
*        NO BUY SERIAL #                                                        
*                                                                               
         OC    PNVDCLT,PNVDCLT     DONE IF NO CLIENT                            
         BNZ   *+14                                                             
         CLC   =C'***',PNVHCLT     OR MASTER CLIENT VARIOUS                     
         BE    VRBUYX                                                           
*                                                                               
         OC    PNVDPRD,PNVDPRD     DONE IF NO PRODUCT                           
         BZ    VRBUYX                                                           
*                                                                               
         OC    PNVDEST,PNVDEST     DONE IF NO ESTIMATE                          
         BZ    VRBUYX                                                           
*                                                                               
         OC    PNVDBYDT,PNVDBYDT   DONE IF NO INSERTION DATE                    
         BZ    VRBUYX                                                           
*                                                                               
         OC    PNVDLIN#,PNVDLIN#   DONE IF NO LINE NUMBER                       
         BZ    VRBUYX                                                           
*                                                                               
*        HAVE CLT/PRD/EST/LINE OF A BUY                                         
*        READ BUY RECORD AND GET SERIAL NUMBER                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH MASTER KEY OF BUY                  
         LA    R4,KEY                                                           
         USING PBUYKEY,R4                                                       
*                                                                               
         MVC   PBUYKAGY,QAGY       SET AGENCY                                   
         MVC   PBUYKMED,QMED       SET MEDIA                                    
         MVI   PBUYKRCD,PBUYKIDQ   SET RECORD IDENTIFER                         
         MVC   PBUYKCLT,PNVDCLT    SET CLIENT CODE                              
         OC    PBUYKCLT,PBUYKCLT   IF NO CLIENT GIVEN                           
         BNZ   *+10                                                             
         MVC   PBUYKCLT,PNVHCLT       DEFAULT TO MASTER CLIENT                  
         MVC   PBUYKPRD,PNVDPRD    SET PRODUCT                                  
         MVC   PBUYKPUB(6),PNVDPUB SET PUB CODE                                 
         OC    PBUYKPUB(6),PBUYKPUB IF PUB NOT GIVEN                            
         BNZ   *+10                                                             
         MVC   PBUYKPUB(6),PNVHPUB    DEFAULT TO MASTER PUB                     
         MVC   PBUYKDAT,PNVDBYDT   SET INSERTION DATE                           
*                                                                               
         CLI   PNVDBYDT+2,0        IF MONTHLY BUY                               
         BNE   *+8                                                              
         MVI   PBUYKDAT+2,1           SET TO FIRST OF MONTH                     
*                                                                               
         MVC   PBUYKEST,PNVDEST    SET ESTIMATE NUMBER                          
         MVC   PBUYKLIN,PNVDLIN#   SET LINE NUMBER                              
*                                                                               
         GOTOR HIGH                READ BUY RECORD POINTER                      
*                                                                               
         CLC   PBUYKEY,KEYSAVE     MUST FIND BUY RECORD                         
         BNE   VRBUY2E                                                          
*                                                                               
         GOTOR GETREC              READ BUY RECORD                              
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ADDING                           
         BNE   VRSER#1D                                                         
*                                                                               
*        MAKE SURE BUY NOT LINKED TO ANOTHER LINE ITEM                          
*              OF THIS INVOICE                                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,PBNVELQ      SET TO FIND INVOICE ELEMENT                  
*                                                                               
         BRAS  RE,GETEL            FIND INVOICE ELEMENT                         
*                                                                               
VRSER#1L DS    0H                                                               
*                                                                               
         BNE   VRSER#1D            NONE FOUND                                   
*                                                                               
         USING PBNVELMD,R6         ESTABLISH INVOICE ELEMENT                    
*                                                                               
         CLC   QSER#,PBNVSER#      MATCH ON THIS INVOICE SER#                   
         BNE   VRSER#1C            OKAY IF NO MATCH                             
*                                                                               
         CLI   ACTNUM,ACTADD       ERROR IF ADDING                              
         BE    VRSER#4E            NO DUPLICATE INVOICE ALLOWED                 
*                                                                               
         CLC   PBNVDSQN,PNVDKSQN   ERROR IF NOT SAME DETAIL SQN                 
         BNE   VRSER#4E                                                         
*                                                                               
VRSER#1C DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     VRSER#1L                                                         
*                                                                               
VRSER#1D DS    0H                  NO DUPLICATES FOUND                          
*                                                                               
         LR    R6,R4               FIND BUY SERIAL # ELM                        
         MVI   ELCODE,PSERELQ      SET ELEMENT CODE                             
*                                                                               
         BRAS  RE,GETEL            FIND SERIAL # ELEMENT                        
         BNZ   VRBUY3E             ELEMENT NOT FOUND                            
*                                                                               
         USING PSERELD,R6          ESTABLISH ELEMENT                            
*                                                                               
*        DISPLAY SERIAL NUMBER                                                  
*                                                                               
         LA    R2,DTLBS#H          POINT TO SERIAL NUMBER FIELD                 
*                                                                               
         OI    PSERNUM+L'PSERNUM-1,X'0F'  FORCE SIGN                            
*                                                                               
         MVC   PNVDSER#,PSERNUM    SAVE BUY SERIAL NUMBER                       
*                                                                               
         EDIT  PSERNUM,DTLBS#,0,ALIGN=LEFT DISPLAY SER #                        
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'DTLBS#    MAX OUTPUT                                   
*                                                                               
*        CHECK IF INVOICE POINTER ALREADY THERE                                 
*                                                                               
VRPBNV   DS    0H                                                               
*                                                                               
         MVI   ELCODE,PBNVELQ      SET ELEMENT CODE                             
         LR    R6,R4               POINT TO START OF BUY RECORD                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST OF TYPE                           
         BNZ   VRPBNVDN            NO MORE ELEMENTS                             
*                                                                               
VRPBNVLP DS    0H                                                               
*                                                                               
         USING PBNVELM,R6          ESTABLISH INVOICE POINTER ELM                
*                                                                               
         CLC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY IF SAME INV SER#                
         BNE   *+10                                                             
         CLC   PBNVDSQN,PNVDKSQN   AND SAME DETAIL SEQUENCE NUMBER              
         BE    VRPBNVX                 SKIP ADDING ELEMENT                      
*                                                                               
VRPBNVCN DS    0H                                                               
         BRAS  RE,NEXTEL           FIND NEXT ELEMENT                            
         BZ    VRPBNVLP            ANOTHER ONE FOUND                            
*                                                                               
VRPBNVDN DS    0H                                                               
*                                                                               
         OI    BUYSW,PBNVADDQ      ADD INVOICE POINTER TO BUY                   
*                                                                               
VRPBNVX  DS    0H                                                               
         B     VRBUYX                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
*        BUY SERIAL NUMBER ENTERED                                              
*                                                                               
VRBSER#  DS    0H                                                               
*                                                                               
         MVC   KEY,SVPSERKY        RESTORE BUY SERIAL PASSIVE                   
*                                                                               
         GOTOR GETREC              GET THE BUY                                  
*                                                                               
         L     R4,AIO              ESTABLISH FOUND BUY                          
         USING PBUYREC,R4                                                       
*                                                                               
         BRAS  RE,CKPRMINV         Check Prisma invoice requirements            
         JE    *+12                                                             
         LHI   RF,PPPRSIER         Cannot attach Prisma inv enabled buy         
         J     VRERR                                                            
*                                                                               
*        CHECK IF DETAIL PRD/EST/LIN ENTERED                                    
*                                                                               
         OC    PNVDPRD,PNVDPRD     CHECK PRODUCT                                
         BNZ   VRBSR#10                                                         
         OC    PNVDPUB,PNVDPUB     CHECK PUB                                    
         BNZ   VRBSR#10                                                         
         OC    PNVDBYDT,PNVDBYDT   CHECK DATE                                   
         BNZ   VRBSR#10                                                         
         OC    PNVDEST,PNVDEST     CHECK ESTIMATE                               
         BNZ   VRBSR#10                                                         
         OC    PNVDLIN#,PNVDLIN#   CHECK LINE NUMBER                            
         BNZ   VRBSR#10                                                         
*                                                                               
*        ONLY SERIAL NUMBER ENTERED                                             
*              FILL IN DETAILS FROM BUY                                         
*                                                                               
         MVC   PNVDCLT,PBUYKCLT    CLIENT                                       
         MVC   PNVDPRD,PBUYKPRD    PRODUCT                                      
         MVC   PNVDPUB,PBUYKPUB    PUB                                          
         MVC   PNVDBYDT,PBUYKDAT   DATE                                         
*                                                                               
         CLI   PBDFREQ,C'M'        IF MONTHLY BUY                               
         BNE   *+8                                                              
         MVI   PNVDBYDT+2,0           KILL DAY                                  
*                                                                               
         MVC   PNVDEST,PBUYKEST    ESTIMATE                                     
         MVC   PNVDLIN#,PBUYKLIN   LINE NUMBER                                  
*                                                                               
         CLC   PNVDPBCD,PNVHPBCD   BASE PUBS MUST MATCH                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     VRBUYX                                                           
*                                                                               
*        HAVE BUY DETAILS DISPLAYED.                                            
*                                                                               
VRBSR#10 DS    0H                                                               
*                                                                               
*        MUST MATCH BUY'S VALUES                                                
*                                                                               
         LA    RF,PNVDCLT          MATCH TO DETAIL CLIENT                       
*                                                                               
         CLC   PNVDCLT,SPACES      IF CLIENT MISSING                            
         BH    *+8                                                              
         LA    RF,PNVHCLT             USE HEADER CLIENT                         
*                                                                               
         CLC   PBUYKCLT,0(RF)                                                   
         BNE   VRBSR#1E               ERROR                                     
*                                                                               
         CLC   PBUYKPRD,PNVDPRD    MUST MATCH DETAIL PRODUCT                    
         BNE   VRBSR#1E                                                         
*                                                                               
         LA    RF,PNVDPUB          MATCH TO DETAIL PUB                          
         OC    PNVDPUB,PNVDPUB     IF PUB MISSING                               
         BNZ   *+8                                                              
         LA    RF,PNVHPUB             USE HEADER PUB                            
         CLC   PBUYKPUB,0(RF)                                                   
         BNE   VRBSR#1E               ERROR                                     
*                                                                               
         MVC   WORK(3),PNVDBYDT    MOVE DATE TO WORK AREA                       
         CLI   PNVDBYDT+2,0        IF MONTHLY BUY                               
         BNE   *+8                                                              
         MVI   WORK+2,1               SET TO FIRST OF MONTH                     
*                                                                               
         CLC   PBUYKDAT,WORK                                                    
         BNE   VRBSR#1E                                                         
*                                                                               
         CLC   PBUYKEST,PNVDEST    MUST MATCH DETAIL ESTIMATE                   
         BNE   VRBSR#1E                                                         
*                                                                               
         CLC   PBUYKLIN,PNVDLIN#   MUST MATCH DETAIL LINE #                     
         BNE   VRBSR#1E                                                         
*                                                                               
*        SERIAL NUMBER MATCHES BUY DETAILS                                      
*                                                                               
         CLC   SVBSR#,PNVDSER#     IF SERIAL ACTUALLY CHANGED                   
         BE    *+8                                                              
         OI    BUYSW,SER#CHGQ         SET FLAG                                  
*                                                                               
         B     VRBUYX                                                           
*                                                                               
VRBUYX   DS    0H                                                               
*                                                                               
         BRAS  RE,PRSMINVD         Set prisma invoice details                   
*                                                                               
*        UPDATE ACTIVITY ELEMENT                                                
*                                                                               
         MVC   SVDTLELM,ELEMENT    SAVE DETAIL ELEMENT                          
*                                                                               
*        ADD DETAIL ELEMENT                                                     
*                                                                               
         L     RF,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING ELEMENT                            
         BNE   *+8                                                              
         L     RF,ADDELM              USE ADDELM ROUTINE                        
*                                                                               
         GOTOR (RF),DMCB,SVDTLELM   ELEMENT TO RECORD                           
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,UPDACT           UPDATE ACTIVITY                              
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ADJUST PASSIVES                                                        
*                                                                               
         MVC   ELEMENT,SVDTLELM    RESTORE DETAIL ELEMENT                       
*                                  DONE BECAUSE OF DEPENDENT USING              
*                                                                               
         CLC   PNVDCLT,SPACES      SKIP IF NO CLIENT                            
         BH    *+10                                                             
         OC    PNVDPUB,PNVDPUB     AND  IF NO PUB                               
         BZ    VRPSVX                                                           
*                                                                               
         CLC   PNVDCLT,PNVHCLT     SKIP IF CLIENT EQUALS HDR CLT                
         BNE   *+10                                                             
         CLC   PNVDPUB,PNVHPUB     AND PUB EQUALS HDR PUB                       
         BE    *+8                                                              
         BRAS  RE,NEWPSV           ADD ANY NEW PASSIVES                         
*                                                                               
VRPSVX   DS    0H                                                               
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - VRBYUPD'               
***********************************************************************         
*                                                                     *         
*        UPDATE BUY RECORD ELEMENTS                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRBYUPD  DS    0H                                                               
*                                                                               
         CLC   SVBSR#,PNVDSER#     SKIP IF BUY SER#    UNCHANGED                
         JNE   VRBYBNV                                                          
         CLI   PNVHIVSR,INVPRM_Q   Prisma invoice?                              
         JE    VRBYBNV                                                          
         CLI   PNVHIVSR,INVRAD_Q   Radia invoice?                               
         JE    VRBYBNV                                                          
         J     VRBYUPDX                                                         
*                                                                               
*        BUY INVOICE POINTER                                                    
*                                                                               
VRBYBNV  DS    0H                                                               
*                                                                               
         CLC   SVBSR#,PNVDSER#     SKIP IF BUY SER# UNCHANGED                   
         BE    VRBYDLX                                                          
*                                                                               
         OC    SVBSR#,SVBSR#       SKIP IF NO OLD BUY SER#                      
         BZ    VRBYDLX                                                          
*                                                                               
*        DELETE POINTER FROM OLD BUY                                            
*                                                                               
VRBYDL   DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              POINT TO KEY BUILD AREA                      
         USING PSERKEY,R4          ESTABLISH BUY SERIAL KEY                     
*                                                                               
         MVC   PSERKAGY,QAGY       SET AGENCY                                   
         MVC   PSERKMED,QMED       SET MEDIA                                    
         MVI   PSERKRCD,PSERKIDQ   SET RECORD CODE                              
         MVC   PSERKCLT,PNVDCLT    SET CLIENT                                   
*                                                                               
         OC    PSERKCLT,PSERKCLT   IF CLIENT MISSING                            
         BNZ   *+10                                                             
         MVC   PSERKCLT,QCLT          USE MASTER CLIENT                         
*                                                                               
         CLC   =C'***',PSERKCLT    CLIENT CAN'T BE VARIOUS                      
         BE    VRSER#2E                                                         
*                                                                               
         ZAP   DUB,=P'1000000000'  COMPLEMENT SER#                              
         SP    DUB,SVBSR#                                                       
         ZAP   PSERKNUM,DUB        SET SERIAL NUMBER                            
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     MUST FIND IT                                 
         BNE   VRSER#3E                                                         
*                                                                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
*                                                                               
         GOTOR GETREC              READ IN BUY                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND BUY RECORD                    
         USING PBUYREC,R4          ESTABLISH BUY RECORD                         
*                                                                               
         SR    R0,R0               INIT ELEMENT COUNTER                         
*                                                                               
*        DELETE OLD POINTER ELEMENT                                             
*                                                                               
         MVI   ELCODE,PBNVELQ      SET ELEMENT CODE                             
         LR    R6,R4               POINT TO START OF BUY RECORD                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST INVOICE POINTER                   
*                                                                               
VRBYDLLP DS    0H                                                               
*                                                                               
         BNZ   VRBYDLDN            NO MORE ELEMENTS                             
*                                                                               
         USING PBNVELMD,R6         ESTABLISH INVOICE POINTER                    
*                                                                               
         AHI   R0,1                INCREMENT INVOICE ELEMENT COUNTER            
*                                                                               
         CLC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY FIND OLD PTR                    
         BNE   VRBYDLCN                                                         
         CLC   PBNVDSQN,PNVDKSQN   AND SAME DETAIL SQN                          
         BNE   VRBYDLCN                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),(R6),0   DELETE ELEMENT                 
*                                                                               
VRBYDLCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT INVOICE PTR ELM                    
*                                                                               
         B     VRBYDLLP                                                         
*                                                                               
VRBYDLDN DS    0H                                                               
*                                                                               
*        DON'T UPDATE MATCH STATUS ELEMENT                                      
*                                                                               
         B     VRBYMATN                                                         
*                                                                               
*        REMOVE MATCHED STATUS FROM BUY IF NO MORE INVOICES                     
*              ASSOCIATED LINKED TO THE BUY                                     
*                                                                               
         CHI   R0,1                SKIP IF MORE THAN ONE INVOICE ELM            
         BH    VRBYMATN                                                         
*                                                                               
         NI    PBDSTAT,X'FF'-X'40' NO LONGER MATCHED                            
*                                                                               
*        REMOVE TEARSHEET INDICATOR FROM BUY                                    
*                                                                               
         NI    PBDSTAT,X'FF'-X'10' NO LONGER TEARSHEET FROM INV                 
*                                                                               
*        RESET MATCHING AND DISCREPANCY STATUSES                                
*                                                                               
         MVI   ELCODE,PBMATELQ     SET ELEMENT CODE                             
         LR    R6,R4               POINT TO START OF BUY RECORD                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST INVOICE POINTER                   
*                                                                               
         BNZ   VRBYMATN            NO MORE ELEMENTS                             
*                                                                               
         USING PBMATELD,R6         ESTABLISH MATCH STATUS ELM                   
*                                                                               
         MVI   PBMTSTAT,PBMTSNIQ   NO INVOICE                                   
         MVI   PBMTDSTA,PBMTDNAQ   NOT APPLICABLE                               
*                                                                               
VRBYMATN DS    0H                                                               
*                                                                               
         GOTO1 PUTREC              WRITE BUY BACK TO FILE                       
*                                                                               
VRBYDLX  DS    0H                                                               
*                                                                               
*        ADD INVOICE POINTER ELEMENT TO BUY                                     
*                                                                               
VRBYAD   DS    0H                                                               
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF NO BUY SERIAL #                      
         BZ    VRBYADX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              POINT TO KEY BUILD AREA                      
         USING PSERKEY,R4          ESTABLISH BUY SERIAL KEY                     
*                                                                               
         MVC   PSERKAGY,QAGY       SET AGENCY                                   
         MVC   PSERKMED,QMED       SET MEDIA                                    
         MVI   PSERKRCD,PSERKIDQ   SET RECORD CODE                              
         MVC   PSERKCLT,PNVDCLT    SET CLIENT                                   
*                                                                               
         OC    PSERKCLT,PSERKCLT   IF CLIENT MISSING                            
         BNZ   *+10                                                             
         MVC   PSERKCLT,QCLT          USE MASTER CLIENT                         
*                                                                               
         CLC   =C'***',PSERKCLT    CLIENT CAN'T BE VARIOUS                      
         BE    VRSER#2E                                                         
*                                                                               
         ZAP   DUB,=P'1000000000'  COMPLEMENT SER#                              
         SP    DUB,PNVDSER#                                                     
         ZAP   PSERKNUM,DUB        SET SERIAL NUMBER                            
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     MUST FIND IT                                 
         BNE   VRSER#3E                                                         
*                                                                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
*                                                                               
         GOTOR GETREC              READ IN BUY                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND BUY RECORD                    
         USING PBUYREC,R4          ESTABLISH BUYREC                             
*                                                                               
         XC    BUYELM,BUYELM                                                    
         LA    R6,BUYELM           ESTABLISH INVOICE POINTER ELEMENT            
         USING PBNVELMD,R6                                                      
*                                                                               
         MVI   PBNVELM,PBNVELQ     SET ELEMENT ID                               
         MVI   PBNVLEN,PBNVLENQ    SET ELEMENT LENGTH                           
         MVC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY  SET INV SER #                  
         MVC   PBNVDSQN,PNVDKSQN   SET DETAIL SEQ NUMBER                        
         MVC   PBNVINV#,PNVHINV#   SET INVOICE NUMBER                           
         MVC   PBNVISRC,PNVHIVSR   Set invoice header source                    
*                                                                               
         LR    R6,R4               COPY START OF BUY RECORD                     
         MVC   ELCODE,BUYELM       SET FOR CURRENT BUY ELEMENT                  
*                                                                               
         BRAS  RE,GETEL            FIND FIRST                                   
*                                                                               
VRBYADLP DS    0H                                                               
*                                                                               
         BNZ   VRBYADDN            ELEMENT NOT FOUND                            
*                                                                               
         USING PBNVELMD,R6         ESTABLISH BUY INVOICE POINTER                
*                                                                               
         CLC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY MATCH TO THIS INV               
         BNE   VRBYADCN                                                         
         CLC   PBNVDSQN,PNVDKSQN   AND SAME DETAIL SQN                          
         BE    VRBYADFD                                                         
*                                                                               
VRBYADCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT INVOICE PTR ELM                    
*                                                                               
         B     VRBYADLP                                                         
*                                                                               
VRBYADDN DS    0H                  ADD NEW POINTER TO BUY                       
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  ADD ELEMENT                
*                                                                               
         CLI   PNVHIVSR,INVIPS_Q   IPS electronic invoice?                      
         JE    VRBYAD30                                                         
         CLI   PNVHIVSR,INVPRM_Q   Prisma invoice?                              
         JE    VRBYAD30                                                         
         CLI   PNVHIVSR,INVRAD_Q   Radia invoice?                               
         JNE   VRBYADFD                                                         
VRBYAD30 XC    BUYELM,BUYELM                                                    
         LA    R6,BUYELM                                                        
         USING PBMATELD,R6         Buy elem - Invoice matching status           
         MVI   PBMATELM,PBMATELQ   Set element ID                               
         MVI   PBMATLEN,PBMTLENQ   Set element length                           
         MVI   PBMTSTAT,PBMTSPDQ   Default to pending status                    
         BRAS  RE,GETIDKPV         Get IDK profile values                       
         CLI   SVIDKPRF+04,C'Y'    Match buy on Prisma INV recon?               
         JNE   VRBYAD36                                                         
         CLI   PNVHSTAT,PNVHMATQ   Matched?                                     
         JNE   VRBYAD36                                                         
         MVI   PBMTSTAT,PBMTSMTQ   Insertion matching status is matched         
         OI    PBDSTAT,X'40'       Matched to invoice                           
         DROP  R6                                                               
*                                                                               
VRBYAD36 LR    R6,R4               Point to begining of Buy record              
         MVC   ELCODE,BUYELM       Look for current Buy element                 
         BRAS  RE,GETEL                                                         
         JE    VRBYADFD            Do not add if already there                  
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  Add element                
*                                                                               
VRBYADFD DS    0H                  ELEMENT ALREADY IN BUY                       
*                                                                               
         CLI   PNVHIVSR,INVIPS_Q   IPS electronic invoice?                      
         JE    *+8                                                              
         CLI   PNVHIVSR,INVPRM_Q   Prisma invoice?                              
         JE    *+12                                                             
         CLI   PNVHIVSR,INVRAD_Q   Radia invoice?                               
         JNE   VRBYAD60                                                         
         LR    R6,R4               Point to begining of Buy record              
         MVI   ELCODE,BYCCIDQ      Look for custom column elements              
         BRAS  RE,GETEL                                                         
         JNE   VRBYAD56                                                         
*                                                                               
         USING BYCCELM,R6                                                       
VRBYAD52 CLI   BYCCELM,0           End of record?                               
         JE    VRBYAD56                                                         
         CLC   BYCCSQN,=AL2(8208)  Reconciled standard column?                  
         JE    VRBYAD55                                                         
         CLC   BYCCSQN,=AL2(8209)  Recon status standard column?                
         JE    VRBYAD55                                                         
VRBYAD53 BRAS  RE,NEXTEL                                                        
         J     VRBYAD52                                                         
*                                                                               
VRBYAD55 CLI   BYCCDATA,C'Y'       Already reconciled?                          
         JE    VRBYAD60                                                         
         GOTO1 VRECUP,DMCB,(X'01',(R4)),(R6),0   Delete element                 
         LR    R6,R4               Point to begining of Buy record              
         MVI   ELCODE,BYCCIDQ      Look for custom column elements              
         BRAS  RE,GETEL                                                         
         JE    VRBYAD52                                                         
         DROP  R6                                                               
*                                                                               
VRBYAD56 XC    BUYELM,BUYELM                                                    
         LA    RE,BUYELM                                                        
         USING BYCCELM,RE          Buy elem - Custom column                     
         MVI   BYCCELM,BYCCIDQ     Set element ID                               
         MVI   BYCCLEN,9           Set element length                           
         MVC   BYCCSQN,=AL2(8208)  Reconciled column sequence #                 
         MVC   BYCCDATA(3),=C'YES' Set Reconcilated column to YES               
         CLI   PNVHSTAT,PNVHMATQ   Matched?                                     
         JE    *+14                                                             
         MVC   BYCCDATA(2),=C'NO'  Set Reconcilated column to NO                
         MVI   BYCCLEN,8           Set element length                           
         DROP  RE                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  Add element                
*                                                                               
         XC    BUYELM,BUYELM                                                    
         LA    RE,BUYELM                                                        
         USING BYCCELM,RE          Buy elem - Custom column                     
         MVI   BYCCELM,BYCCIDQ     Set element ID                               
         MVI   BYCCLEN,9           Set element length                           
         MVC   BYCCSQN,=AL2(8209)  Recon status column sequence #               
         MVC   BYCCDATA(3),=C'YES' Set Recon status column to YES               
         CLI   PNVHSTAT,PNVHMATQ   Matched?                                     
         JE    *+14                                                             
         MVC   BYCCDATA(2),=C'NO'  Set Reconcilated column to NO                
         MVI   BYCCLEN,8           Set element length                           
         DROP  RE                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  Add element                
*                                                                               
VRBYAD60 CLI   PNVHIVSR,INVIPS_Q   IPS electronic invoice?                      
         JE    *+8                                                              
         CLI   PNVHIVSR,INVPRM_Q   Prisma invoice?                              
         JE    *+8                                                              
         CLI   PNVHIVSR,INVRAD_Q   Radia invoice?                               
         JNE   VRBYAD70                                                         
         BRAS  RE,GETIDKPV         Get IDK profile values                       
         CLI   SVIDKPRF+04,C'Y'    Match buy on Prisma INV recon?               
         JNE   VRBYAD70                                                         
         CLI   PNVHSTAT,PNVHMATQ   Matched?                                     
         JNE   VRBYAD70                                                         
         OI    PBDSTAT,X'40'       Matched to invoice                           
*                                                                               
         LR    R6,R4               Point to begining of Buy record              
         MVI   ELCODE,PBMATELQ     Insertion matching status element            
         USING PBMATELD,R6         Buy elem - Invoice matching status           
         BRAS  RE,GETEL                                                         
         JNE   *+12                                                             
         MVI   PBMTSTAT,PBMTSMTQ   Set buy matching status to matched           
         J     VRBYAD70                                                         
         XC    BUYELM,BUYELM                                                    
         LA    R6,BUYELM                                                        
         MVI   PBMATELM,PBMATELQ   Set element ID                               
         MVI   PBMATLEN,PBMTLENQ   Set element length                           
         MVI   PBMTSTAT,PBMTSMTQ   Set buy matching status to matched           
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  Add element                
         J     VRBYAD70                                                         
         DROP  R6                                                               
*                                                                               
VRBYAD70 DS    0H                                                               
*                                                                               
VRBYADX  DS    0H                                                               
*                                                                               
         GOTO1 PUTREC              WRITE BUY BACK TO FILE                       
*                                                                               
VRBYBNVX DS    0H                                                               
*                                                                               
VRBYUPDX DS    0H                                                               
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   VRLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
VRLNKX   DS    0H                                                               
*                                                                               
         BRAS  RE,DR               DISPLAY DETAIL                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - VRERR '                
***********************************************************************         
*                                                                     *         
*        VALREC ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDTENEE LHI   RF,PPEDTENE         LINE ITEM DATE NOT ENTERED                   
         J     VRERR                                                            
*                                                                               
VRBYLKER LHI   RF,PPELOCKD                                                      
         J     VRERR                                                            
*                                                                               
VRDTENVE LHI   RF,PPEDTENV         LINE ITEM DATE NOT VALID                     
         J     VRERR                                                            
*                                                                               
VRCLRNNE LHI   RF,PPECLRNV         NUMBER OF COLORS NOT VALID                   
         J     VRERR                                                            
*                                                                               
VRLNSNNE LHI   RF,PPELNSNV         NUMBER OF LINES NOT VALID                    
         J     VRERR                                                            
*                                                                               
VRRTENVE LHI   RF,PPERTENV         RATE NOT VALID                               
         J     VRERR                                                            
*                                                                               
VRPRMNVE LHI   RF,PPEPRMNV         PREMIUM NOT VALID                            
         J     VRERR                                                            
*                                                                               
VRGRSNVE LHI   RF,PPEGRSNV         GROSS NOT VALID                              
         J     VRERR                                                            
*                                                                               
VRNETNVE LHI   RF,PPENETNV         NET NOT VALID                                
         J     VRERR                                                            
*                                                                               
VRDCL1E  LHI   RF,PPEVARNV         CLIENT *** NOT OK IN DETAIL                  
         J     VRERR                                                            
*                                                                               
VRSER#1E LHI   RF,PPESR#NM         SERIAL # MUST BE NUMERIC                     
         J     VRERR                                                            
*                                                                               
VRSER#2E LHI   RF,PPEVARNV         CLIENT MUST BE NON C'***'                    
         J     VRERR                                                            
*                                                                               
VRSER#3E LHI   RF,PPEBUYNF         BUY NOT ON FILE                              
         J     VRERR                                                            
*                                                                               
VRSER#4E LHI   RF,PPESR#4E         BUY ALREADY LINKED TO INVOICE                
         J     VRERR                                                            
*                                                                               
VRLIN1E  LHI   RF,PPELINNM         BUY LINE NUMBER MUST BE NUMERIC              
         J     VRERR                                                            
*                                                                               
VRLIN2E  LHI   RF,PPELINNM         BUY LINE NUMBER MUST BE < 256                
         J     VRERR                                                            
*                                                                               
VRIMPNVE LHI   RF,PPEIMPNV         IMPRESSIONS NOT VAILD                        
         J     VRERR                                                            
*                                                                               
VRCPMNVE LHI   RF,PPECPMNV         CPMS        NOT VAILD                        
         J     VRERR                                                            
*                                                                               
VRBUY2E  LHI   RF,PPEBUYNF         BUYREC NOT FOUND                             
         LA    R2,DTLDCLH          POINT TO DETAIL CLIENT                       
         J     VRERR                                                            
*                                                                               
VRBSR#1E LHI   RF,PPENMTCH         BUYREC DOESN'T MATCH DETAILS                 
         LA    R2,DTLDCLH          POINT TO DETAIL CLIENT                       
         J     VRERR                                                            
*                                                                               
VRDPUB1E LHI   RF,PPEBASPB         DTL BASE PUB NOT SAME AS HDR BASE            
         LA    R2,DTLPUBH          POINT TO DETAIL PUB                          
         J     VRERR                                                            
*                                                                               
VRBUY3E  LHI   RF,PPESR#NF         SERIAL NUMBER MISSING                        
         J     VRERR                                                            
*                                                                               
VRDCMER1 LHI   RF,PPEDCMNF         D COMMENT CODE NOT FOUND                     
         J     VRERR                                                            
*                                                                               
VRDCMER2 LHI   RF,PPEDCMCD         CUT OFF DATE PASSED                          
         J     VRERR                                                            
*                                                                               
VRDCMER3 LHI   RF,PPEDCMMD         D COMMENT CODE NOT VALID FOR MEDIA           
         J     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - EDTRTE'                
***********************************************************************         
*                                                                     *         
*        EDIT RATE LIKE THE BUY DOES                                  *         
*                                                                     *         
*NTRY    R1==> RATE FIELD                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTRTE   NTR1  BASE=*,LABEL=*      EDIT RATE FIELD                              
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         USING PNVDTLD,ELEMENT     ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT KEY                 
         USING PNVRECD,MINMKEY     ESTABLISH MASTER RECORD  KEY                 
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         MVI   PNVDCSTP,0          INIT COST TYPE                               
         MVI   PNVDCSIN,C' '       INIT COST INDICATOR                          
*                                                                               
         MVI   BYTE,2              DEFAULT TO 2 DECIMALS RATE                   
*                                                                               
         CLI   QMED,C'N'           IF NEWSPAPER                                 
         BNE   *+12                                                             
         MVI   PNVDCTPN,C'U'          INIT COST TYPE NEWSPAPER                  
         MVI   BYTE,5                 DEFAULT TO 5 DECIMALS                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,FLDILEN        GET LENGTH                                   
         BZ    EDTRTEX               NOTHING ENTERED                            
*                                                                               
         LA    R4,FLDDATA          POINT TO INPUT                               
*                                                                               
         TM    FLDIIND,FINPALF     TEST VALID ALPHA                             
         BO    EDRTALF             (FREE, ETC)                                  
*                                                                               
EDRTLOOP DS    0H                                                               
*                                                                               
         CLI   0(R4),C'S'          IF GROSS=NET                                 
         BNE   EDRTSN                                                           
*                                                                               
         MVI   PNVDCSIN,C'S'          SET COST TYPE                             
*                                                                               
         B     EDRTCONT            GO LOOK AT NEXT INPUT POSITION               
*                                                                               
EDRTSN   DS    0H                                                               
*                                                                               
         CLI   0(R4),C'C'          IF COMMISSION ONLY                           
         BNE   EDRTCN                                                           
*                                                                               
         CLI   PNVDCSIN,C' '          MUST BE CURRENTLY GROSS                   
         BNE   EDRTNVE                                                          
*                                                                               
         MVI   PNVDCSIN,C'C'          SET COST TYPE                             
*                                                                               
         B     EDRTCONT            GO LOOK AT NEXT INPUT POSITION               
*                                                                               
EDRTCN   DS    0H                                                               
*                                                                               
         CLI   0(R4),C'N'          IF NET TO BE GROSSED UP                      
         BNE   EDRTNN                                                           
*                                                                               
         MVI   PNVDCSTP,C'N'          SET COST TYPE                             
*                                                                               
         B     EDRTCONT            GO LOOK AT NEXT INPUT POSITION               
*                                                                               
EDRTNN   DS    0H                                                               
*                                                                               
         CLI   QMED,C'N'           IF NEWSPAPER                                 
         BNE   EDRTTN                                                           
*                                                                               
         CLI   0(R4),C'T'          IF TOTAL RATE                                
         BNE   EDRTTN                                                           
*                                                                               
         MVI   PNVDCTPN,C'T'          SET COST TYPE NEWSPAPER                   
         MVI   BYTE,2                 SET TO 2 DECIMALS RATE                    
*                                                                               
         B     EDRTCONT            GO LOOK AT NEXT INPUT POSITION               
*                                                                               
EDRTTN   DS    0H                                                               
*                                                                               
         B     EDRTDONE                                                         
*                                                                               
EDRTCONT DS    0H                                                               
         LA    R4,1(R4)               BUMP TO NEXT INPUT POSITION               
         BCTR  R3,0                   DECREMENT INPUT LENGTH                    
         B     EDRTLOOP                                                         
*                                                                               
EDRTDONE DS    0H                                                               
*                                                                               
EDRTALF  DS    0H                  ALL ALPHA INPUT                              
*                                                                               
         MVC   PNVDRDEC,BYTE       SET NUMBER OF DECIMAL PLACES                 
*                                                                               
         OI    BYTE,X'80'          RETURN PL8 VALUE                             
*                                                                               
         CLC   =C'SFREE',0(R4)     IF FREE WITH GROSS=NET                       
         BNE   EDRTCSV             NO                                           
*                                                                               
         MVI   PNVDCSIN,C'S'       SET COST TYPE                                
*                                  RESET FIELDS FOR CASHVAL CALL                
         LA    R4,1(R4)            POINT TO "FREE"                              
         LA    R3,4                SET FIELD LENGTH WITHOUT "S"                 
*                                                                               
EDRTCSV  DS    0H                                                               
         GOTO1 CASHVAL,DMCB,(BYTE,(R4)),(0,(R3)),0,0,0,0  VALIDATE              
*                                                                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   EDRTNVE                                                          
*                                                                               
         ZAP   PNVDRATE,4(8,R1)      SAVE  RATE                                 
*                                                                               
EDTRTEX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
EDRTNVE  LHI   RF,PPECSTNV         INVALID RATE/COST                            
         J     EDRTERR                                                          
*                                                                               
EDRTERR  DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - EDTPREM'               
***********************************************************************         
*                                                                     *         
*        EDIT PREMIUM LIKE THE BUY DOES                               *         
*                                                                     *         
*NTRY    R1==> PREMIUM FIELD                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTPREM  NTR1  BASE=*,LABEL=*      EDIT PREMIUM FIELD                           
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7          ESTABLISH MINIO CONTROL BLOCK                
*                                                                               
         USING PNVDTLD,ELEMENT     ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT KEY                 
         USING PNVRECD,MINMKEY     ESTABLISH MASTER RECORD  KEY                 
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         MVI   PNVDPRTP,0          INIT PREMIUM COST TYPE                       
         MVI   PNVDPRIN,C' '       INIT PREMIUM COST INDICATOR                  
         XC    PNVDPREM,PNVDPREM   INIT PREMIUM COST                            
         MVI   PNVDNCL,0           INIT NUMBER OF COLORS                        
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,FLDILEN       GET INPUT LENGTH                              
         BZ    EDTPREMX            DONE IF NOT ENTERED                          
*                                                                               
         CLI   QMED,C'N'           MUST BE NEWSPAPERS                           
         BNE   EDPRNWSE                                                         
*                                                                               
         CLC   =C'FREE',FLDDATA    C'FREE' ALLOWED                              
         BNE   *+14                                                             
         ZAP   PNVDPREM,=P'0'         STORE AS ZERO                             
         B     EDTPREMX                                                         
*                                                                               
         LA    R6,FLDDATA          POINT TO INPUT                               
*                                                                               
         CLI   FLDDATA+1,C'C'      SKIP IF NO COLORS ENTERED                    
         BNE   EDPRCLRX                                                         
*                                                                               
         CLI   FLDDATA,C'1'        NUMBER OF COLORS MUST BE NUMERIC             
         BL    EDPRCLRE                                                         
         CLI   FLDDATA,C'4'        AND 1 TO 4                                   
         BH    EDPRCLRE                                                         
*                                                                               
         MVC   PNVDNCL,FLDDATA     SAVE NUMBER OF COLORS                        
         NI    PNVDNCL,X'0F'       KILL ZONE                                    
*                                                                               
         CLI   FLDILEN,2           TEST INPUT LENGTH                            
         BE    EDPRCSTX              NUMBER OF COLORS ONLY                      
*                                                                               
         CLI   FLDDATA+2,C'/'      MUST BE '/'                                  
         BNE   EDPRNVE                                                          
*                                                                               
         LA    R6,FLDDATA+3        POINT TO START OF DOLLARS                    
         AHI   R5,-3               DECREMENT LENGTH COUNTER                     
         BNP   EDPRCS1E            MUST HAVE A COST                             
*                                                                               
EDPRCLRX DS    0H                                                               
*                                                                               
         MVC   PNVDPRIN,PNVDCSIN   SET DEFAULT PREMIUM RATE IND                 
*                                                                               
*        DETERMINE COST TYPE AND INDICATOR                                      
*                                                                               
EDPRLOOP DS    0H                  CHECK FOR COST PREFIXES                      
*                                                                               
         CLI   0(R6),C'S'          TEST GROSS=NET OVERRIDE                      
         BE    *+8                                                              
         CLI   0(R6),C'C'          TEST COMMISSION ONLY OVERRIDE                
         BNE   EDPRINN                                                          
*                                                                               
         MVC   PNVDPRIN,0(R6)      SET PREMIUM INDICATOR                        
*                                                                               
         CLC   PNVDCSIN,PNVDPRIN   MUST BE THE SAME AS THAT FOR RATE            
         BNE   EDPRCS2E                                                         
*                                                                               
         B     EDPRCONT                                                         
*                                                                               
EDPRINN  DS    0H                                                               
*                                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDPRNN                                                           
*                                                                               
         MVC   PNVDPRTP,0(R6)      SET PREMIUM COST TYPE                        
*                                                                               
         B     EDPRCONT                                                         
*                                                                               
EDPRNN   DS    0H                                                               
*                                                                               
         B     EDPRDONE            NO MORE PREFIXES                             
*                                                                               
EDPRCONT DS    0H                                                               
*                                                                               
         LA    R6,1(R6)            BUMP INPUT POINTER                           
*                                                                               
         BCT   R5,EDPRLOOP                                                      
*                                                                               
         B     EDPRNVE             MUST HAVE A COST                             
*                                                                               
EDPRDONE DS    0H                                                               
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',(R6)),(0,(R5)),0,0,0,0  VALIDATE             
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    EDPRCS3E                                                         
*                                                                               
         ZAP   PNVDPREM,4(8,R1)      SAVE  RATE                                 
*                                                                               
EDPRCSTX DS    0H                                                               
*                                                                               
EDTPREMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
EDPRCS1E LHI   RF,PPEPRCNE         PREMIUM COST NOT ENTERED                     
         J     EDPRERR                                                          
*                                                                               
EDPRNWSE LHI   RF,PPEPRNWS         RESTRICTED TO NEWSPAPER                      
         J     EDPRERR                                                          
*                                                                               
EDPRCS2E LHI   RF,PPEPRTPX         COST TYPES INCOMPATIBLE                      
         J     EDPRERR                                                          
*                                                                               
EDPRCS3E LHI   RF,PPEPRCNV         COST NOT VALID                               
         J     EDPRERR                                                          
*                                                                               
EDPRCLRE LHI   RF,PPEPRCLR         NUMBER OF COLORS NOT NUMERIC                 
         J     EDPRERR                                                          
*                                                                               
EDPRNVE  LHI   RF,PPEPRMNV         INVALID PREMIUM                              
         J     EDPRERR                                                          
*                                                                               
EDPRERR  DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - VRPSV'                 
***********************************************************************         
*                                                                     *         
*        UPDATE PASSIVE POINTERS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NEWPSV   NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         USING PNVDTLD,ELEMENT     ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT KEY                 
         USING PNVRECD,MINMKEY     ESTABLISH MASTER RECORD  KEY                 
*                                                                               
         IC    R0,DMINBTS          SAVE SETTING                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH B1 PASSIVE KEY                     
         USING PNV1KEY,R4                                                       
*                                                                               
*        ADD B1 PASSIVE                                                         
*                                                                               
         MVC   PNV1AGY,PNVKAGY     SET AGENCY                                   
         MVC   PNV1MED,PNVKMED     SET MEDIA                                    
         MVI   PNV1RCD,PNV1RCDQ    SET PASSIVE CODE                             
         MVC   PNV1CLT,PNVDCLT     SET CLIENT                                   
*                                                                               
         CLC   PNV1CLT,SPACES      IF NO CLIENT                                 
         BH    *+10                                                             
         MVC   PNV1CLT,PNVHCLT        USE HEADER CLIENT                         
*                                                                               
         CLC   PNV1CLT,=C'***'     CLIENT CAN'T BE *** - VARIOUS                
         BE    NEWPSVX                                                          
*                                                                               
         MVC   PNV1PUB,PNVDPUB     SET PUB                                      
*                                                                               
         OC    PNV1PUB,PNV1PUB     IF PUB MISSING                               
         BNZ   *+10                                                             
         MVC   PNV1PUB,PNVHPUB        USE HEADER PUB                            
*                                                                               
         MVC   PNV1SER#,PNVKSER#   SET SER#                                     
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ PASSIVE                                 
*                                                                               
         CLC   PNV1KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   NWPSV1NF                                                         
*                                                                               
         CLC   PNV1DISK,QDISK         DONE IF DISK ADDR SAME                    
         BE    NWPSV1X                                                          
*                                                                               
         MVC   PNV1DISK,QDISK         ELSE SET NEW DISK ADDR                    
*                                                                               
         NI    PNV1CNTL,X'FF'-PNVCDELQ     FORCE NON-DELETED                    
*                                                                               
         GOTO1 WRITE                  RE-WRITE THE PASSIVE                      
*                                                                               
         B     NWPSV1X                                                          
*                                                                               
NWPSV1NF DS    0H                  PASSIVE NOT ON FILE                          
*                                                                               
         MVC   PNV1KEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   PNV1DISK,QDISK      SET DISK ADDRESS OF MASTER REC               
*                                                                               
         GOTO1 ADD                 ADD PASSIVE TO FILE                          
*                                                                               
NWPSV1X  DS    0H                                                               
*                                                                               
*        ADD B2 PASSIVE                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH B1 PASSIVE KEY                     
         USING PNV2KEY,R4                                                       
*                                                                               
         MVC   PNV2AGY,PNVKAGY     SET AGENCY                                   
         MVC   PNV2MED,PNVKMED     SET MEDIA                                    
         MVI   PNV2RCD,PNV2RCDQ    SET PASSIVE CODE                             
         MVC   PNV2CLT,PNVDCLT     SET CLIENT                                   
*                                                                               
         CLC   PNV2CLT,SPACES      IF NO CLIENT                                 
         BH    *+10                                                             
         MVC   PNV2CLT,PNVHCLT        USE HEADER CLIENT                         
*                                                                               
         MVC   PNV2PUB,PNVDPUB     SET PUB                                      
*                                                                               
         OC    PNV2PUB,PNV2PUB     IF PUB MISSING                               
         BNZ   *+10                                                             
         MVC   PNV2PUB,PNVHPUB        USE HEADER PUB                            
*                                                                               
         MVC   PNV2SDTE,PNVHSTRT   SET INVOICE START DATE                       
         MVC   PNV2EDTE,PNVHEND    SET INVOICE END   DATE                       
         MVC   PNV2SER#,PNVKSER#   SET SER#                                     
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ PASSIVE                                 
*                                                                               
         CLC   PNV2KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   NWPSV1NF                                                         
*                                                                               
         CLC   PNV2DISK,QDISK         DONE IF DISK ADDR SAME                    
         BE    NWPSV2X                                                          
*                                                                               
         MVC   PNV2DISK,QDISK         ELSE SET NEW DISK ADDR                    
*                                                                               
         NI    PNV2CNTL,X'FF'-PNVCDELQ     FORCE NON-DELETED                    
*                                                                               
         GOTO1 WRITE                  RE-WRITE THE PASSIVE                      
*                                                                               
         B     NWPSV2X                                                          
*                                                                               
NWPSV2NF DS    0H                  PASSIVE NOT ON FILE                          
*                                                                               
         MVC   PNV2KEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   PNV2DISK,QDISK      SET DISK ADDRESS OF MASTER REC               
*                                                                               
         GOTO1 ADD                 ADD PASSIVE TO FILE                          
*                                                                               
NWPSV2X  DS    0H                                                               
*                                                                               
         B     NWPSV3X             DO NOT ADD B3 PASSIVE                        
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH B3 PASSIVE KEY                     
         USING PNV3KEY,R4                                                       
*                                                                               
         MVC   PNV3AGY,PNVKAGY     SET AGENCY                                   
         MVC   PNV3MED,PNVKMED     SET MEDIA                                    
         MVI   PNV3RCD,PNV3RCDQ    SET PASSIVE CODE                             
         MVC   PNV3CLT,PNVDCLT     SET CLIENT                                   
*                                                                               
         CLC   PNV3CLT,SPACES      IF NO CLIENT                                 
         BH    *+10                                                             
         MVC   PNV3CLT,PNVHCLT        USE HEADER CLIENT                         
*                                                                               
         MVC   PNV3PBCD,PNVDPBCD   SET PUB                                      
*                                                                               
         OC    PNV3PBCD,PNV3PBCD   IF PUB MISSING                               
         BNZ   *+10                                                             
         MVC   PNV3PBCD,PNVHPUB       USE HEADER PUB                            
*                                                                               
         MVC   PNV3INV#,PNVHINV#   SET INVOICE NUMBER                           
*                                                                               
         GOTO1 HIGH                READ PASSIVE                                 
*                                                                               
         CLC   PNV3KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   NWPSV3NF                                                         
*                                                                               
         CLC   PNV3DISK,QDISK         DONE IF DISK ADDR SAME                    
         BE    NWPSV3X                                                          
*                                                                               
         MVC   PNV3DISK,QDISK         ELSE SET NEW DISK ADDR                    
*                                                                               
         NI    PNV3CNTL,X'FF'-PNVCDELQ     FORCE NON-DELETED                    
*                                                                               
         GOTO1 WRITE                  RE-WRITE THE PASSIVE                      
*                                                                               
         B     NWPSV3X                                                          
*                                                                               
NWPSV3NF DS    0H                  PASSIVE NOT ON FILE                          
*                                                                               
         MVC   PNV3KEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   PNV3DISK,QDISK      SET DISK ADDRESS OF MASTER REC               
*                                                                               
         GOTO1 ADD                 ADD PASSIVE TO FILE                          
*                                                                               
NWPSV3X  DS    0H                                                               
*                                                                               
         STC   R0,DMINBTS          RESTORE SETTING                              
*                                                                               
NEWPSVX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV20 - PRINT NEW INVOICE - Prisma invoice detail'            
*                                                                               
***********************************************************************         
*                                                                     *         
*        Prisma invoice detail (AIO has buy record)                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
PRSMINVD NTR1  BASE=*,LABEL=*      Set prisma invoice details                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         USING PNVDTLD,ELEMENT     ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT KEY                 
         USING PNVRECD,MINMKEY     ESTABLISH MASTER RECORD  KEY                 
*                                                                               
         CLI   SVINVSRC,INVPRM_Q   Prisma invoice upload?                       
         JE    *+12                                                             
         CLI   SVINVSRC,INVRAD_Q   Radia invoice upload?                        
         JNE   PRSMIVDX                                                         
*                                                                               
         OC    PNVDNET,PNVDNET     Have net amount?                             
         JNZ   PRSMIVDX                                                         
         OC    PNVDGRS,PNVDGRS     Have gross amount?                           
         JNZ   PRSMIVDX                                                         
         OC    PNVDRATE,PNVDRATE   Have invoice item rate?                      
         JNZ   PRSMIVDX                                                         
*                                                                               
         L     R4,AIO              ESTABLISH FOUND BUY                          
         USING PBUYREC,R4                                                       
         CLI   PBUYKRCD,X'20'      Buy record ID?                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBDELEM,X'20'       Buy description element?                     
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PNVD#LIN,1          Default to 1 line item for Prisma            
         MVC   PNVDSPC,PBDSPACE    Copy buy's space description                 
         MVC   PNVDCTPN,PBDCOSTY   Copy buy's cost type                         
         MVC   PNVDCSIN,PBDCOSIN   Copy buy's cost indicator                    
         MVC   PNVDCSTP,PBDCTYP    Copy buy's net input indicator               
*                                                                               
         XC    SVWRKELM,SVWRKELM   Temporary usage for GETINS block             
         USING PVALUES,SVWRKELM                                                 
         GOTOR VGETINS,DMCB,0(R4),PVALUES,(C'Y',7(R4)),(C'F',0)                 
*                                                                               
         ICM   RF,15,GROSS         Get binary gross                             
         CVD   RF,DUB                                                           
         ZAP   PNVDGRS,DUB         Copy gross amt to item as packed             
*                                                                               
         ICM   RF,15,PYABLE        Get binary payable                           
         A     RF,CSHDSC           Add cash discount to get net amt             
         CVD   RF,DUB                                                           
         ZAP   PNVDNET,DUB         Copy net amt to item as packed               
*                                                                               
         ZAP   PNVDRATE,PNVDGRS    Set item rate to gross                       
         CLI   PNVDCSTP,C'N'       Entered as net?                              
         JNE   *+10                                                             
         ZAP   PNVDRATE,PNVDNET    Set item rate to net                         
*                                                                               
PRSMIVDX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV20 - PRINT NEW INVOICE - Prisma invoice detail'            
*                                                                               
***********************************************************************         
*                                                                     *         
*        Prisma invoice detail (AIO has buy record)                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
CKPRMINV NTR1  BASE=*,LABEL=*      Check Prisma invoice requirements            
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         CLI   SVINVSRC,INVPRM_Q   Prisma invoice?                              
         JE    SETCCEQ                                                          
         CLI   SVINVSRC,INVRAD_Q   Radia invoice?                               
         JE    SETCCEQ                                                          
*                                                                               
         BRAS  RE,GETIDKPV         Get IDK profile values                       
         CLI   SVIDKPRF+03,C'Y'                                                 
         JNE   SETCCEQ                                                          
*                                                                               
         L     R4,AIO              Establish found buy                          
         MVI   ELCODE,PBYDKELQ     Prisma upload element code                   
         LR    R6,R4                                                            
         SR    R0,R0                                                            
         USING PBYDKELD,R6                                                      
         BRAS  RE,GETEL            Have Prisma upload element?                  
         JNE   SETCCEQ                                                          
         TM    PBYDKST4,BYPRMIVQ   Prisma invoice enabled campaign?             
         JNZ   SETCCNEQ                                                         
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
         LTORG                                                                  
         DROP                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*        Get IDK profile value                                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETIDKPV NTR1  BASE=*,LABEL=*      Get IDK profile value                        
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         XC    SVIDKPRF,SVIDKPRF   Prisma control profile values                
         XC    WORK,WORK                                                        
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          Make system lower case                       
         MVC   WORK+04(2),AGENCY                                                
         MVC   WORK+06(1),QMED                                                  
         MVC   WORK+07(3),QCLT                                                  
         CLI   SVCLTOFC,C' '                                                    
         JNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFC                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,DATAMGR                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - UPDACT'                
***********************************************************************         
*                                                                     *         
*        UPDATE ACTIVITY ELEMENT                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
UPDACT   NTR1  BASE=*,LABEL=*      UPDATE ACTIVITY ELEMENT                      
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING MINBLKD,R7                                                       
*                                                                               
         USING PNVDTLD,SVDTLELM    ESTABLISH DETAIL ELEMENT                     
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
         USING PNVRECD,MINMKEY     ESTABLISH MASTER RECORD                      
*                                                                               
         CLI   ACTNUM,ACTDEL       SKIP IF A DELETION                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT                                                  
         BE    UPDACT10                                                         
*                                                                               
         CLC   SVDTLOLD,SVDTLELM   DONE IF NO CHANGES TO DETAIL                 
         BE    UPDACTX                                                          
*                                                                               
UPDACT10 DS    0H                                                               
*                                                                               
**       MVC   PNVCPID,SVPNVPID                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(25,WORK)  GET TODAY/TIME                      
*                                                                               
         MVC   SVDATE,WORK         SAVE DATE                                    
         MVC   SVTIME,WORK+3       SAVE TIME AS BINARY                          
*                                                                               
*        FIND PRIOR ACTIVITY ELEMENT FOR DETAIL                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING PNVACTHD,ELEMENT    ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         MVI   PNVAKCDE,PNVAKDTQ   SET AS DETAIL ACTIVITY ELEMENT               
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY   SET TO FIND ANY ACT ELM              
         MVC   PNVAKDSQ,PNVDKSQN   SET DETAIL SEQ NUMBER                        
         MVI   PNVAKACT,PNVAKACQ   SET AS ACTIVITY ELEMENT                      
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT GET FIRST ACTIVITY ELEMENT                   
*                                                                               
UACTLOOP DS    0H                                                               
*                                                                               
         BNZ   UACTDONE            END OF ACTIVITY ELEMENTS                     
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVACTHD(0),0(R1)   MOVE ELEMENT TO WORK AREA                    
*                                                                               
UACTCONT DS    0H                                                               
*                                                                               
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY   FILTER ON ACTIVITY ELM               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  FIND NEXT ELEMENT                           
*                                                                               
         B     UACTLOOP                                                         
*                                                                               
UACTDONE DS    0H                                                               
*                                                                               
         MVI   ACTSW,C'N'          ASSUME NEW ACTIVITY ELEMENT                  
*                                                                               
         CLC   SVPNVPID,PNVAHPID   IF SAME PERSON                               
         BNE   *+10                                                             
         CLC   SVDATE,PNVAHDTE     AND SAME DATE                                
         BNE   *+12                                                             
         MVI   ACTSW,C'O'              FLAG AS OLD ELEMENT                      
         B     UACT10                                                           
*                                  ELSE                                         
         SR    RF,RF                                                            
         ICM   RF,3,PNVAKCSQ          BUMP THE SEQUENCE NUMBER                  
         AHI   RF,1                                                             
         STCM  RF,3,PNVAKCSQ                                                    
*                                                                               
         XC    PNVACHGS,PNVACHGS   CLEAR CHANGES INDICATORS                     
*                                                                               
UACT10   DS    0H                                                               
*                                                                               
         MVI   PNVAKLEN,PNVACTLQ   SET ELEMENT LENGTH                           
*                                                                               
         MVC   PNVAHPID,SVPNVPID   SET PID                                      
         MVC   PNVAHDTE,SVDATE     SET DATE                                     
*                                                                               
*        SET ACTION INDICATOR                                                   
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK FOR ADD                                
         BNE   *+12                                                             
         OI    PNVAHCH1,PNVADADD     SET INDICATOR                              
         B     UACTACTX                                                         
*                                                                               
         CLI   ACTNUM,ACTDEL       CHECK FOR DEL                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     CHECK FOR DEL MATCHED                        
         BNE   *+12                                                             
         OI    PNVAHCH1,PNVADDEL     SET INDICATOR                              
         B     UACTACTX                                                         
*                                                                               
         CLI   ACTNUM,ACTREST      CHECK FOR RESTORE                            
         BNE   *+12                                                             
         OI    PNVAHCH1,PNVADRES     SET INDICATOR                              
         B     UACTACTX                                                         
*                                                                               
UACTACTX DS    0H                                                               
*                                                                               
*        COMPARE FIELD BY FIELD FOR CHANGES                                     
*                                                                               
UACTCHG  DS    0H                                                               
*                                                                               
A        USING PNVDTLD,SVDTLOLD   ESTABLISH SECOND DETAIL ELM                   
*                                                                               
         CLI   ACTNUM,ACTDEL       CHECK FOR DEL                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     CHECK FOR DEL MATCHED                        
         BNE   *+12                                                             
         OI    PNVAHCH3,PNVADBS#      BUY SERIAL # CHANGED                      
         B     UACTCHGX                                                         
*                                                                               
         CLC   PNVDPUB,A.PNVDPUB   PUB CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH1,PNVADPUB      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDDTE,A.PNVDDTE   RUN DATE CHANGED?                            
         BE    *+8                                                              
         OI    PNVAHCH1,PNVADDTE      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDSPC,A.PNVDSPC   SPACE CHANGED?                               
         BE    *+8                                                              
         OI    PNVAHCH1,PNVADSPC      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDACAP,A.PNVDACAP CAPTION CHANGED?                             
         BNE   *+10                                                             
         CLC   PNVDACP2,A.PNVDACP2 CAPTION 2 CHANGED?                           
         BE    *+8                                                              
         OI    PNVAHCH1,PNVADCAP      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDNCL,A.PNVDNCL   # OF COLORS CHANGED?                         
         BE    *+8                                                              
         OI    PNVAHCH1,PNVADNCL      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDCDST,A.PNVDCDST CASH DISCOUNT STATUS                         
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADCD       YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDGST,A.PNVDGST   GST CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADGST      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDPST,A.PNVDPST   PST CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADPST      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDSREP,A.PNVDSREP REP CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADREP      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDRATE,A.PNVDRATE RATE CHANGED?                                
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADRTE      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDGRS,A.PNVDGRS   GROSS CHANGED?                               
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADGRS      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDNET,A.PNVDNET   NET CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADNET      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDPREM,A.PNVDPREM PREMIUM CHANGED?                             
         BE    *+8                                                              
         OI    PNVAHCH2,PNVADPRM      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDCLT,A.PNVDCLT   CLIENT CHANGED?                              
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADCLT      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDSER#,A.PNVDSER# BUY SER# CHANGED?                            
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADBS#      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDPRD,A.PNVDPRD   PRD CHANGED?                                 
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADPRD      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDEST,A.PNVDEST   ESTIMATE CHANGED?                            
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADEST      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDBYDT,A.PNVDBYDT BUY LINE DATE CHANGED?                       
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADBDT      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDLIN#,A.PNVDLIN# BUY LINE CHANGED?                            
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADBLN      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDAIMP,A.PNVDAIMP ACTUAL IMPRESSIONS CHANGED?                  
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADIMP      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDACPM,A.PNVDACPM ACTUAL CPMS CHANGED?                         
         BE    *+8                                                              
         OI    PNVAHCH3,PNVADCPM      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVD#LIN,A.PNVD#LIN NUMBER OF INSERTIONS                         
         BE    *+8                                                              
         OI    PNVAHCH4,PNVADLNS      YES - SET INDICATOR                       
*                                                                               
         CLC   PNVDDCM,A.PNVDDCM   DCOMM CODE CHANGED?                          
         BE    *+8                                                              
         OI    PNVAHCH4,PNVADDCM      YES - SET INDICATOR                       
*                                                                               
UACTCHGX DS    0H                                                               
*                                                                               
*        PUT ACTIVITY ELEMENT IN MINIO SET                                      
*                                                                               
         L     RF,ADDELM           ASSUMING THIS A NEW ELEMENT                  
*                                                                               
         CLI   ACTSW,C'O'          IF THIS AN OLD ELEMENT                       
         BNE   *+8                                                              
         L     RF,WRTELM              WRITE OLD ELEMENT                         
*                                                                               
         GOTOR (RF),DMCB,ELEMENT   PUT ELEMENT IN MINIO SET                     
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTNUM,ACTDLMAT     IF DELETING MATCHED                          
         BNE   UACTDMTX                                                         
*                                                                               
*        ADD ACTIVITY ELEMENT FOR NEW RNO                                       
*                                                                               
         MVC   PNVAKDSQ,SVDTLSQN   SET RNO SQN                                  
*                                                                               
         LHI   RF,1                                                             
         STCM  RF,3,PNVAKCSQ       SET TO FIRST ACTIVITY ELEMENT                
*                                                                               
         XC    PNVACHGS,PNVACHGS                                                
         OI    PNVAHCH1,PNVADADD   INDICATE DETAIL ADDED                        
*                                                                               
         GOTOR ADDELM,DMCB,ELEMENT ADD ACTIVITY ELEMENT                         
*                                                                               
UACTDMTX DS    0H                                                               
*                                                                               
UPDACTX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - DR'                        
***********************************************************************         
*                                                                     *         
*        DISPLAY  INVOICE DETAIL FIELDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DR       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        READ IN DETAIL ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET ELEMENT ID                               
         MVC   PNVDKSQN,QDSQN      SET SEQUENCE NUMBER                          
         MVI   PNVDKTYP,PNVDKDSQ   SET ELEMENT TYPE                             
*                                                                               
         GOTO1 GETELM,DMCB,PNVDKEY FIND ELEMENT                                 
         BZ    *+6                 ELEMENT FOUND                                
         DC    H'0'                MUST FIND IT                                 
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVDTLD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         MVC   DTLSER#+13(5),=C'(ADB)'                                          
         CLI   PNVDIVSR,INVIPS_Q                                                
         JNE   *+10                                                             
         MVC   DTLSER#+13(5),=C'(IPS)'                                          
         CLI   PNVDIVSR,INVPRM_Q                                                
         JNE   *+10                                                             
         MVC   DTLSER#+13(5),=C'(PRM)'                                          
         CLI   PNVDIVSR,INVRAD_Q                                                
         JNE   *+10                                                             
         MVC   DTLSER#+13(5),=C'(RAD)'                                          
         OI    DTLSER#H+6,FOUTTRN  Re-display serial# with source               
*                                                                               
*        DISPLAY RUN DATE                                                       
*                                                                               
DRDTE    DS    0H                                                               
*                                                                               
         LA    R2,DTLDTEH          POINT TO LINE ITEM DATE                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDDTE,PNVDDTE     SKIP IF NO DATE GIVEN                        
         BZ    DRDTEX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PNVDDTE),(17,FLDDATA)  DISP DATE                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRDTEX   DS    0H                                                               
*                                                                               
*        DISPLAY SPACE ENTRY                                                    
*                                                                               
DRSPC    DS    0H                                                               
*                                                                               
         LA    R2,DTLSPCH          POINT TO SPACE FIELD                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDSPC,PNVDSPC     SKIP IF NO SPACE GIVEN                       
         BZ    DRSPCX                                                           
*                                                                               
         MVC   FLDDATA(L'PNVDSPC),PNVDSPC DISPLAY SPACE                         
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRSPCX   DS    0H                                                               
*                                                                               
*        DISPLAY AD CAPTION                                                     
*                                                                               
DRACAP   DS    0H                                                               
*                                                                               
         LA    R2,DTLCAP1H         POINT TO AD CAPTIOND                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDACAP,PNVDACAP   SKIP IF NO AD CAPTION                        
         BZ    DRACAPX                                                          
*                                                                               
         MVC   FLDDATA(L'PNVDACAP),PNVDACAP DISPLAY AD CAPTION                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRACAPX  DS    0H                                                               
*                                                                               
DRACP2   DS    0H                                                               
*                                                                               
         LA    R2,DTLCAP2H         POINT TO AD CAP2 FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDACP2,PNVDACP2   SKIP IF NO AD CAP2                           
         BZ    DRACP2X                                                          
*                                                                               
         MVC   FLDDATA(L'PNVDACP2),PNVDACP2 DISPLAY AD CAP 2                    
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRACP2X  DS    0H                                                               
*                                                                               
*        DISPLAY NUMBER OF COLORS                                               
*                                                                               
DRNCL    DS    0H                                                               
*                                                                               
         LA    R2,DTLCLRH          # OF COLOURS FIELD                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDNCL,PNVDNCL     SKIP IF NO # CLRS                            
         BZ    DRNCLX                                                           
*                                                                               
         EDIT  PNVDNCL,DTLCLR,0,ALIGN=LEFT DISP # CLRS                          
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
         OI    FLDIIND,FINPNUM     INDICATE NUMERIC                             
*                                                                               
DRNCLX   DS    0H                                                               
*                                                                               
*        DISPLAY NUMBER OF LINES                                                
*                                                                               
DR#LN    DS    0H                                                               
*                                                                               
         LA    R2,DTLLNSH          # OF LINES FIELD                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVD#LIN,PNVD#LIN   SKIP IF NO # OF LINS                         
         BZ    DR#LNX                                                           
*                                                                               
         EDIT  PNVD#LIN,DTLLNS,0,ALIGN=LEFT DISP # LINES                        
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
         OI    FLDIIND,FINPNUM     INDICATE NUMERIC                             
*                                                                               
DR#LNX   DS    0H                                                               
*                                                                               
*        DISPLAY REP                                                            
*                                                                               
DRREP    DS    0H                                                               
*                                                                               
         LA    R2,DTLREPH          POINT TO REP FIELD                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDSREP,PNVDSREP   SKIP IF NO REP PRESENT                       
         BZ    DRREPX                                                           
*                                                                               
         MVC   QREP,PNVDSREP       SET REP CODE                                 
*                                                                               
         GOTOR DISREP                                                           
*                                                                               
         MVC   QREP,SVREP          RESTORE CURRENT MASTER REP                   
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRREPX   DS    0H                                                               
*                                                                               
*        DISPLAY CD                                                             
*                                                                               
DRCD     DS    0H                                                               
*                                                                               
         LA    R2,DTLCDH           POINT TO CD STATUS FLD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDCDST,PNVDCDST   SKIP IF NO CD                                
         BZ    DRCDX                                                            
*                                                                               
         MVC   FLDDATA(L'PNVDCDST),PNVDCDST DISPLAY CD STATUS                   
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRCDX    DS    0H                                                               
*                                                                               
*        DISPLAY GST                                                            
*                                                                               
DRGST    DS    0H                                                               
*                                                                               
         LA    R2,DTLGSTH          POINT TO GST FLD                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDGST,PNVDGST     SKIP IF NO GST                               
         BZ    DRGSTX                                                           
*                                                                               
         MVC   FLDDATA(L'PNVDGST),PNVDGST DISPLAY GST                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRGSTX   DS    0H                                                               
*                                                                               
*        DISPLAY PST                                                            
*                                                                               
DRPST    DS    0H                                                               
*                                                                               
         LA    R2,DTLPSTH          POINT TO PST FLD                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDPST,PNVDPST     SKIP IF NO PST                               
         BZ    DRPSTX                                                           
*                                                                               
         MVC   FLDDATA(L'PNVDPST),PNVDPST DISPLAY PST                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRPSTX   DS    0H                                                               
*                                                                               
*        DISPLAY RATE                                                           
*                                                                               
DRRATE   DS    0H                                                               
*                                                                               
         LA    R2,DTLRTEH          RATE FIELD                                   
*                                                                               
         BRAS  RE,FMTRTE           DISPLAY RATE                                 
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRRATEX  DS    0H                                                               
*                                                                               
*        DISPLAY PREMIUM                                                        
*                                                                               
DRPREM   DS    0H                                                               
*                                                                               
         LA    R2,DTLPREMH         PREMIUM FIELD                                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDPREM,PNVDPREM   SKIP IF NO PREM                              
         BNZ   *+12                                                             
         CLI   PNVDNCL,0           AND NO COLORS                                
         BE    DRPREMX                                                          
*                                                                               
         BRAS  RE,FMTPREM          DISPLAY PREMIUM                              
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRPREMX  DS    0H                                                               
*                                                                               
*        DISPLAY GROSS                                                          
*                                                                               
DRGRS    DS    0H                                                               
*                                                                               
         LA    R2,DTLGRSH          GROSS FIELD                                  
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDGRS,PNVDGRS     SKIP IF NO GROSS                             
         BZ    DRGRSX                                                           
*                                                                               
         EDIT  PNVDGRS,DTLGRS,2,ALIGN=LEFT DISP GROSS                           
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRGRSX   DS    0H                                                               
*                                                                               
*        DISPLAY NET                                                            
*                                                                               
DRNET    DS    0H                                                               
*                                                                               
         LA    R2,DTLNETH          NET FIELD                                    
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDNET,PNVDNET     SKIP IF NO NET                               
         BZ    DRNETX                                                           
*                                                                               
         EDIT  PNVDNET,DTLNET,2,ALIGN=LEFT DISP NET                             
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRNETX   DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM CLIENT                                               
*                                                                               
DRDCL    DS    0H                                                               
*                                                                               
         LA    R2,DTLDCLH          POINT TO DETAIL CLIENT                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
         BRAS  RE,BUMP             BUMP TO CLIENT NAME                          
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,DTLDCLH          POINT TO DETAIL CLIENT                       
*                                                                               
         OC    PNVDCLT,PNVDCLT     SKIP IF NO DETAIL CLIENT                     
         BZ    DRDCLX                                                           
*                                                                               
         MVC   SVCLT,QCLT          SAVE INVOICE CLIENT                          
         MVC   SVCLTNM,CLTNM       SAVE INVOICE CLIENT NAME                     
*                                                                               
         MVC   QCLT,PNVDCLT        SET TO DISPLAY LINE ITEM CLIENT              
*                                                                               
         GOTOR DISCLT              DISPLAY CLIENT CODE                          
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         MVC   QCLT,SVCLT          RESTORE INVOICE CLIENT                       
         MVC   CLTNM,SVCLTNM       RESTORE INVOICE CLIENT NAME                  
*                                                                               
DRDCLX   DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM BUY SERIAL NUMBER                                    
*                                                                               
DRSER#   DS    0H                                                               
*                                                                               
         LA    R2,DTLBS#H          POINT TO SERIAL NUMBER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF NO BUY SERIAL #                      
         BZ    DRSER#X                                                          
*                                                                               
         EDIT  PNVDSER#,DTLBS#,0,ALIGN=LEFT                                     
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
         OI    FLDIIND,FINPNUM     INDICATE NUMERIC                             
*                                                                               
DRSER#X  DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM PRODUCT                                              
*                                                                               
DRDPR    DS    0H                                                               
*                                                                               
         LA    R2,DTLDPRH          POINT TO DETAIL PRODUCT                      
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
         BRAS  RE,BUMP             BUMP TO PRODUCT NAME                         
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,DTLDPRH          POINT TO DETAIL PRODUCT                      
*                                                                               
         OC    PNVDPRD,PNVDPRD     SKIP IF NO DETAIL PRODUCT                    
         BZ    DRDPRX                                                           
*                                                                               
         MVC   SVCLT,QCLT          SAVE CURRENT MASTER CLIENT                   
*                                                                               
         OC    PNVDCLT,PNVDCLT     IF THERE IS A DETAIL CLIENT                  
         BZ    *+10                                                             
         MVC   QCLT,PNVDCLT           USE DETAIL CLIENT                         
*                                                                               
         MVC   QPRD,PNVDPRD        SET PRODUCT CODE                             
*                                                                               
         GOTOR DISPRD              DISPLAY CLIENT CODE                          
*                                                                               
         MVC   QCLT,SVCLT          RESTORE MASTER CLIENT                        
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRDPRX   DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM PUB                                                  
*                                                                               
DRDPB    DS    0H                                                               
*                                                                               
         LA    R2,DTLDPBH          POINT TO DETAIL PUB                          
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
         BRAS  RE,BUMP             BUMP TO PUB NAME                             
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,DTLDPBH          POINT TO DETAIL PUB                          
*                                                                               
         OC    PNVDPUB,PNVDPUB     SKIP IF NO PUB GIVEN                         
         BZ    DRDPBX                                                           
*                                                                               
         MVC   SVPUB,QPUB          SAVE MASTER PUB                              
         MVC   SVPUBNM,PUBNM       SAVE MASTER PUB NAME                         
*                                                                               
         MVC   QPUB,PNVDPUB        SET DETAIL PUB                               
*                                                                               
         GOTOR DISPUB              DISPLAY PUB CODE                             
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         MVC   QPUB,SVPUB          RESTORE MASTER PUB                           
         MVC   PUBNM,SVPUBNM       RESOTER MASTER PUB NAME                      
*                                                                               
DRDPBX   DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM ESTIMATE                                             
*                                                                               
DRDES    DS    0H                                                               
*                                                                               
         LA    R2,DTLDESH          POINT TO DETAIL ESTIMATE                     
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
         BRAS  RE,BUMP             BUMP TO ESTIMATE NAME                        
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,DTLDESH          POINT TO DETAIL ESTIMATE                     
*                                                                               
         OC    PNVDEST,PNVDEST     SKIP IF NO ESTIMATE GIVEN                    
         BZ    DRDESX                                                           
*                                                                               
         MVC   BEST,PNVDEST        SET ESTIMATE CODE                            
         SR    RF,RF                                                            
         ICM   RF,3,BEST           GET ESTIMATE NUMBER                          
         CVD   RF,DUB              CVB                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  QEST,DUB            MAKE CHARACTER                               
*                                                                               
         OC    PNVDCLT,PNVDCLT     IF THERE IS A DETAIL CLIENT                  
         BZ    *+10                                                             
         MVC   QCLT,PNVDCLT           USE DETAIL CLIENT                         
*                                                                               
         GOTOR DISEST              DISPLAY ESTIMATE CODE                        
*                                                                               
         MVC   SVCLT,QCLT          RESTORE CLIENT CODE                          
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRDESX   DS    0H                                                               
*                                                                               
*        DISPLAY INSERTION DATE                                                 
*                                                                               
DRDDT    DS    0H                                                               
*                                                                               
         LA    R2,DTLDDTH          POINT TO INSERTION DATE                      
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDBYDT,PNVDBYDT   SKIP IF NO INSERTION DATE                    
         BZ    DRDDTX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PNVDBYDT),(17,FLDDATA)                            
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRDDTX   DS    0H                                                               
*                                                                               
*        DISPLAY LINE ITEM LINE #                                               
*                                                                               
DRLIN#   DS    0H                                                               
*                                                                               
         LA    R2,DTLLINH          POINT TO LINE NUMBER                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDLIN#,PNVDLIN#  SKIP IF NO LINE NUMBER                        
         BZ    DRLIN#X                                                          
*                                                                               
         EDIT  PNVDLIN#,DTLLIN,0,ALIGN=LEFT DISPLAY LIN #                       
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
         OI    FLDIIND,FINPNUM     INDICATE NUMERIC                             
*                                                                               
DRLIN#X  DS    0H                                                               
*                                                                               
*        DISPLAY IMPRESSIONS                                                    
*                                                                               
DRIMPS   DS    0H                                                               
*                                                                               
         LA    R2,DTLIMPSH         POINT TO IMPS FIELD                          
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDAIMP,PNVDAIMP   SKIP IF NO IMPS                              
         BZ    DRIMPSX                                                          
*                                                                               
         EDIT  PNVDAIMP,DTLIMPS,0,ALIGN=LEFT DISPLAY IMPS                       
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRIMPSX  DS    0H                                                               
*                                                                               
*        DISPLAY COST PER THOUSAND                                              
*                                                                               
DRCPMS   DS    0H                                                               
*                                                                               
         LA    R2,DTLCPMSH         POINT TO CPMS FIELD                          
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDACPM,PNVDACPM   SKIP IF NO CPMS                              
         BZ    DRCPMSX                                                          
*                                                                               
         EDIT  PNVDACPM,DTLCPMS,2,ALIGN=LEFT DISPLAY CPMS                       
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRCPMSX  DS    0H                                                               
*                                                                               
*        DISPLAY DISCREPANCY COMMENT CODE                                       
*                                                                               
DRDCM    DS    0H                                                               
*                                                                               
         LA    R2,DTLDCMH          POINT TO DCOMM FIELD                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDDCM,PNVDDCM     SKIP IF NO CODE                              
         BZ    DRDCMX                                                           
*                                                                               
         MVC   8(L'PNVDDCM,R2),PNVDDCM  DISPLAY CODE                            
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DRDCMX   DS    0H                                                               
*                                                                               
*        CHECK IF TRANSFERRING TO PFM                                           
*                                                                               
         CLI   PFAID,9             IF PFKEY9                                    
         BE    *+8                                                              
         CLI   PFAID,21            OR 21                                        
         BNE   DRPFMX                                                           
*                                                                               
         GOTOR GOPFM                                                            
*                                                                               
DRPFMX   DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - FMTRTE'                    
***********************************************************************         
*                                                                     *         
*        DISPLAY  RATE ON SCREEN                                      *         
*                                                                     *         
*NTRY    R2==>  RATE FIELD                                            *         
*        R6==>  DETAIL ELEMENT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FMTRTE   NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING PNVDKEY,R6                                                       
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDRATE,PNVDRATE   DONE IF NO RATE                              
         BZ    FMTRTEX                                                          
*                                                                               
         CP    PNVDRATE,=P'0'      IF ZERO COST                                 
         BNZ   FMTRTE10                                                         
         MVC   FLDDATA(4),=C'FREE'    DISPLAY 'FREE'                            
         CLI   PNVDCSIN,C'S'       IF COST INDICATOR IS "S"                     
         BNE   FMTRTEX                                                          
         MVC   FLDDATA(5),=C'SFREE'   DISPLAY 'SFREE'                           
         B     FMTRTEX                DONE                                      
*                                                                               
FMTRTE10 DS    0H                                                               
         LA    R3,FLDDATA          FIRST POSITION OF OUTPUT                     
         SR    R4,R4                                                            
         IC    R4,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   R4,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   R4,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         MVC   BYTE,PNVDRDEC       SET NUMBER OF DECIMALS IN RATE               
*                                                                               
         OC    BYTE,BYTE           IF NONE GIVEN                                
         BNZ   *+8                                                              
         MVI   BYTE,2                 DEFAULT TO 2 DECIMAL PLACES               
*                                                                               
         CLI   QMED,C'N'           IF NEWSPAPER                                 
         BNE   FMRTTN                                                           
*****                                                                           
*****    CLI   PNVDCTPN,C'U'       IF UNIT RATE                                 
*****    BNE   *+8                                                              
*****    MVI   BYTE,5                 SET TO 5 DECIMAL PLACES                   
*                                                                               
         CLI   PNVDCTPN,C'T'       IF TOTAL RATE                                
         BNE   FMRTTN                                                           
*                                                                               
*****    MVI   BYTE,2                 SET TO 2 DECIMAL PLACES                   
*                                                                               
         MVC   0(1,R3),PNVDCTPN       DISPLAY IT                                
         LA    R3,1(R3)               BUMP OUTPUT POINTER                       
         BCTR  R4,0                   DECREMENT FIELD LENGTH                    
*                                                                               
FMRTTN   DS    0H                                                               
*                                                                               
         CLI   PNVDCSTP,C' '       IF COST TYPE PRESENT                         
         BNH   *+16                                                             
         MVC   0(1,R3),PNVDCSTP       DISPLAY IT                                
         LA    R3,1(R3)               BUMP OUTPUT POINTER                       
         BCTR  R4,0                   DECREMENT FIELD LENGTH                    
*                                                                               
         CLI   PNVDCSIN,C' '       IF COST INDICATOR PRESENT                    
         BNH   *+16                                                             
         MVC   0(1,R3),PNVDCSIN       DISPLAY IT                                
         LA    R3,1(R3)               BUMP OUTPUT POINTER                       
         BCTR  R4,0                   DECREMENT FIELD LENGTH                    
*                                                                               
         CLI   BYTE,5              IF 5 DECIMAL PLACES                          
         BNE   FMTRTE70                                                         
*                                                                               
         EDIT  PNVDRATE,(17,WORK),5,ALIGN=LEFT,FLOAT=-,COMMAS=YES               
*                                                                               
         B     FMTRTE80                                                         
*                                                                               
FMTRTE70 DS    0H                  ELSE USE 2 DECIMAL PLACES                    
*                                                                               
         EDIT  PNVDRATE,(17,WORK),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES               
*                                                                               
FMTRTE80 DS    0H                                                               
*                                                                               
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY RATE                                 
*                                                                               
FMTRTEX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - FMTPREM'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  PREMIUM ON SCREEN                                   *         
*                                                                     *         
*NTRY    R2==>  PREMIUM FIELD                                         *         
*        R6==>  DETAIL ELEMENT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FMTPREM  NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING PNVDKEY,R6                                                       
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         OC    PNVDPREM,PNVDPREM   DONE IF NO PREMIUM                           
         BNZ   *+12                                                             
         CLI   PNVDNCL,0           AND NO COLORS                                
         BE    FMTPREMX                                                         
*                                                                               
         OC    PNVDPREM,PNVDPREM   SKIP IF NO PREMIUM                           
         BZ    FMPRFREX                                                         
*                                                                               
         CP    PNVDPREM,=P'0'      IF ZERO COST                                 
         BNZ   *+14                                                             
         MVC   FLDDATA(4),=C'FREE'    DISPLAY 'FREE'                            
         B     FMTPREMX               DONE                                      
*                                                                               
FMPRFREX DS    0H                                                               
*                                                                               
         LA    R3,FLDDATA          FIRST POSITION OF OUTPUT                     
         SR    R4,R4                                                            
         IC    R4,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   R4,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   R4,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         CLI   PNVDNCL,0           IF COLORS GIVEN                              
         BE    FMPRCLRX                                                         
*                                                                               
         MVC   0(1,R3),PNVDNCL        DISPLAY NUMBER OF COLORS                  
         OI    0(R3),X'F0'            FILL IN ZONE                              
         MVI   1(R3),C'C'             INDICATE COLORS                           
*                                                                               
         LA    R3,2(R3)               BUMP OUTPUT POINTER                       
         SHI   R4,2                   DECREMENT AVAILABLE LENGTH                
*                                                                               
         OC    PNVDPREM,PNVDPREM      DONE IF NO PREMIUM COST                   
         BZ    FMTPREMX                                                         
*                                                                               
         MVI   0(R3),C'/'                DISPLAY C'/'                           
*                                                                               
         LA    R3,1(R3)                  BUMP OUTPUT POINTER                    
         BCTR  R4,0                      DECREMENT LENGTH COUNTER               
*                                                                               
FMPRCLRX DS    0H                                                               
*                                                                               
         CLI   PNVDPRTP,C' '       IF COST TYPE PRESENT                         
         BNH   *+16                                                             
         MVC   0(1,R3),PNVDPRTP       DISPLAY IT                                
         LA    R3,1(R3)               BUMP OUTPUT POINTER                       
         BCTR  R4,0                   DECREMENT FIELD LENGTH                    
*                                                                               
         CLI   PNVDPRIN,C' '       IF COST INDICATOR PRESENT                    
         BNH   *+16                                                             
         MVC   0(1,R3),PNVDPRIN       DISPLAY IT                                
         LA    R3,1(R3)               BUMP OUTPUT POINTER                       
         BCTR  R4,0                   DECREMENT FIELD LENGTH                    
*                                                                               
         OC    PNVDPREM,PNVDPREM   SKIP IF NO PREMIUM                           
         BZ    FMTPREMX               (SHOULD NEVER HAPPEN)                     
*                                                                               
         EDIT  PNVDPREM,(17,WORK),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES               
*                                                                               
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY PREMIUM                              
*                                                                               
FMTPREMX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - DK'                        
***********************************************************************         
*                                                                     *         
*        DISPLAY  KEY FROM SELECTION ON LIST SCREEN                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),KEY    SET MASTER KEY                   
*                                                                               
         USING PNVRECD,MINMKEY     ESTABLISH MASTER KEY                         
*                                                                               
*        ACTION UNLINK NEEDS TO BUILD MASTER KEY FROM SCRATCH                   
*                                                                               
         CLI   ACTNUM,ACTUNLNK     IF ACTION UNLINK                             
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED                     
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   DKBLDKYX                                                         
*                                                                               
         XC    PNVKEY,PNVKEY       INIT KEY                                     
*                                                                               
         MVC   PNVKAGY,QAGY        SET AGENCY CODE                              
         MVC   PNVKMED,QMED        SET MEDIA                                    
         MVI   PNVKRCD,PNVKRCDQ    SET RECORD CODE                              
         MVC   PNVKSER#,QSER#      SET INVOICE SERIAL NUMBER                    
*                                                                               
DKBLDKYX DS    0H                                                               
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        READ IN HEADER ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING PNVHKEY,R6                                                       
*                                                                               
         MVI   PNVHKCDE,PNVHKIDQ   SET HEADER ELM CODE                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVHDRELM,0(R1)      SAVE HEADER ELEMENT                          
*                                                                               
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
*                                                                               
*        READ IN DETAIL ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVDKEY,R6                                                       
*                                                                               
         CLI   ACTNUM,ACTUNLNK     IF ACTION UNLINK                             
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED                     
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   DKDKY10                                                          
*                                                                               
         XC    PNVDKEY,PNVDKEY     INIT DETAIL ELEMENT KEY                      
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET DETAIL ELM CODE                          
         MVC   PNVDKSQN,QDSQN      SET SEQUENCE NUMBER                          
         MVI   PNVDKTYP,PNVDKDSQ   SET FOR DETAIL DESCRIPTION                   
*                                                                               
         B     DKDKYX                                                           
*                                                                               
DKDKY10  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SELLISTN       GET RELATIVE NUMBER IN LIST                  
         MHI   RF,L'SVLSTDTL       CALCULATE INDEX                              
         LA    RF,SVLSTDTL(RF)     INDEX TO ENTRY  IN TABLE                     
*                                                                               
         MVC   PNVDKEY,0(RF)       SET DETAIL KEY FOR SELECTION                 
*                                                                               
DKDKYX   DS    0H                                                               
*                                                                               
         GOTO1 GETELM,DMCB,PNVDKEY FIND ELEMENT                                 
         BZ    *+6                 ELEMENT FOUND                                
         DC    H'0'                MUST FIND IT                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   SVDTLELM,0(R1)      MOVE ELEMENT TO WORK AREA                    
*                                                                               
         USING PNVDTLD,SVDTLELM    ESTABLISH DETAIL ELEMENT                     
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         LA    R2,DTLMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QMED,PNVKMED        SET MEDIA                                    
*                                                                               
         GOTOR DISMED              DISPLAY MEDIA                                
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         LA    R2,DTLCLTH          POINT TO CLIENT FIELD                        
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QCLT,PNVHCLT        SET CLIENT                                   
*                                                                               
         GOTOR DISCLT              DISPLAY CLIENT                               
*                                                                               
*        SET MASTER PUB ON SCREEN                                               
*                                                                               
         LA    R2,DTLPUBH          POINT TO PUB FIELD                           
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QPUB,PNVHPUB        SET MASTER PUB                               
*                                                                               
         GOTOR DISPUB              DISPLAY PUB CODE                             
*                                                                               
*        SET INVOICE NUMBER ON SCREEN                                           
*                                                                               
         LA    R2,DTLINVH          POINT TO INVOICE # FIELD                     
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   FLDDATA(L'PNVHINV#),PNVHINV# DISPLAY INVOICE NUMBER              
         MVI   FLDILEN,L'PNVHINV#  SET INPUT LENGTH                             
         MVI   FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
*                                                                               
*        SET INVOICE NUMBER ON SCREEN                                           
*                                                                               
         LA    R2,DTLINVH          POINT TO INVOICE # FIELD                     
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         MVC   FLDDATA(L'PNVHINV#),PNVHINV# DISPLAY INVOICE NUMBER              
         MVI   FLDILEN,11          SET INPUT LENGTH                             
         MVI   FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
*                                                                               
*        SET DETAIL SEQUENCE NUMBER ON SCREEN                                   
*                                                                               
         LA    R2,DTLDTL#H         POINT TO DETAIL SEQ NUMBER FIELD             
*                                                                               
         EDIT  PNVDKSQN,DTLDTL#,0,ALIGN=LEFT                                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         STC   R0,FLDILEN          LENGTH OF INPUT                              
         OI    FLDIIND,FINPNUM     INPUT IS NUMERIC                             
*                                                                               
         CLI   ACTNUM,ACTUNLNK     SKIP IF ACTION UNLINK                        
         BE    *+8                                                              
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTDLMAT     OR ACTION DELETE MATCHED LINE ITEM           
         BE    *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - DL'                        
***********************************************************************         
*                                                                     *         
*        DELETE A DETAIL                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DL       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR TSTLOK              MAKE SURE CLIENT IS NOT LOCKED               
         BNE   DLBYLKER            CLIENT LOCKED                                
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        IF DELETING MATCHED ITEM, FIND LAST DETAIL ELEMENT                     
*                                                                               
         CLI   ACTNUM,ACTDLMAT     SKIP IF NOT DELETING MATCHED ITEM            
         BNE   DLMTCX                                                           
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET FOR DETAIL ELEMENTS                      
         MVI   PNVDKLEN,1          FILTER ON ELEMENT TYPE                       
*                                                                               
         XC    SVDTLSQN,SVDTLSQN   INIT DETAIL SQN SAVEAREA                     
*                                                                               
         GOTO1 GETELM,DMCB,PNVDKEY FIND FIRST ELEMENT                           
*                                                                               
DLMTCLP  DS    0H                                                               
*                                                                               
         BNZ   DLMTCDN             END OF DETAIL ELEMENTS                       
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   IF DETAIL DESCRIPTION ELEMENT                
         BNE   DLMTCCN                                                          
*                                                                               
         MVC   SVDTLSQN,PNVDKSQN      SAVE SEQUENCE NUMBER                      
*                                                                               
DLMTCCN  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  FIND NEXT ELEMENT                           
*                                                                               
         B     DLMTCLP                                                          
*                                                                               
DLMTCDN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,SVDTLSQN       BUMP LAST SQN BY 1                           
         AHI   RF,1                                                             
         STCM  RF,3,SVDTLSQN       SET AS NEXT AVAILABLE SQN                    
*                                                                               
DLMTCX   DS    0H                                                               
*                                                                               
*        READ IN ALL RELATED DETAIL ELEMENTS AND FLAG FOR DELETE                
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET ELEMENT ID                               
         MVC   PNVDKSQN,QDSQN      SET SEQUENCE NUMBER                          
         MVI   PNVDKLEN,PNVDKTYP-PNVDKEY   FILTER ON DETAIL SQN                 
*                                                                               
         MVI   SVCOMSQN,0          INIT COMMENT SQN SAVEAREA                    
*                                                                               
         GOTO1 GETELM,DMCB,PNVDKEY FIND FIRST ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING PNVDKEY,SVDTLELM    ESTABLISH DETAIL ELEMENT                     
*                                                                               
DLDTLLP  DS    0H                                                               
*                                                                               
         XC    SVDTLELM,SVDTLELM   INIT ELEMENT SAVEAREA                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,15,MINELEM       POINT TO FOUND ELEMENT                       
         BZ    DLDTLDN             NONE FOUND                                   
*                                                                               
         OC    0(L'PNVDKEY,R1),0(R1) DONE IF NO ELEMENT                         
         BZ    DLDTLDN                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVDTLD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         CLI   SVINVSRC,INVPRM_Q    Prisma invoice?                             
         JE    DLDTLL10                                                         
         CLI   SVINVSRC,INVRAD_Q    Radia invoice?                              
         JE    DLDTLL10                                                         
         CLI   PNVDIVSR,INVPRM_Q    Prisma invoice?                             
         JE    *+12                                                             
         CLI   PNVDIVSR,INVRAD_Q    Radia invoice?                              
         JNE   DLDTLL10                                                         
         LHI   RF,PPPRSMER                                                      
         J     VRERR                                                            
*                                                                               
DLDTLL10 DS    0H                                                               
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   MUST BE A DETAIL ELEMENT                     
         BNE   DLDTLDN             END OF DETAIL ELEMENTS                       
*                                                                               
         CLC   PNVDKSQN,QDSQN      MUST MATCH DETAIL SEQUENCE NUMBER            
         BNE   DLDTLDN             END OF DETAIL ELEMENTS                       
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   IF DETAIL DESCRIPTION ELEMENT                
         BNE   DLDTLN                                                           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVDTLOLD(0),0(R1)   SAVE FOR ACTIVITY ANALYSIS                   
*                                                                               
         OI    PNVDSTAT,PNVDDLQ       FLAG AS DELETED                           
*                                                                               
         CLI   ACTNUM,ACTDLMAT     IF ACTION DELETE MATCHED ITEM                
         BNE   *+8                                                              
         OI    PNVDSTAT,PNVDMTQ       FLAG AS MATCHED                           
*                                                                               
         GOTOR WRTELM,DMCB,PNVDKEY RE-WRITE ELEMENT                             
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTNUM,ACTDLMAT     SKIP IF NOT DELETING MATCHED ITEM            
         BNE   DLDTLDTX                                                         
*                                                                               
*        ADD RNO WITH SAME DESRIPTION                                           
*                                                                               
         NI    PNVDSTAT,X'FF'-PNVDDLQ-PNVDMTQ UNDELETE & UNMATCH                
         XC    PNVDSER#,PNVDSER#   KILL BUY SERIAL NUMBER                       
*                                  MAKES THIS AN RNO                            
         ICM   R0,3,PNVDKSQN       SAVE CURRENT DETAIL SQN                      
*                                                                               
         MVC   PNVDKSQN,SVDTLSQN   SET WITH NEXT AVAILABLE SQN                  
*                                                                               
         GOTOR ADDELM,DMCB,PNVDKEY ADD NEW LINE ITEM                            
*                                                                               
         STCM  R0,3,PNVDKSQN       RESTORE CURRENT SQN                          
*                                                                               
DLDTLDTX DS    0H                                                               
*                                                                               
         B     DLDTL20                                                          
*                                                                               
DLDTLN   DS    0H                                                               
*                                                                               
         USING PNVCKEY,SVDTLELM    ESTABLISH COMMENT ELEMENT                    
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   IF DETAIL COMMENT     ELEMENT                
         BE    *+8                                                              
         CLI   PNVCKTYP,PNVCKPBQ   IF DETAIL COMMENT     ELEMENT                
         BNE   DLCOMN                                                           
*                                                                               
         OI    PNVCSTAT,PNVCDLQ       FLAG AS DELETED                           
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   IF GENERAL COMMENT                           
         BNE   *+10                                                             
         MVC   SVCOMSQN,PNVCKCGP      SAVE GROUP SEQUENCE NUMBER                
*                                                                               
         GOTOR WRTELM,DMCB,PNVCKEY RE-WRITE ELEMENT                             
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTNUM,ACTDLMAT     SKIP IF NOT DELETING MATCHED ITEM            
         BNE   DLDTLCMX                                                         
*                                                                               
         NI    PNVCSTAT,X'FF'-PNVCDLQ UNDELETE COMMENT                          
*                                                                               
         ICM   R0,3,PNVCKDSQ       SAVE SQN                                     
         MVC   PNVCKDSQ,SVDTLSQN   SET TO NEXT AVAILABLE SQN                    
*                                                                               
         GOTOR ADDELM,DMCB,PNVCKEY ADD COMMENT                                  
*                                                                               
         STCM  R0,3,PNVCKDSQ       RESTORE SQN                                  
*                                                                               
DLDTLCMX DS    0H                                                               
*                                                                               
         B     DLDTL20                                                          
*                                                                               
DLCOMN   DS    0H                                                               
*                                                                               
         USING PNVAKEY,SVDTLELM    ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         CLI   PNVAKACT,PNVAKACQ   IF DETAIL ACTIVITY    ELEMENT                
         BNE   DLACTN                                                           
*                                                                               
         OI    PNVASTAT,PNVADLQ       FLAG AS DELETED                           
*                                                                               
         CLI   ACTNUM,ACTDLMAT     IF ACTION DELETE MATCHED ITEM                
         BNE   *+8                                                              
         OI    PNVASTAT,PNVAMTQ       FLAG AS MATCHED WHEN DELETED              
*                                                                               
         GOTOR WRTELM,DMCB,SVDTLELM RE-WRITE ELEMENT                            
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DLDTL20                                                          
*                                                                               
DLACTN   DS    0H                                                               
*                                                                               
         B     DLDTLCN                                                          
*                                                                               
DLDTL20  DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,PNVDKEY  RE-SET   FILE POINTERS                      
*                                                                               
*        CLI   MINERR,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVDTLD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
DLDTLCN  DS    0H                                                               
*                                                                               
         MVI   PNVDKLEN,3          FILTER ON SQN NUMBER                         
*                                                                               
         GOTOR NXTELM,DMCB,PNVDKEY  FIND NEXT ELEMENT                           
*                                                                               
         B     DLDTLLP                                                          
*                                                                               
DLDTLDN  DS    0H                                                               
*                                                                               
         BRAS  RE,UPDACT           UPDATE ACTIVITY ELEMENTS                     
*                                                                               
*        ADD COMMENT TO RNO SHOWING WHO DELETED THIS LINE ITEM                  
*                                                                               
         CLI   ACTNUM,ACTDLMAT     SKIP IF NOT DELETING MATCHED ITEM            
         BNE   DLDMCMX                                                          
*                                                                               
         XC    SVDTLELM,SVDTLELM   INIT ELM WORKAREA                            
*                                                                               
         MVI   PNVCKCDE,PNVCKDTQ   DETAIL COMMENT                               
         MVI   PNVCKLEN,PNVCOMLQ   MINIMUM LENGTH TO START                      
         MVC   PNVCKDSQ,SVDTLSQN   SET RNO SEQUENCE NUMBER                      
         MVI   PNVCKTYP,PNVCKCMQ   DETAIL COMMENT                               
         SR    RF,RF                                                            
         IC    RF,SVCOMSQN         BUMP COMMENT SEQUENCE NUUMBER                
         AHI   RF,1                                                             
         STC   RF,PNVCKCGP         GROUP SEQUENCE NUMBER                        
         MVI   PNVCKCSQ,1          FIRST COMMENT OF GROUP                       
*                                                                               
         MVC   PNVCPID,SVPNVPID    SET PID                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(25,WORK)  GET TODAY/TIME                      
*                                                                               
         MVC   SVDATE,WORK         SAVE DATE                                    
         MVC   SVTIME,WORK+3       SAVE TIME AS BINARY                          
*                                                                               
         MVC   PNVCDATE,SVDATE     SET DATE                                     
         MVC   PNVCTIME,SVTIME     SET TIME                                     
*                                                                               
         MVC   PNVCCOM(L'RNOMSG1),RNOMSG1 START OF RNO MSG                      
         LA    R3,PNVCCOM+L'RNOMSG1 START OF NEXT PART OF MSG                   
*                                                                               
         SR    R2,R2               MAKE SURE NO SCREEN FIELD POINTED TO         
         GOTOR TRNPID,DMCB,(0,SVPNVPID),(40,0(R3))      USER NAME               
*                                                                               
         LA    R3,40(R3)           FIND END OF NAME                             
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+12                                                             
         MVI   0(R3),X'00'         SET TO NULLS                                 
         BCT   R3,*-12                                                          
*                                                                               
         MVC   1(4,R3),=C' on '                                                 
         LA    R3,5(R3)            BUMP POINTER                                 
*                                                                               
         GOTOR DATCON,DMCB,(X'43',SVDATE),(13,0(R3))  DATE                      
*                                                                               
         LA    R3,8(R3)                                                         
         MVC   0(4,R3),=C' at '                                                 
         LA    R3,4(R3)                                                         
*                                                                               
         SR    RF,RF               DISPLAY TIME                                 
         SR    RE,RE                                                            
*                                                                               
         IC    RF,SVTIME           GET HRS IN BINARY                            
         AHI   RF,6                ADJUST DDS HRS TO MILITARY HRS               
*                                                                               
         MHI   RF,100              SHIFT DIGITS LEFT                            
*                                                                               
         IC    RE,PNVCTIME+1       GET MINUTES                                  
         AR    RF,RE               MILITARY TIME                                
*                                                                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(4,R3),DUB         DISPLAY TIME                                 
*                                                                               
         LA    R3,4(R3)            NEXT AVAILABLE POSITION                      
*                                                                               
         MVC   0(2,R3),=C'. '      END OF FIRST SENTENCE                        
         LA    R3,2(R3)                                                         
*                                                                               
         LA    RF,PNVCKEY          CALCULATE ELEMENT LENGTH                     
         SR    R3,RF                                                            
*                                                                               
         STC   R3,PNVCKLEN         SET ELEMENT LENGTH                           
*                                                                               
         GOTOR ADDELM,DMCB,PNVCKEY ADD ELEMENT TO RECORD                        
         BE    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
*        FORMAT SECOND PART OF MESSAGE                                          
*                                                                               
         XC    PNVCCOM(256-PNVCOMLQ),PNVCCOM  CLEAR 1ST PART OF MSG             
*                                                                               
         MVI   PNVCKCSQ,2          SECOND LINE OF MSG                           
         LA    R3,PNVCCOM          START OF MESSAGE                             
*                                                                               
         MVC   0(L'RNOMSG2,R3),RNOMSG2 REST OF MESSAGE                          
*                                                                               
         LA    R3,L'RNOMSG2(R3)    BUMP POINTER                                 
*                                                                               
         LA    RF,PNVCKEY          CALCULATE ELEMENT LENGTH                     
         SR    R3,RF                                                            
*                                                                               
         STC   R3,PNVCKLEN         SET ELEMENT LENGTH                           
*                                                                               
         GOTOR ADDELM,DMCB,PNVCKEY ADD ELEMENT TO RECORD                        
*                                                                               
         B     DLDMCMX                                                          
*                                                                               
RNOMSG1  DC    C'This RNO represents a cleared insertion that was previx        
               ously on the invoice, but was deleted by '                       
RNOMSG2  DC    C'The RNO was created and any comments from the old invox        
               ice item were copied for informational purposes. '               
*                                                                               
DLDMCMX  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        DELETE LINKAGE ELEMENT FROM THE BUY                                    
*                                                                               
         CLI   ACTNUM,ACTDLMAT     SKIP IF DELETING MATCHED ELM                 
         BE    DLSER#X                                                          
*                                                                               
         MVC   SVDTLELM,SVDTLOLD   RESTORE LINE ITEM ELEMENT                    
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF LINE ITEM NOT LINKED                 
         BZ    DLSER#X                                                          
*                                                                               
*        READ BUY RECORD                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH BUY SER# PASSIVE                   
         USING PSERKEY,R4                                                       
*                                                                               
         MVC   PSERKAGY,QAGY       SET AGENCY                                   
         MVC   PSERKMED,QMED       SET MEDIA                                    
         MVI   PSERKRCD,PSERKIDQ   SET RECORD CODE                              
         MVC   PSERKCLT,PNVDCLT    SET CLIENT                                   
*                                                                               
         ZAP   DUB,=P'1000000000'  COMPLEMENT SER#                              
         SP    DUB,PNVDSER#                                                     
         ZAP   PSERKNUM,DUB        SET SERIAL NUMBER                            
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     SKIP IF NOT FOUND                            
         BNE   DLSER#X                                                          
*                                                                               
*        DELETE POINTER FROM OLD BUY                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
*                                                                               
         GOTOR GETREC              READ IN BUY                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND BUY RECORD                    
         USING PBUYREC,R4          ESTABLISH BUY RECORD                         
*                                                                               
*        DELETE OLD POINTER ELEMENT                                             
*                                                                               
         MVI   ELCODE,PBNVELQ      SET ELEMENT CODE                             
         LR    R6,R4               POINT TO START OF BUY RECORD                 
*                                                                               
         SR    R0,R0               INIT INVOICE ELEMENT POINTER                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST INVOICE POINTER                   
*                                                                               
DLBYDLLP DS    0H                                                               
*                                                                               
         BNZ   DLBYDLDN            NO MORE ELEMENTS                             
*                                                                               
         USING PBNVELMD,R6         ESTABLISH INVOICE POINTER                    
*                                                                               
         AHI   R0,1                INCREMENT INVOICE ELEMENT COUNTER            
*                                                                               
         CLC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY FIND OLD PTR                    
         BNE   DLBYDLCN                                                         
         CLC   PBNVDSQN,PNVDKSQN   AND SAME DETAIL SQN                          
         BNE   DLBYDLCN                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),(R6),0   DELETE ELEMENT                 
*                                                                               
DLBYDLCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT INVOICE PTR ELM                    
*                                                                               
         B     DLBYDLLP                                                         
*                                                                               
DLBYDLDN DS    0H                                                               
*                                                                               
         CHI   R0,1                SKIP IF THERE ARE SEVERAL INV ELMS           
         BH    DLBYMATN                                                         
*                                                                               
*        REMOVE MATCHED STATUS FROM BUY                                         
*                                                                               
         NI    PBDSTAT,X'FF'-X'40' NO LONGER MATCHED                            
*                                                                               
*        REMOVE TEARSHEET INDICATOR FROM BUY                                    
*                                                                               
         NI    PBDSTAT,X'FF'-X'10' NO LONGER TEARSHEET FROM INV                 
*                                                                               
*        STOP UPDATING MATCH STATUS ELEMENT - AB DOES IT                        
*                                                                               
         B     DLBYMATN                                                         
*                                                                               
*        REMOVE MATCHING STATUS ELEMENT                                         
*                                                                               
         MVI   ELCODE,PBMATELQ     SET ELEMENT CODE                             
         LR    R6,R4               POINT TO START OF BUY RECORD                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST INVOICE POINTER                   
*                                                                               
         BNZ   DLBYMATN            NO MORE ELEMENTS                             
*                                                                               
         USING PBMATELD,R6         ESTABLISH MATCH STATUS ELM                   
*                                                                               
         MVI   PBMTSTAT,PBMTSNIQ   NO INVOICE                                   
         MVI   PBMTDSTA,PBMTDNAQ   NOT APPLICABLE                               
*                                                                               
DLBYMATN DS    0H                                                               
*                                                                               
         GOTO1 PUTREC              WRITE BUY BACK TO FILE                       
*                                                                               
DLBYDLX  DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDLMAT     IF DELETING MATCHED LINE ITEM                
         BNE   *+8                                                              
         BRAS  RE,DR                  RE-DISPLAY RECORD                         
*                                                                               
DLSER#X  DS    0H                                                               
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   DLLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
DLLNKX   DS    0H                                                               
*                                                                               
DLX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - DLERR '                
***********************************************************************         
*                                                                     *         
*        DELREC ERROR MESSAGES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DLBYLKER LHI   RF,PPELOCKD                                                      
         J     DLERR                                                            
*                                                                               
DLERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - RS'                        
***********************************************************************         
*                                                                     *         
*        RESTORE A DETAIL                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RS       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR TSTLOK              MAKE SURE CLIENT IS NOT LOCKED               
         BNE   RSBYLKER            CLIENT LOCKED                                
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SETS                      
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        READ IN ALL RELATED DETAIL ELEMENTS AND FLAG FOR DELETE                
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH DETAIL ELEMENT KEY                 
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET ELEMENT ID                               
         MVC   PNVDKSQN,QDSQN      SET SEQUENCE NUMBER                          
         MVI   PNVDKLEN,PNVDKTYP-PNVDKEY   FILTER ON DETAIL SQN                 
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING PNVDKEY,SVDTLELM    ESTABLISH DETAIL ELEMENT                     
*                                                                               
RSDTLLP  DS    0H                                                               
*                                                                               
         BNZ   RSDTLDN             END OF DETAIL ELEMENTS                       
*                                                                               
         L     R1,MINELEM          POINT TO FOUND ELEMENT                       
         SR    RF,RF                                                            
         IC    RF,1(R1)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVDTLD(0),0(R1)    MOVE ELEMENT TO WORK AREA                    
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   IF DETAIL DESCRIPTION ELEMENT                
         BNE   RSDTLN                                                           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVDTLOLD(0),0(R1)   SAVE FOR ACTIVITY ANALYSIS                   
*                                                                               
         NI    PNVDSTAT,X'FF'-PNVDDLQ     UNDELETE                              
         B     RSDTL10                                                          
*                                                                               
RSDTLN   DS    0H                                                               
*                                                                               
         USING PNVCKEY,SVDTLELM    ESTABLISH COMMENT ELEMENT                    
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   IF DETAIL COMMENT     ELEMENT                
         BE    RSCOM                                                            
         CLI   PNVCKTYP,PNVCKPBQ   IF DETAIL COMMENT     ELEMENT                
         BNE   RSCOMN                                                           
*                                                                               
RSCOM    DS    0H                                                               
*                                                                               
         NI    PNVCSTAT,X'FF'-PNVCDLQ     UNDELETE                              
         MVC   SVWRKELM,SVDTLELM   SAVE DETAIL ELEMENT FOR LATER                
         B     RSDTL10                                                          
*                                                                               
RSCOMN   DS    0H                                                               
*                                                                               
         USING PNVAKEY,SVDTLELM    ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         CLI   PNVAKACT,PNVAKACQ   IF DETAIL ACTIVITY    ELEMENT                
         BNE   *+12                                                             
         NI    PNVASTAT,X'FF'-PNVADLQ     UNDELETE                              
         B     RSDTL10                                                          
*                                                                               
         B     RSDTLCN                                                          
*                                                                               
RSDTL10  DS    0H                                                               
*                                                                               
         GOTOR WRTELM,DMCB,SVDTLELM RE-WRITE ELEMENT                            
*                                                                               
         GOTOR GETELM,DMCB,SVDTLELM RE-SET   FILE POINTERS                      
*                                                                               
RSDTLCN  DS    0H                                                               
*                                                                               
         MVI   PNVDKLEN,PNVDKTYP-PNVDKEY   FILTER ON DETAIL SQN                 
*                                                                               
         GOTOR NXTELM,DMCB,SVDTLELM FIND NEXT ELEMENT                           
*                                                                               
         B     RSDTLLP                                                          
*                                                                               
RSDTLDN  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
         CLI   MINERR,0            MUST SUCCEED                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        UPDATE PASSIVE POINTERS                                                
*                                                                               
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
*                                                                               
         CLC   PNVDCLT,SPACES      SKIP IF NO CLIENT                            
         BH    *+10                                                             
         OC    PNVDPUB,PNVDPUB     AND  IF NO PUB                               
         BZ    RSPSVX                                                           
*                                                                               
         CLC   PNVDCLT,PNVHCLT     SKIP IF CLIENT EQUALS HDR CLT                
         BNE   *+10                                                             
         CLC   PNVDPUB,PNVHPUB     AND PUB EQUALS HDR PUB                       
         BE    *+8                                                              
         BRAS  RE,NEWPSV           ADD ANY NEW PASSIVES                         
*                                                                               
RSPSVX   DS    0H                                                               
*                                                                               
*        RE-LINK TO BUY                                                         
*                                                                               
RSBYAD   DS    0H                                                               
*                                                                               
         USING PNVDKEY,SVWRKELM    ESTABLISH DETAIL ELEMENT                     
*                                                                               
         OC    PNVDSER#,PNVDSER#   SKIP IF NO BUY SERIAL #                      
         BZ    RSBYADX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              POINT TO KEY BUILD AREA                      
         USING PSERKEY,R4          ESTABLISH BUY SERIAL KEY                     
*                                                                               
         MVC   PSERKAGY,QAGY       SET AGENCY                                   
         MVC   PSERKMED,QMED       SET MEDIA                                    
         MVI   PSERKRCD,PSERKIDQ   SET RECORD CODE                              
         MVC   PSERKCLT,PNVDCLT    SET CLIENT                                   
*                                                                               
         OC    PSERKCLT,PSERKCLT   IF CLIENT MISSING                            
         BNZ   *+10                                                             
         MVC   PSERKCLT,QCLT          USE MASTER CLIENT                         
*                                                                               
         ZAP   DUB,=P'1000000000'  COMPLEMENT SER#                              
         SP    DUB,PNVDSER#                                                     
         ZAP   PSERKNUM,DUB        SET SERIAL NUMBER                            
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     MUST FIND IT                                 
         BNE   RSBYADX                                                          
*                                                                               
         MVI   RDUPDATE,C'Y'       SET FOR UPDATE                               
*                                                                               
         GOTOR GETREC              READ IN BUY                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND BUY RECORD                    
         USING PBUYREC,R4          ESTABLISH BUYREC                             
*                                                                               
         XC    BUYELM,BUYELM                                                    
         LA    R6,BUYELM           ESTABLISH INVOICE POINTER ELEMENT            
         USING PBNVELMD,R6                                                      
*                                                                               
         MVI   PBNVELM,PBNVELQ     SET ELEMENT ID                               
         MVI   PBNVLEN,PBNVLENQ    SET ELEMENT LENGTH                           
         MVC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY  SET INV SER #                  
         MVC   PBNVDSQN,PNVDKSQN   SET DETAIL SEQ NUMBER                        
         MVC   PBNVINV#,PNVHINV#   SET INVOICE NUMBER                           
         MVC   PBNVISRC,PNVHIVSR   Set invoice header source                    
*                                                                               
         LR    R6,R4               COPY START OF BUY RECORD                     
         MVC   ELCODE,BUYELM       SET FOR CURRENT BUY ELEMENT                  
*                                                                               
         BRAS  RE,GETEL            FIND FIRST                                   
*                                                                               
RSBYADLP DS    0H                                                               
*                                                                               
         BNZ   RSBYADDN            ELEMENT NOT FOUND                            
*                                                                               
         USING PBNVELMD,R6         ESTABLISH BUY INVOICE POINTER                
*                                                                               
         CLC   PBNVSER#,PNVKSER#-PNVKEY+MINMKEY MATCH TO THIS INV               
         BNE   RSBYADCN                                                         
         CLC   PBNVDSQN,PNVDKSQN   AND SAME DETAIL SQN                          
         BE    RSBYADFD                                                         
*                                                                               
RSBYADCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT INVOICE PTR ELM                    
*                                                                               
         B     RSBYADLP                                                         
*                                                                               
RSBYADDN DS    0H                  ADD NEW POINTER TO BUY                       
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  ADD ELEMENT                
*                                                                               
         CLI   PNVHIVSR,INVIPS_Q   IPS electronic invoice?                      
         JNE   RSBYADFD                                                         
         XC    BUYELM,BUYELM                                                    
         LA    R6,BUYELM                                                        
         USING PBMATELD,R6         Buy elem - Invoice matching status           
         MVI   PBMATELM,PBMATELQ   Set element ID                               
         MVI   PBMATLEN,PBMTLENQ   Set element length                           
         MVI   PBMTSTAT,PBMTSPDQ   Default to pending status                    
         DROP  R6                                                               
         LR    R6,R4               Point to begining of Buy record              
         MVC   ELCODE,BUYELM       Look for current Buy element                 
         BRAS  RE,GETEL                                                         
         JE    RSBYADFD            Do not add if already there                  
*                                                                               
         GOTO1 VRECUP,DMCB,(X'01',(R4)),BUYELM,(R6)  Add element                
*                                                                               
         GOTO1 PUTREC              WRITE BUY BACK TO FILE                       
*                                                                               
RSBYADFD DS    0H                  ELEMENT ALREADY IN BUY                       
*                                                                               
RSBYADX  DS    0H                                                               
*                                                                               
RSSER#X  DS    0H                                                               
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   RSLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
RSLNKX   DS    0H                                                               
*                                                                               
RSX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - RSERR '                
***********************************************************************         
*                                                                     *         
*        RESTORE ERROR MESSAGES                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RSBYLKER LHI   RF,PPELOCKD                                                      
         J     DLERR                                                            
*                                                                               
RSERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
SELMSG01 DC    C'RECORD DISPLAYED - HIT PF12 TO RETURN OR NEXT SEL'             
*                                                                               
         LTORG                                                                  
*                                                                               
PF12TXT  DC    C'PF12=RETURN/NEXTSEL'                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
*        CLI   PFAID,2             PF2, CLIENT2?                                
*        BE    CKPFK10                                                          
*        CLI   PFAID,5             PF5, CLT LIST?                               
*        BE    CKPFK10                                                          
*        CLI   PFAID,6             PF6, PRD LIST?                               
*        BE    CKPFK10                                                          
*                                                                               
*        CLI   PFAID,12            PF12, EXIT/RETURN?                           
*        JE    SETCCEQ                                                          
*        CLI   PFAID,24            PF24, EXIT/RETURN?                           
*        JE    SETCCEQ                                                          
*                                                                               
*        J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
*        LA    R1,WORK                                                          
*        USING GLVXFRSY,R1                                                      
*                                                                               
*        MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
*        MVC   GLVXFRPR,=C'INV'    SET FROM PROGRAM                             
*        MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
*        MVC   GLVXTOPR,=C'INV'    SET TO   PROGRAM                             
*        OI    GLVXFLG1,GLV1RETN                                                
*        OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
*        GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
*        MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
*        CLI   PFAID,2             RECORD IS CLIENT2?                           
*        BNE   CKPFK15                                                          
*        MVC   DUB,=C'CLIENT2 '                                                 
*        B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLIENT?                            
*        BNE   CKPFK20                                                          
*        MVC   DUB,=C'CLIENT  '                                                 
*        B     CKPFK25                                                          
*                                                                               
CKPFK20  CLI   PFAID,6             RECORD IS PRD?                               
*        BNE   CKPFK21                                                          
*        MVC   DUB,=C'PRODUCT '                                                 
*        B     CKPFK25                                                          
*                                                                               
CKPFK21  DS    0H                                                               
*        J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
*        MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
*        CLI   PFAID,2             CLIENT MAINT?                                
*        BNE   CKPFK30                                                          
*        MVC   DUB,=C'CHANGE  '                                                 
*        CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
*        BE    CKPFK40                                                          
*        CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
*        BE    CKPFK40                                                          
*        CLI   MODE,VALREC         MODE IS VALREC?                              
*        BE    CKPFK40                                                          
*        CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
*        BE    CKPFK40                                                          
*        CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
*        BE    CKPFK40                                                          
*        MVC   DUB,=C'DISPLAY '                                                 
*        B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLIENT LIST?                                 
*        BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
*        B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
*        BNE   CKPFK35                                                          
*        B     CKPFK30H                                                         
*                                                                               
CKPFK35  DS    0H                                                               
*        J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
*        GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTMEDH,,GLVPRKEY   KEY                   
*        GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTMEDH,,GLVPRMD    MEDIA                 
*        GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTCLTH,,GLVPRCLT   CLIENT                
*                                                                               
*        J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D20 - INVOICE DETAIL MAINT/LIST - LISTLIND'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR A LIST SCREEN LINE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTLIND DSECT                                                                  
LISTLIN  DS    0XL(L'LSTLIN1)      LIST SCREEN LINE                             
LSSTAT   DS    CL1                 C'D' - DELETED                               
LSDTL#   DS    CL5                 DETAIL SEQUENCE NUMBER                       
         DS    CL1                                                              
LSRUNDT  DS    CL8                 DETAIL RUN DATE                              
         DS    CL1                                                              
LSSPACE  DS    CL17                DETAIL SPACE DESCRIPTION                     
         DS    CL1                                                              
LSCAP1   DS    CL20                CAPTION                                      
         DS    CL1                                                              
LSSER#   DS    CL10                BUY SERIAL NUMBER                            
*                                                                               
         PRINT OFF                                                              
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVFCD          INVOICE DETAIL MAINT SCREEN                  
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVFAD          DETAIL LIST SCREEN                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO20   DS    F                   RELOACTION FACTOR                            
*                                                                               
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
CHGMED   EQU   X'80'               AGENCY CHANGED                               
CHGCLT   EQU   X'40'               CLIENT CHANGED                               
CHGPUB   EQU   X'20'               PUB CHANGED                                  
CHGINV   EQU   X'10'               INVOICE NUMBER CHANGED                       
CHGDTL   EQU   X'08'               DETAIL SEQUENCE NUMBER CHANGED               
*                                                                               
SVCLT    DS    CL3                 CLIENT CODE SAVEAREA                         
SVDCL    DS    CL3                 DETAIL CLIENT CODE SAVEAREA                  
SVCLTNM  DS    CL20                CLIENT NAME SAVEAREA                         
SVPRD    DS    CL3                 PRODUCT CODE SAVEAREA                        
SVDPR    DS    CL3                 PRODUCT CODE SAVEAREA                        
SVPRDNM  DS    CL20                PRODUCT NAME SAVEAREA                        
SVEST    DS    CL3                 ESTIMATE CODE SAVEAREA                       
SVDES    DS    CL3                 ESTIMATE CODE SAVEAREA                       
SVESTNM  DS    CL20                ESTIMATE NAME SAVEAREA                       
SVLINNM  DS    XL1                 LINE NUMBER SAVEAREA                         
SVSER#   DS    XL5                 INSERTION SERIAL NUMBER SAVEAREA             
SVPUB    DS    XL6                 PUB NUMBER SAVEAREA                          
SVPUBNM  DS    CL20                PUB NAME   SAVEAREA                          
SVREP    DS    CL4                 REP CODE                                     
SVPSERKY DS    XL32                BUY SERIAL# PASSIVE SAVEAREA                 
SVBSR#   DS    PL5                 BUY SERIAL NUMBER                            
SVAIMP   DS    PL5                 ACTUAL IMPRESSIONS SAVEAREA                  
SVACPM   DS    PL5                 ACTUAL CPMS SAVEAREA                         
SVDSQN   DS    XL2                 DETAIL SEQUENCE NUMBER SAVEAREA              
*                                                                               
SVIDKPRF DS    CL16                Prisma CONTROL PROFILE VALUES                
*                                                                               
SVHDRELM DS    XL256               HEADER ELEMENT SAVEAREA                      
SVDTLELM DS    XL256               DETAIL ELEMENT SAVEAREA                      
SVWRKELM DS    XL256               DETAIL ELEMENT SAVEAREA                      
SVDTLOLD DS    XL256               DETAIL ELEMENT OLD                           
SVVKLKEY DS    XL(L'PNVKEY)        INVOICE KEY SAVEAREA FOR LIST                
*                                                                               
SVLSTDTL DS    20XL(L'PNVDKEY)     DETAIL KEYS FOR LIST                         
SVLSTLNQ EQU   *-SVLSTDTL          TABLE LENGTH                                 
*                                                                               
SVDATE   DS    XL3                 TODAY'S DATE                                 
SVTIME   DS    XL3                 TIME - HMS - BINARY                          
*                                                                               
SVDTLSQN DS    XL2                 DETAIL  SEQUENCE NUMBER                      
SVCOMSQN DS    XL1                 COMMENT SEQUENCE NUMBER                      
*                                                                               
LRLASTSW DS    XL1                 C'Y' - END OF DETAILS                        
*                                                                               
BUYSW    DS    XL1                 SWITCH FOR HANDLING BUY                      
PBNVADDQ EQU   X'80'               ADD PBNVELEM                                 
SER#CHGQ EQU   X'40'               SERIAL # CHANGED                             
*                                                                               
ACTSW    DS    XL1                 C'N' - NEW ACTIVITY ELEMENT                  
*                                  C'O' - OLD ACTIVITY ELEMENT                  
*                                                                               
SVCURST  DS    0H                  CURRENT STATUS                               
*                                                                               
SVDCM    DS    CL10                CURRENT DISCREPANCY COMMENT CODE             
*                                                                               
         DS    0D                  ALIGNMENT                                    
BUYELM   DS    CL128               BUY ELEMENT WORKAREA                         
*                                                                               
         ORG                                                                    
*                                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
** DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD         PERVAL CONTROL BLOCKALD                      
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM RECORD LAYOUTS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDHDR          FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPSTBLK          BLOCK FOR PST VALIDATION CALL                
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE         DSECT FOR CONTROL FILE RECORDS               
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACGENFILE         DSECT FOR OFFICE RECORDS                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   5                   MAX NUM OF FILTER SCANNER ENTRIES            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS         Common facilities                            
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GTINSV_D DSECT                                                                  
       ++INCLUDE PVALUES           Return block for GETINS                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102PPPNV20   03/01/21'                                      
         END                                                                    
