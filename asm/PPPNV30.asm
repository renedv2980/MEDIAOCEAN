*          DATA SET PPPNV30    AT LEVEL 017 AS OF 03/25/05                      
*PHASE T41D30A                                                                  
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST'                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*AUB08/03      BOBY                BIG BANG                                     
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST'                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D30 - COMMENTS MAINT/LIST                          *         
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41D00 (PNV CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41DFA (MAINTENANCE)                           *         
*               SCREEN T4???? (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- FIELD ON SCREEN                                 *         
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
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - INIT'                    
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41D30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41D30,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         ST    RE,RELO30           SAVE RELOCATION FACTOR                       
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVI   ERROR,0             CLEAR OLD STYLE ERROR CODE                   
*                                                                               
         CLC   =CL8'COMMENT',QRECORD IF NOT A COMMENT LAST TIME                 
         BE    INIT10                                                           
*                                                                               
         MVC   QRECORD,=CL8'COMMENT'   SET TO COMMENT RECORD TYPE               
         XC    SVCOMELM,SVCOMELM       INIT SAVED COMMENT ELEMENT               
         XC    SVCSQ,SVCSQ             CLEAR SAVED SEQUENCE NUMBER              
         XC    SVDISP,SVDISP           CLEAR SAVED DISPLACEMENT                 
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - CKMODE'                  
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     TSTMODEX                                                         
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     TSTMODEX                                                         
*                                                                               
         CLI   MODE,DISPREC        DISREC?                                      
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     TSTMODEX                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     TSTMODEX                                                         
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     TSTMODEX                                                         
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     TSTMODEX                                                         
*                                                                               
TSTMODEX DS    0H                                                               
         XIT1                                                                   
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - VK'                      
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
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,COMMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,COMCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,COMPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
*        VALIDATE INVOICE NUMBER                                                
*                                                                               
         LA    R2,COMINVH          POINT TO INVOICE FIELD                       
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
*        IS ACTION COMPATIBLE WITH DELETE/LIVE STATUS                           
*                                                                               
         TM    PNV3CNTL,PNVCDELQ   IF DELETED                                   
         BNO   VKDELN                                                           
*                                                                               
         CLI   ACTNUM,ACTDIS          ACTION MUST BE DISPLAY                    
         BE    *+8                                                              
         CLI   ACTNUM,ACTREST         OR RESTORE                                
         BNE   VKDELER                                                          
*                                                                               
         B     VKACT1                                                           
*                                                                               
VKDELN   DS    0H                  RECORD FOUND AND NOT DELETED                 
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION CANNOT BE DELETED                     
         BE    VKDEL1ER                                                         
*                                                                               
VKACT1   DS    0H                                                               
*                                                                               
*        READ IN INVOICE MASTER RECORD                                          
*                                                                               
         MVC   QDISK,KEY+27        SAVE MASTER REC'S DISK ADDR                  
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
         LA    R2,COMSER#H                                                      
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
         MVI   MINDELSW,C'Y'       READ DELETED MINIO SET                       
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
         LA    R2,COMPERH          POINT TO PERIOD FIELD                        
*                                                                               
         GOTOR DISPER,DMCB,PNVHSTRT   DISPLAY PERIOD                            
*                                                                               
*        VALIDATE DETAIL SEQUENCE NUMBER                                        
*                                                                               
VKDTL    DS    0H                                                               
*                                                                               
         LA    R2,COMDTL#H         POINT TO DETAIL SEQUENCE #                   
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
         GOTOR GETFLD              READ IN FIELD                                
*                                                                               
W        USING FLDHDRD,FLDH        ESTABLISH FIELD HEADER                       
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT AREA                            
         XC    SVDTLELM,SVDTLELM   INIT ELEMENT SAVEAREA                        
         LA    R6,ELEMENT          BUILD ELEMENT KEY                            
         USING PNVDKEY,R6                                                       
*                                                                               
         MVI   PNVDKCDE,PNVDKIDQ   SET DETAIL ELM CODE                          
         MVI   PNVDKLEN,PNVDTLLQ   SET ELEMENT LENGTH                           
*                                                                               
         ZAP   DUB,=P'0'           INIT DETAIL NUMBER = HEADER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,W.FLDILEN      IF  SEQUENCE NUMBER ENTERED                  
         BZ    VKDTL10                                                          
*                                                                               
         TM    W.FLDIIND,FINPNUM       INPUT MUST BE NUMERIC                    
         BNO   VKDTL3E                                                          
*                                                                               
         BCTR  RF,0                    DECREMENT FOR EXECUTE                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,W.FLDDATA(0)        PACK INPUT                               
*                                                                               
VKDTL10  DS    0H                                                               
*                                                                               
         L     RF,GETELM           START WITH A GETELM                          
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BNE   VKDTL15                CHECK PFKEY                               
*                                                                               
         CLI   PFAID,0             IF PFKEY HIT                                 
         BE    *+16                                                             
         XC    SVCSQ,SVCSQ            INIT COMMENT SQN                          
         XC    SVDISP,SVDISP          INIT DISPLACEMENT                         
*                                                                               
         CLI   PFAID,11            IF PFKEY 11                                  
         BE    *+8                                                              
         CLI   PFAID,23            OR PFKEY 23 HIT                              
         BNE   *+14                                                             
         AP    DUB,=P'1'              BUMP SEQUENCE NUMBER                      
         MVI   PNVDKLEN,PNVDKSQN-PNVDKEY    MATCH ON ELEMENT TYPE               
*                                                                               
         CLI   PFAID,10            IF PFKEY 10                                  
         BE    *+8                                                              
         CLI   PFAID,22            OR PFKEY 22 HIT                              
         BNE   *+10                                                             
         SP    DUB,=P'1'              DECREMENT SEQUENCE NUMBER                 
*                                                                               
         CP    DUB,=P'0'           IF TOP OF LIST GONE BY                       
         BNM   *+10                                                             
         ZAP   DUB,=P'0'              DEFAULT TO 0                              
*                                                                               
VKDTL15  DS    0H                                                               
*                                                                               
         CP    DUB,=P'0'           SKIP IF NO DETAIL SQN (HEADER)               
         BE    VKDTL90                                                          
*                                                                               
         CVB   R0,DUB              CVB                                          
         STCM  R0,3,PNVDKSQN       SET DETAIL SEQUENCE NUMBER                   
*                                                                               
         MVI   PNVDKTYP,PNVDKDSQ   SET FOR DETAIL DESCRIPTION                   
*                                                                               
VKDTLLP  DS    0H                                                               
*                                                                               
         GOTOR (RF),DMCB,ELEMENT   READ FOR ELEMENT                             
         BE    VKDTLFD             ELEMENT FOUND                                
*                                                                               
VKDTLNF  DS    0H                  ELEMENT NOT FOUND                            
*                                                                               
         CLI   ACTNUM,ACTDIS       ERROR IF NOT DISPLAYING                      
         BNE   VKDTL2E                                                          
*                                                                               
         CLI   PFAID,10            ERROR IF NOT PFKEY PREV                      
         BE    *+8                                                              
         CLI   PFAID,22                                                         
         BNE   VKDTL2E                                                          
*                                                                               
         SP    DUB,=P'1'              DECREMENT SEQUENCE NUMBER                 
*                                                                               
         CP    DUB,=P'0'           IF TOP OF LIST GONE BY                       
         BNM   *+14                                                             
         ZAP   DUB,=P'0'              DEFAULT TO 0                              
         B     VKDTL90                                                          
*                                                                               
         CVB   R0,DUB              CVB                                          
         STCM  R0,3,PNVDKSQN       SET DETAIL SEQUENCE NUMBER                   
*                                                                               
         B     VKDTLLP                                                          
*                                                                               
VKDTLFD  DS    0H                  SOME ELEMENT FOUND                           
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   OKAY IF DETAIL ELEMENT                       
         BE    VKDTLFD1                                                         
*                                                                               
         CLI   PFAID,11            IF PFKEY NEXT                                
         BE    *+8                                                              
         CLI   PFAID,23                                                         
         BE    *+6                                                              
         DC    H'0'                      SHOULD NOT BE HERE                     
*                                                                               
         L     RF,NXTELM              SWITCH TO NEXT ELMENT                     
         B     VKDTLLP                                                          
*                                                                               
VKDTLFD1 DS    0H                                                               
*                                                                               
         MVC   SVDTLELM,PNVDTLD    SAVE DETAIL ELEMENT                          
*                                                                               
         DROP  R6                                                               
         USING PNVDTLD,SVDTLELM    ESTABLISH DETAIL ELEMENT                     
*                                                                               
VKDTL90  DS    0H                                                               
*                                                                               
*        RE-DISPLAY DETAIL SEQUENCE NUMBER                                      
*                                                                               
VKDTL#   DS    0H                                                               
*                                                                               
         LA    R2,COMDTL#H         POINT TO LINE ITEM DATE                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   SKIP IF NOT DETAIL ELM                       
         BNE   VKDTL#X                                                          
*                                                                               
         EDIT  PNVDKSQN,COMDTL#,0,ALIGN=LEFT                                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'COMDTL#   MAX OUTPUT                                   
*                                                                               
VKDTL#X  DS    0H                                                               
*                                                                               
*        DISPLAY RUN DATE                                                       
*                                                                               
VKDTE    DS    0H                                                               
*                                                                               
         LA    R2,COMDTEH          POINT TO LINE ITEM DATE                      
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD HEADER                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   SKIP IF NOT DETAIL ELM                       
         BNE   VKDTEX                                                           
*                                                                               
         OC    PNVDDTE,PNVDDTE     SKIP IF NO DATE GIVEN                        
         BZ    VKDTEX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PNVDDTE),(17,FLDDATA)  DISP DATE                  
*                                                                               
VKDTEX   DS    0H                                                               
*                                                                               
*        DISPLAY SPACE ENTRY                                                    
*                                                                               
VKSPC    DS    0H                                                               
*                                                                               
         LA    R2,COMSPCH          POINT TO SPACE FIELD                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   SKIP IF NOT DETAIL ELM                       
         BNE   VKSPCX                                                           
*                                                                               
         OC    PNVDSPC,PNVDSPC     SKIP IF NO SPACE GIVEN                       
         BZ    VKSPCX                                                           
*                                                                               
         MVC   FLDDATA(L'PNVDSPC),PNVDSPC DISPLAY SPACE                         
*                                                                               
VKSPCX   DS    0H                                                               
*                                                                               
*        DISPLAY AD CAPTION                                                     
*                                                                               
VKACAP   DS    0H                                                               
*                                                                               
         LA    R2,COMCAP1H         POINT TO AD CAPTIOND                         
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   SKIP IF NOT DETAIL ELM                       
         BNE   VKACAPX                                                          
*                                                                               
         OC    PNVDACAP,PNVDACAP   SKIP IF NO AD CAPTION                        
         BZ    VKACAPX                                                          
*                                                                               
         MVC   FLDDATA(L'PNVDACAP),PNVDACAP DISPLAY AD CAPTION                  
*                                                                               
VKACAPX  DS    0H                                                               
*                                                                               
VKACP2   DS    0H                                                               
*                                                                               
         LA    R2,COMCAP2H         POINT TO AD CAP2 FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   SKIP IF NOT DETAIL ELM                       
         BNE   VKACP2X                                                          
*                                                                               
         OC    PNVDACP2,PNVDACP2   SKIP IF NO AD CAP2                           
         BZ    VKACP2X                                                          
*                                                                               
         MVC   FLDDATA(L'PNVDACP2),PNVDACP2 DISPLAY AD CAP 2                    
*                                                                               
VKACP2X  DS    0H                                                               
*                                                                               
VKDTLX   DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH COMMENTS ELEMENT                   
         USING PNVCOMD,R6                                                       
*                                                                               
         MVI   PNVCKCDE,PNVCKHDQ   SET FOR HEADER COMMENTS                      
*                                                                               
         OC    SVDTLELM,SVDTLELM   IF DETAIL ELEMENT PRESENT                    
         BZ    *+14                                                             
         MVI   PNVCKCDE,PNVCKDTQ      SET FOR DETAIL COMMENTS                   
         MVC   PNVCKDSQ,PNVDKSQN      SET DETAIL SEQUENCE NUMBER                
*                                                                               
         LA    R2,COMTYPH          POINT TO COMMENT TYPE                        
*                                                                               
         MVI   PNVCKTYP,PNVCKPBQ   DEFAULT TO DIALOGUE COMMENTS                 
*                                                                               
         CLI   COMTYP,C'G'         IF GENERAL COMMENTS                          
         BNE   *+8                                                              
         MVI   PNVCKTYP,PNVCKCMQ      RESET FOR GENERAL COMMENTS                
*                                                                               
         BRAS  RE,CLRFLD           CLEAR FIELD                                  
*                                                                               
         MVI   FLDDATA,C'D'        DEFAULT TO DIALOGUE COMMENTS                 
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   IF GENERAL  COMMENTS                         
         BNE   *+8                                                              
         MVI   FLDDATA,C'G'           RESET                                     
*                                                                               
         LA    R2,COMTYPNH         POINT TO COMMENT TYPE                        
*                                                                               
         BRAS  RE,CLRFLD                                                        
*                                                                               
         MVC   FLDDATA(9),=CL9'BYR/PYR'                                         
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   IF GENERAL  COMMENTS                         
         BNE   *+10                                                             
         MVC   FLDDATA(9),=CL9'GENERAL'                                         
*                                                                               
*        VALIDATE COMMENTS GROUP NUMBER                                         
*                                                                               
         LA    R2,COMGRP#H         POINT TO COMMENTS GROUP #                    
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
         GOTOR GETFLD                                                           
*                                                                               
W        USING FLDHDRD,FLDH        ESTABLISH DATA                               
*                                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'1'           DEFAULT TO FIRST GROUP                       
         MVI   PNVCKCGP,1                                                       
         MVI   PNVCKCSQ,1          DEFAULT TO FIRST COMMENT SQN                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,W.FLDILEN      IF  NO GROUP NUMBER                          
         BNZ   *+12                                                             
         MVI   PNVCKLEN,PNVCKCGP-PNVCKEY    SET TO MATCH ON DETAIL              
         B     VKGRP10                                                          
*                                                                               
         TM    W.FLDIIND,FINPNUM   INPUT MUST BE NUMERIC                        
         BNO   VKGRP3E                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,W.FLDDATA(0)    PACK INPUT                                   
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,PNVCKCGP         SET COMMENT GROUP NUMBER                     
*                                                                               
VKGRP10  DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BNE   VKGRP20                                                          
*                                                                               
         CLI   PFAID,0             AND IF PFKEY HIT                             
         BE    VKGRP20                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,PNVCKCGP       GET COMMENT GROUP NUMBER                     
*                                                                               
         CLI   PFAID,8             IF PFKEY DOWN HIT                            
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BNE   VKGRP8N                                                          
*                                                                               
         AHI   RE,1                   BUMP GROUP NUMBER                         
         MVI   PNVCKLEN,PNVCKCGP-PNVCKEY    SET TO MATCH ON DETAIL              
         B     VKGRP15                                                          
*                                                                               
VKGRP8N  DS    0H                                                               
*                                                                               
         CLI   PFAID,7             IF PFKEY UP HIT                              
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BNE   VKGRP7N                                                          
*                                                                               
         SHI   RE,1                   DECREMENT SEQUENCE NUMBER                 
         BZ    VKGRP4E                   NO MORE TO FIND                        
*                                                                               
         B     VKGRP15                                                          
*                                                                               
VKGRP7N  DS    0H                                                               
*                                                                               
VKGRP15  DS    0H                                                               
*                                                                               
         STC   RE,PNVCKCGP         SET GROUP NUMBER                             
*                                                                               
VKGRP20  DS    0H                                                               
*                                                                               
         L     RF,GETELM           READ HI/EQUAL 1ST TIME                       
*                                                                               
VKGRPLP  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
*                                                                               
         GOTOR (RF),DMCB,ELEMENT   READ FOR ELEMENT                             
         BE    VKGRPFD             ELEMENT FOUND                                
*                                                                               
VKGRPNF  DS    0H                  ELM NOT FOUND                                
*                                                                               
         CLI   ACTNUM,ACTADD          OKAY IF ACTION ADD                        
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD        OKAY IF ACTION ADD                        
         BNE   VKGRPNF1                                                         
*                                                                               
         CLI   PNVCKLEN,0          OKAY IF EXACT MATCH WANTED                   
         BNE   VKGRPDN                                                          
*                                                                               
         AHI   R0,1                ELSE USE NEXT AVAILABLE #                    
         STC   R0,PNVCKCGP                                                      
         B     VKGRPDN                                                          
*                                                                               
VKGRPNF1 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS       ERROR IF NOT DISPLAYING                      
         BNE   VKGRP2E                                                          
*                                                                               
         CLI   PFAID,8             EOF   ERROR IF PFKEY DOWN                    
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BE    VKGRP4E                                                          
*                                                                               
         CLI   PFAID,7             ERROR IF NOT PFKEY UP                        
         BE    *+8                                                              
         CLI   PFAID,19                                                         
         BNE   VKGRP4E                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PNVCKCGP         ELSE DECREMENT GRP #                         
         SHI   RE,1                                                             
         STC   RE,PNVCKCGP                                                      
*                                                                               
         B     VKGRPLP             FIND NEXT GROUP                              
*                                                                               
VKGRPFD  DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELM                           
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BNE   VKGRPFD1                                                         
*                                                                               
         CLI   PFAID,8             IF PFKEY DOWN                                
         BE    *+8                                                              
         CLI   PFAID,20                                                         
         BNE   VKGRPFD0                                                         
*                                                                               
         CLI   PNVCKCSQ,1          IF NOT 1ST COMMENT                           
         BE    VKGRPFD0                                                         
*                                                                               
         L     RF,NXTELM           CONTINUE LOOKING                             
         B     VKGRPLP                                                          
*                                                                               
VKGRPFD0 DS    0H                                                               
*                                                                               
         MVC   ELEMENT,0(R6)       COPY TO ELEMENT WORKAREA                     
         B     VKGRPDN                                                          
*                                                                               
VKGRPFD1 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD                                                  
         BNE   VKGRPDN                                                          
*                                                                               
         CLI   W.FLDILEN,0         ERROR IF LOOKING FOR EXACT MATCH             
         BNE   VKGRP1E                                                          
*                                  ELSE                                         
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         ICM   R0,1,PNVCKCGP       SAVE FOUND GROUP NUMBER                      
         L     RF,NXTELM           SET TO READ NEXT ELEMENT                     
*                                                                               
         B     VKGRPLP                                                          
*                                                                               
VKGRPDN  DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTABADD                                                  
         BNE   VKGRPD10                                                         
*                                                                               
         AHI   R0,1                BUMP TO NEXT AVAILABLE GROUP NO.             
*                                                                               
         STCM  R0,1,PNVCKCGP       SET NEW GROUP NUMBER                         
         MVI   PNVCKCSQ,1          FORCE SEQUENCE # 1                           
*                                                                               
VKGRPD10 DS    0H                                                               
*                                                                               
*        RE-DISPLAY GROUP SEQUENCE NUMBER                                       
*                                                                               
         EDIT  PNVCKCGP,COMGRP#,0,ALIGN=LEFT                                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'COMGRP#   MAX OUTPUT                                   
*                                                                               
         CLC   SVELMKEY(PNVCKCSQ-PNVCKEY),PNVCKEY  IF NEW GROUP                 
         BE    *+22                                                             
         XC    SVCOMELM,SVCOMELM   INIT SAVED COMMENT ELEMENT                   
         XC    SVCSQ,SVCSQ         INIT COMMENT SQN                             
         XC    SVDISP,SVDISP       INIT DISPLACEMENT                            
*                                                                               
         MVC   SVELMKEY,ELEMENT    SAVE COMMENT KEY                             
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
         CLI   ACTNUM,ACTABADD     IF ADDING                                    
         BE    *+8                                                              
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   *+10                                                             
         XC    PNVKELMK,PNVKELMK      RECORD CAN'T BE FOUND                     
*                                                                               
         MVC   KEY,MINMKEY         SET MASTER PART OF KEY                       
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
*        COMMENTS ERROR MESSAGES TO BE FORMULATED                               
*                                                                               
VKGRP1E  LHI   RF,PPEGRPFD         GROUP ALREADY ON FILE                        
         J     VKERR1                                                           
*                                                                               
VKGRP2E  LHI   RF,PPEGRPNF         GROUP NOT ON FILE                            
         J     VKERR1                                                           
*                                                                               
VKGRP3E  LHI   RF,PPEGRPNN         GROUP ID MUST BE NUMERIC                     
         J     VKERR1                                                           
*                                                                               
VKGRP4E  LHI   RF,PPEGRPXX         NO MORE COMMENT GROUPS                       
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
         LA    R2,COMSER#H         POINT TO SERIAL NUMBER FIELD                 
         BRAS  RE,CLRFLD           CLEAR INVOICE SERIAL NUMBER FIELD            
*                                                                               
         LA    R2,COMPERH          POINT TO PERIOD FIELD                        
         BRAS  RE,CLRFLD           CLEAR INVOICE PERIOD FIELD                   
*                                                                               
         LA    R2,COMDTEH                                                       
         BRAS  RE,CLRFLD           CLEAR RUN DATE FIELD                         
*                                                                               
         LA    R2,COMCAP1H                                                      
         BRAS  RE,CLRFLD           CLEAR AD CAPTION FIELD                       
         LA    R2,COMCAP2H                                                      
         BRAS  RE,CLRFLD                                                        
*                                                                               
         B     VKERR2                                                           
*                                                                               
VKERR1   DS    0H                                                               
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
VKERR2   DS    0H                                                               
*                                                                               
         LA    R2,COMWHOH                                                       
         BRAS  RE,CLRFLD           CLEAR WHO FIELD                              
*                                                                               
         LA    R2,COMWHENH                                                      
         BRAS  RE,CLRFLD           CLEAR WHEN FIELD                             
*                                                                               
         LA    R2,COMTIMEH                                                      
         BRAS  RE,CLRFLD           CLEAR TIME FIELD                             
*                                                                               
         LA    R2,COMLIN1H         POINT TO FIRST COMMENT                       
         BRAS  RE,CLRSCRN          CLEAR TO END OF SCREEN                       
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
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
         TITLE 'PPPNV20 - PRINT NEW INVOICE COMMENTSS - BUMP'                   
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
         TITLE 'PPPNV30 - PRINT NEW INVOICE COMMENTS - CLRSCRN'                 
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CLEAR FIELDS TO END OF SCREEN                     *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> STARTING FIELD                                         *         
*                                                                     *         
*EXIT    ALL UNPROTECTED FIELDS ARE CLEARED TO END OF SCREEN          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
CLRSCRLP DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR UPPROTECTED FIELD                      
*                                                                               
CLRSCRCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNZ   CLRSCRLP            MORE FIELDS ON SCREEN                        
*                                                                               
CLRSCRDN DS    0H                                                               
*                                                                               
CLRSCRNX DS    0H                  ALL DONE                                     
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - VR'                      
***********************************************************************         
*                                                                     *         
*        VALIDATE INVOICE COMMENTS FIELDS                             *         
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
         USING PNVHDRD,SVHDRELM    ESTABLISH HEADER ELEMENT                     
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         XC    MINMKEY,MINMKEY     INIT MASTER MINIO KEY                        
         MVC   MINMKEY(PNVKELMK-PNVKEY),QINVKEY SET MASTER KEY                  
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
*        CHECK IF GOING TO PFM                                                  
*                                                                               
         CLI   PFAID,9             IF PF9                                       
         BE    *+8                                                              
         CLI   PFAID,21            OR PF21                                      
         BNE   VRPFMX                                                           
*                                                                               
         GOTOR GOPFM               SWITCH TO PFM                                
*                                                                               
         B     VRX                 ALL DONE                                     
*                                                                               
VRPFMX   DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
*                                                                               
*        READ IN COMMENTS ELEMENT                                               
*                                                                               
         USING PNVCOMD,ELEMENT     ESTABLISH COMMENTS ELEMENT KEY               
*                                                                               
         MVC   PNVCKEY,SVELMKEY    RESTORE COMMENT STARTING ELM KEY             
*                                                                               
*        DELETE ALL EXISTING COMMENTS FOR GROUP                                 
*                                                                               
         CLI   ACTNUM,ACTCHA       MUST BE CHANGE                               
         BNE   VRDELDN                                                          
*                                                                               
         MVI   PNVCKLEN,PNVCKCSQ-PNVCKEY  SET FOR MATCH ON GRP #                
*                                                                               
VRDELLP  DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ 1ST/NEXT COMMENT                        
         BNZ   VRDELDN             EOF                                          
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         GOTOR DELELM,DMCB,(R6)    DELETE IT                                    
*                                                                               
VRDELCN  DS    0H                                                               
*                                                                               
         B     VRDELLP                                                          
*                                                                               
VRDELDN  DS    0H                                                               
*                                                                               
*        BUILD NEW ELEMENTS                                                     
*                                                                               
         MVI   PNVCKLEN,PNVCOMLQ   SET MINIMUM LENGTH                           
*                                                                               
         LA    R2,COMLIN1H         POINT TO FIRST LINE OF COMMENT               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         LA    R0,(COMLINLH-COMLIN1H)/(COMLIN2H-COMLIN1H)+1 # OF LNS            
*                                                                               
         MVC   PNVCPID,SVPNVPID    SET WHOSE CHANGING THINGS                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(25,WORK)  GET TODAY/TIME                      
*                                                                               
         MVC   PNVCDATE,WORK       SET DATE                                     
         MVC   PNVCTIME(3),WORK+3  SET TIME HMS BINARY                          
*                                                                               
         MVC   SVELMHDR,PNVCKEY    SAVE ELEMENT HEADER                          
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BE    VRLNK                  SPECIAL HANDLING                          
*                                                                               
VRGRPLP  DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,FLDILEN        GET COMMENT LENGTH                           
         BZ    VRGRPDN             NO MORE DATA FOR COMMENTS                    
*                                                                               
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PNVCCOM(0),FLDDATA  SAVE COMMENT                                 
*                                                                               
         LA    R1,PNVCOMLQ+1(RE)   NEW ELEMENT LENGTH                           
         STC   R1,PNVCKLEN         RESET ELEMENT LENGTH                         
*                                                                               
         GOTOR ADDELM,DMCB,PNVCOMD ADD ELEMENT TO INVOICE                       
*                                                                               
VRGRPCN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PNVCKCSQ         BUMP SEQUENCE NUMBER                         
         AHI   RF,1                FOR NEXT LINE                                
*                                                                               
*        RE-INITIALZE ELEMENT BUILD AREA                                        
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         MVC   PNVCKEY(PNVCOMLQ),SVELMHDR    RESTORE COMMENT HEADER             
*                                                                               
         STC   RF,PNVCKCSQ         SET NEXT SEQUENCE NUMBER                     
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT LINE                            
*                                                                               
         BCT   R0,VRGRPLP                                                       
*                                                                               
VRGRPDN  DS    0H                                                               
*                                                                               
         B     VRMNCLS                                                          
*                                                                               
*        CALL IS FROM LINK                                                      
*        ADD COMMENTS DIRECTLY FROM WORKER FILE                                 
*                                                                               
VRLNK    DS    0H                                                               
*                                                                               
         ICM   R2,15,ALNKCOMM      POINT TO COMMENTS IN WORKER FILE             
         BZ    VRLNKDN                NONE                                      
*                                                                               
         USING WKRCOMD,R2          ESTABLISH WORKER FIELD                       
*                                                                               
VRLNKLP  DS    0H                                                               
*                                                                               
         CLI   WKCMRID,0           DONE IF END OF RECORDS                       
         BE    VRLNKDN                                                          
*                                                                               
         CLC   WKCMMPCD,=X'0022'   MUST BE COMMENT MAP CODE                     
         BE    *+10                                                             
         CLC   WKCMMPCD,=X'0023'                                                
         BNE   VRLNKDN                                                          
*                                                                               
         LA    RF,PNVCOMLQ         SET TO BASIC LENGTH                          
         STC   RF,PNVCKLEN                                                      
*                                                                               
         ICM   RF,3,WKCMRLEN       GET RECORD LENGTH                            
         LR    R0,RF               SAVE RECORD LENGTH                           
*                                                                               
         SHI   RF,WKCMHDLQ         SUBTRACT HEADER LENGTH                       
*                                  NOW HAVE COMMENT LENGTH                      
         BNP   VRLNKCN             NO INPUT                                     
*                                                                               
         CHI   RF,220              MAX 220 BYTES                                
         BNH   *+8                                                              
         LHI   RF,220                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNVCCOM(0),WKCMCOM  SAVE COMMENT                                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PNVCKLEN         GET ELEMENT LENGTH                           
         LA    RE,1(RF,RE)         NEW ELEMENT LENGTH                           
         STC   RE,PNVCKLEN         RESET ELEMENT LENGTH                         
*                                                                               
         GOTOR ADDELM,DMCB,PNVCOMD ADD ELEMENT TO INVOICE                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PNVCKCSQ         BUMP SEQUENCE NUMBER                         
         AHI   RF,1                FOR NEXT LINE                                
*                                                                               
*        RE-INITIALZE ELEMENT BUILD AREA                                        
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         MVC   PNVCKEY(PNVCOMLQ),SVELMHDR    RESTORE COMMENT HEADER             
*                                                                               
         STC   RF,PNVCKCSQ         SET NEXT SEQUENCE NUMBER                     
*                                                                               
VRLNKCN  DS    0H                                                               
*                                                                               
         AR    R2,R0               POINT TO NEXT COMMENT RECORD                 
         B     VRLNKLP                                                          
*                                                                               
VRLNKDN  DS    0H                                                               
*                                                                               
         XC    SVCSQ,SVCSQ         SET FOR DISPLAY WHOLE SCREEN                 
         XC    SVDISP,SVDISP                                                    
*                                                                               
         B     VRMNCLS                                                          
*                                                                               
VRMNCLS  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   VRLNKX                                                           
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
         MVI   DDCOMSW,C'D'        COMMENTS DONE                                
*                                                                               
         B     VRX                 SKIP RE-DISPLAY                              
*                                                                               
VRLNKX   DS    0H                                                               
*                                                                               
         BRAS  RE,DR               DISPLAY COMMENTS                             
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
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - DR'                      
***********************************************************************         
*                                                                     *         
*        DISPLAY  INVOICE COMMENTS FIELDS                             *         
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
*        CLEAR SCREEN                                                           
*                                                                               
         LA    R2,COMWHOH          POINT TO WHO FIELD                           
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LA    R2,COMWHENH         POINT TO WHEN ENTERED DATE                   
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,COMTIMEH         POINT TO TIME FIELD                          
*                                                                               
         BRAS  RE,CLRFLD           CLEAR OUT OLD DATA                           
*                                                                               
         LA    R2,COMLIN1H         POINT TO FIRST COMMENT LINE                  
*                                                                               
         BRAS  RE,CLRSCRN          CLEAR REST OF SCREEN                         
*                                                                               
         B     DRLINDDN                                                         
*                                                                               
         LA    R0,(COMLINLH-COMLIN1H)/(COMLIN2H-COMLIN1H)+1 #OF LINES           
*                                                                               
DRLINDLP DS    0H                  CLEAR OUT COMMENT LINES                      
*                                                                               
         BRAS  RE,CLRFLD           INIT THE OUTPUT AREA                         
*                                                                               
DRLINDCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT LINE                            
*                                                                               
         BCT   R0,DRLINDLP                                                      
*                                                                               
DRLINDDN DS    0H                                                               
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
*        READ IN COMMENTS ELEMENT                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH COMMENTS ELEMENT KEY               
         USING PNVCKEY,R6                                                       
*                                                                               
         MVC   PNVCKEY,SVELMKEY    SET ELEMENT KEY                              
*                                                                               
         CLI   PFAID,9             IF PF9                                       
         BE    *+8                                                              
         CLI   PFAID,21            OR PF21                                      
         BNE   DRPFMX                                                           
*                                                                               
         GOTOR GOPFM               SWITCH TO PFM                                
*                                                                               
         B     DRX                 ALLDONE                                      
*                                                                               
DRPFMX   DS    0H                                                               
*                                                                               
****     MVI   PNVCKCSQ,1          SET SQN TO 1                                 
****     MVI   SVCSQ,0             CLEAR SAVE AREAS                             
****     MVI   SVDISP,0                                                         
*                                                                               
         MVI   PNVCKLEN,PNVCKCSQ-PNVCKEY   MATCH ON GROUP SQN                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SVCSQ          GET LAST SEQUENCE NUMBER                     
*                                                                               
         CLI   SVDISP,0            IF NO RESIDUAL PIECE TO PRINT                
         BNE   *+8                                                              
         AHI   RF,1                   BUMP TO NEXT SQN                          
*                                                                               
         STC   RF,PNVCKCSQ         SET SAVED COMMENT SQN                        
*                                                                               
DRLIN01  DS    0H                                                               
*                                                                               
         GOTO1 GETELM,DMCB,ELEMENT FIND FIRST ELEMENT                           
         BZ    DRLIN05             FOUND                                        
*                                                                               
*        NOT FOUND - RESTART WITH FIRST COMMENT                                 
*                                                                               
         CLI   PNVCKCSQ,1          DONE IF WE WERE LOOKING FOR 1ST              
         BNH   DRLINDN1                                                         
*                                                                               
         MVI   PNVCKCSQ,1          SET SQN TO 1                                 
         MVI   SVCSQ,0             CLEAR SAVE AREAS                             
         MVI   SVDISP,0                                                         
*                                                                               
         B     DRLIN01             RESTART DISPLAY                              
*                                                                               
DRLIN05  DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   SVCOMELM,0(R6)      SAVE COMMENT ELEMENT                         
         MVC   SVCSQ,PNVCKCSQ      SAVE COMMENT SQN                             
*                                                                               
*        DISPLAY WHO                                                            
*                                                                               
DRWHO    DS    0H                                                               
*                                                                               
         OC    PNVCKEY,PNVCKEY     SKIP IF NO ELEMENT AROUND                    
         BZ    DRWHOX                                                           
*                                                                               
         LA    R2,COMWHOH          POINT TO WHO FIELD                           
*                                                                               
         GOTOR TRNPID,DMCB,PNVCPID TRANSLATE PID                                
*                                                                               
DRWHOX   DS    0H                                                               
*                                                                               
*        DISPLAY ENTRY DATE                                                     
*                                                                               
DRDTE    DS    0H                                                               
*                                                                               
         OC    PNVCDATE,PNVCDATE   SKIP IF NO DATE GIVEN                        
         BZ    DRDTEX                                                           
*                                                                               
         LA    R2,COMWHENH         POINT TO WHEN ENTERED DATE                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,PNVCDATE),(17,FLDDATA)  DISP DATE                 
*                                                                               
DRDTEX   DS    0H                                                               
*                                                                               
*        DISPLAY TIME                                                           
*                                                                               
DRTIME   DS    0H                                                               
*                                                                               
         OC    PNVCKEY,PNVCKEY     SKIP IF NO ELEMENT AROUND                    
         BZ    DRTIMEX                                                          
*                                                                               
         LA    R2,COMTIMEH         POINT TO TIME FIELD                          
*                                                                               
         SR    RF,RF                                                            
         SR    RE,RE                                                            
*                                                                               
         IC    RF,PNVCTIME         GET HRS IN BINARY                            
         AHI   RF,6                ADJUST DDS HRS TO MILITARY HRS               
*                                                                               
         MHI   RF,100              SHIFT DIGITS LEFT                            
*                                                                               
         IC    RE,PNVCTIME+1       GET MINUTES                                  
         AR    RF,RE               MILITARY TIME                                
*                                                                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA(4),DUB      DISPLAY TIME                                 
*                                                                               
DRTIMEX  DS    0H                                                               
*                                                                               
*        DISPLAY COMMENTS                                                       
*                                                                               
         LA    R2,COMLIN1H         POINT TO FIRST COMMENT LINE                  
         LA    R0,(COMLINLH-COMLIN1H)/(COMLIN2H-COMLIN1H)+1 #OF LINES           
*                                                                               
DRLINLP  DS    0H                                                               
*                                                                               
         OC    PNVCKEY,PNVCKEY     DONE IF NO ELEMENT FOUND                     
         BZ    DRLINDN1                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PNVCKLEN         GET COMMENT ELM LENGTH                       
         AHI   RF,-PNVCOMLQ        SUBTRACT BASIC LENGTH                        
         BNP   DRLINCN             NO COMMENT                                   
*                                                                               
         LA    R1,PNVCCOM          POINT TO START OF COMMENT                    
*                                                                               
         CLI   SVDISP,0            SKIP IF NO RESIDUAL TO PRINT                 
         BE    DRLIN10                                                          
*                                                                               
         SHI   RF,L'COMLIN1        DECREMENT LENGTH TO PRINT                    
         AHI   R1,L'COMLIN1        BUMP TO NEXT SECTION OF COMMENT              
         CLM   RF,1,SVDISP         CHECK IF STARTING POINT REACHED              
         BH    *-12                NO                                           
*                                                                               
         MVI   SVDISP,0            CLEAR RESIDUAL                               
*                                                                               
DRLIN10  DS    0H                                                               
*                                                                               
         LR    R3,RF               SAVE TOTAL COMMENT LENGTH                    
*                                                                               
DRLIN2LP DS    0H                                                               
*                                                                               
         LR    RF,R3               REMAINING LENGTH TO PRINT                    
*                                                                               
         CHI   RF,L'COMLIN1        IF MORE COMMENT THAN WILL FIT ON LIN         
         BNH   *+8                                                              
         LHI   RF,L'COMLIN1           DISPLAY ONE LINE'S WORTH                  
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),0(R1)    DISPLAY COMMENT                              
*                                                                               
DRLIN2CN DS    0H                                                               
*                                                                               
         LA    R1,1(RF,R1)         NEXT PIECE OF COMMENT                        
*                                                                               
         AHI   RF,1                RESTORE TRUE LENGTH                          
         SR    R3,RF               DECREMENT LENGTH TO PRINT                    
         BNP   DRLIN2DN            COMMENT ALL PRINTED                          
*                                                                               
         BCT   R0,*+8              DECREMENT LINE COUNTER                       
         B     DRLINDN                NO MORE LINES                             
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT LINE                            
*                                                                               
         B     DRLIN2LP            PRINT NEXT LINE                              
*                                                                               
DRLIN2DN DS    0H                                                               
*                                                                               
DRLINCN  DS    0H                                                               
*                                                                               
         BCT   R0,*+8              DECREMENT LINE COUNTER                       
         B     DRLINDN                NO MORE LINES                             
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT LINE                            
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT   READ NEXT COMMENT                          
*                                                                               
         BNZ   DRLINDN1            END OF COMMENTS                              
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   SVCOMELM,0(R6)      SAVE COMMENT ELEMENT                         
         MVC   SVCSQ,PNVCKCSQ      SAVE COMMENT SQN                             
*                                                                               
         B     DRLINLP                                                          
*                                                                               
DRLINDN  DS    0H                                                               
*                                                                               
         STC   R3,SVDISP           SAVE REMAINING LENGTH TO PRINT               
*                                                                               
DRLINDN1 DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
*                                                                               
LRX      DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - WKRCOMD'                 
***********************************************************************         
*                                                                     *         
*        WORKER RECORD COMMENT DSECT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WKRCOMD  DSECT                     WORKER RECORD COMMENTS                       
WKCMRID  DS    XL1                 RECORD ID                                    
WKCMRLEN DS    XL2                 RECORD LENGTH                                
WKCMMPCD DS    XL2                 MAP CODE                                     
WKCMTYPE DS    XL1                 DATA TYPE                                    
WKCMHDLQ EQU   *-WKRCOMD           HEADER LENGTH                                
WKCMCOM  DS    0C                  COMMENT                                      
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
       ++INCLUDE PPPNVFBD          INVOICE COMMENTS MAINT SCREEN                
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
*********INCLUDE PPPNVFBD          COMMENTS LIST SCREEN                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO30   DS    F                   RELOACTION FACTOR                            
*                                                                               
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
CHGMED   EQU   X'80'               AGENCY CHANGED                               
CHGCLT   EQU   X'40'               CLIENT CHANGED                               
CHGPUB   EQU   X'20'               PUB CHANGED                                  
CHGINV   EQU   X'10'               INVOICE NUMBER CHANGED                       
CHGCOM   EQU   X'08'               COMMENTS SEQUENCE NUMBER CHNAGED             
*                                                                               
SVCLT    DS    CL3                 CLIENT CODE SAVEAREA                         
SVDCL    DS    CL3                 COMMENTS CLIENT CODE SAVEAREA                
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
SVPSERKY DS    XL32                BUY SERIAL# PASSIVE SAVEAREA                 
SVBSR#   DS    PL5                 BUY SERIAL NUMBER                            
SVAIMP   DS    PL5                 ACTUAL IMPRESSIONS SAVEAREA                  
SVACPM   DS    PL5                 ACTUAL CPMS SAVEAREA                         
SVDSQN   DS    XL2                 COMMENTS SEQUENCE NUMBER SAVEAREA            
*                                                                               
SVELMKEY DS    XL(L'PNVCKEY)       ELEMENT KEY SAVEAREA                         
SVELMHDR DS    XL(PNVCOMLQ)        ELEMENT HEADER SAVEAREA                      
SVHDRELM DS    XL(PNVHDRLQ)        HEADER ELEMENT SAVEAREA                      
SVDTLELM DS    XL(PNVDTLLQ)        DETAIL ELEMENT SAVEAREA                      
SVCOMELM DS    XL256               COMMENT ELEMENT                              
SVCSQ    DS    XL1                 SAVED COMMENT SEQUENCE NUMBER                
SVDISP   DS    XL1                 SAVED COMMENT DISPLACEMENT                   
*                                                                               
BUYSW    DS    XL1                 SWITCH FOR HANDLING BUY                      
PBNVADDQ EQU   X'80'               ADD PBNVELEM                                 
SER#CHGQ EQU   X'40'               SERIAL # CHANGED                             
*                                                                               
         DS    0D                  ALIGNMENT                                    
BUYELM   DS    CL256               BUY ELEMENT WORKAREA                         
*                                                                               
         ORG                                                                    
*                                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE         CONTROL SYSTEM RECORD LAYOUTS                
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
*DDGLVXCTLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD        GLOBBER XCTL DSECT                           
         PRINT ON                                                               
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS        GLOBBER EQUATES                              
         PRINT ON                                                               
*DDGLPFMD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGLPFMD          PFM FILE INFO GLOBAL                         
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM RECORD LAYOUTS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PTRACLTPP         CLT TRAFFIC OFFICE CODE PASSIVE PTR          
         EJECT                                                                  
*                                                                               
       ++INCLUDE POFFCLTPP         CLT OFFICE CODE PASSIVE PTR                  
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
       ++INCLUDE ACGENFILE         DSECT FOR OFFICE RECORDS                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   5                   MAX NUM OF FILTER SCANNER ENTRIES            
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPPNV30   03/25/05'                                      
         END                                                                    
