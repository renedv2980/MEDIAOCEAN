*          DATA SET PPPNV40    AT LEVEL 048 AS OF 10/12/17                      
*PHASE T41D40A                                                                  
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY'                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
*MAR/06  BOBY  DISPLAY ACTUAL STATUS                                            
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY'                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D40 - INVOICE HEADER ACTIVITY       *                        
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41D00 (PNV CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41DF8 (MAINTENANCE)                           *         
*                                                                               
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
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY '                              
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41D40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41D40,RR=RE                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         ST    RE,RELO40                                                        
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY - CKMODE'                      
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   CKMVKN                                                           
*                                                                               
         CLI   ACTNUM,ACTHIST      IF DOWNLOADING HISTORY                       
         BNE   CKMVK10                                                          
*                                                                               
         GOTOR GETINPUT,DMCB,LNKAIO  READ IN LINKIO DATA                        
*                                                                               
CKMVK10  DS    0H                                                               
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMVKN   DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         VALREC?                                      
         BNE   CKMVRN                                                           
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT A LINK CALL                      
         BNE   CKMVR10                                                          
*                                                                               
         CLI   ACTNUM,ACTHIST      SKIP IF NOT INVOICE HISTORY DOWNLOAD         
         BNE   CKMVR10                                                          
*                                                                               
         BRAS  RE,HST              SEND INVOICE HISTORY                         
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMVR10  DS    0H                                                               
*                                                                               
         BRAS  RE,VR                                                            
         B     CKMODEX                                                          
*                                                                               
CKMVRN   DS    0H                                                               
*                                                                               
         CLI   MODE,DISPREC        DISREC?                                      
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPKEY        DISKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
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
CKMODEX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY - VK'                          
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
         MVC   SVKEY,KEY           SAVE CURRENT PRTDIR KEY                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,HDAMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
*                                                                               
         CLI   FLDILEN,0           MEDIA IS REQUIRED                            
         BE    VKMEDER                                                          
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,HDACLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VKCLTER                                                          
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,HDAPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         CLI   FLDILEN,0           PUB IS REQUIRED                              
         BE    VKPUBER                                                          
*                                                                               
         LA    R2,HDAINVH          POINT TO INVOICE FIELD                       
*                                                                               
         GOTOR VVALINVN            VALIDATE INVOICE #                           
*                                                                               
         CLI   FLDILEN,0           INVOICE IS REQUIRED                          
         BE    VKINVER                                                          
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
         ZIC   R1,FLDILEN                                                       
         AHI   R1,-1                                                            
         EX    R1,VK90                                                          
         J     VK95                                                             
*                                                                               
VK90     MVC   PNV3INV#(0),HDAINV                                               
*                                                                               
VK95     DS    0H                                                               
         OC    PNV3INV#,SPACES     MAKE SURE IT FILLED WITH SPACES              
*                                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR KEY                          
*                                                                               
*                                                                               
         CLC   PNV3KEY,KEYSAVE     YES, SAME RECORD?                            
         JE    VK110               YES                                          
         J     VKINV2ER            NO, RECORD NOT FOUND                         
*                                                                               
VK110    DS    0H                                                               
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
         LA    R2,HDASER#H                                                      
*                                                                               
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),QSER#(L'QSER#+1)  UNPACK                       
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   FLDDATA(2*L'PNVKSER#),WORK   DISPLAY SERIAL NUMBER               
         MVI   FLDILEN,2*L'PNVKSER#  SET FIELD LENGTH                           
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
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
VKINV1ER LHI   RF,PPELONG         INVOICE  NUMBER TOO LARGE                     
         J     VKERR                                                            
*                                                                               
VKINV2ER LHI   RF,PPEINVNF        INVOICE  NOT ON FILE                          
         J     VKERR                                                            
*                                                                               
VKDELER  LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR                                                            
*                                                                               
VKDEL1ER LHI   RF,PPERECDL        RECORD IS DELETED                             
         J     VKERR                                                            
*                                                                               
VKDUPKER LHI   RF,PPEDUPKY        DUP KEY                                       
         J     VKERR                                                            
*                                                                               
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,HDASER#H         POINT TO SERIAL NUMBER FIELD                 
         BRAS  RE,CLRFLD           CLEAR INVOICE SERIAL NUMBER FIELD            
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY - VR'                          
***********************************************************************         
*                                                                     *         
*        VALIDATE INVOICE HEADER FIELDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
*                                                                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY - DR'                          
***********************************************************************         
*                                                                     *         
*        DISPLAY INVOICE HEADER RECORD                               *          
*                                                                     *         
***********************************************************************         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
DRAC     USING PNVKEY,R4           DISPLAY REC ACTIVE USING                     
*                                                                               
*                                                                               
         MVC   DRAC.PNVKEY,QINVKEY SHOULD HAVE KEY OF MASTER KEY                
         MVC   QSER#,DRAC.PNVKSER#                                              
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CURDET#,HDADET       CHECK IF DET# CHANGED                       
         JNE   DR05                 IF YES START FROM SCRETCH                   
*                                                                               
DR01     DS    0H                                                               
*                                                                               
         CLC   QINVKEY,CURMKEY                                                  
         JNE   DR05                                                             
         OC    CONTIFLG,CONTIFLG    CHECK IF CONTINUATION                       
         JNZ   DR30                                                             
*                                                                               
*        BUILD ELEM TO MAKE A SEARCH FOR                                        
*        LENGTH SO FAR IS 4, SEQ NUMBER IS A VOID FOR (HEADER)                  
*                                                                               
DR05     DS    0H                                                               
         XC    CONTIFLG,CONTIFLG    CLEAR FLAG                                  
         MVC   CURMKEY,QINVKEY      SAVE CURRENT MASTER KEY                     
         MVC   CURDET#,HDADET       SAVE CURRENT DET SEQ #                      
*                                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR ELM AREA                              
         USING PNVACTHD,R3                                                      
*                                                                               
         OC    HDADET,HDADET        CHECK IF DETAIL# ENTERED                    
         BNZ   DR08                                                             
*                                                                               
*        BUILD HEADER ACT ELEM TO DO SEARCH                                     
*                                                                               
         MVI   PNVAKEY,PNVAKHDQ     ELEM ID                                     
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY   LENGTH OF KEY TO SEARCH FOR          
         MVI   PNVAKACT,PNVAKACQ    ELEM ID                                     
         J     DR09                                                             
*                                                                               
DR08     DS    0H                                                               
*                                                                               
*        BUILD DETAIL ACT ELEM TO DO SEARCH                                     
*                                                                               
         MVI   PNVAKEY,PNVAKDTQ     ELEM ID                                     
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY   LENGTH OF KEY TO SEARCH FOR          
         MVI   PNVAKACT,PNVAKACQ    ELEM ID                                     
*                                                                               
         LA    R2,HDADETH                                                       
         ZIC   R1,FLDILEN           GET LENGTH                                  
         AHI   R1,-1                                                            
         EX    R1,PACKSEQ#                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,PNVAKDSQ        STORE SEQ#                                  
         J     DR09                                                             
*                                                                               
PACKSEQ# PACK  DUB,HDADET(0)                                                    
*                                                                               
DR09     DS    0H                                                               
         GOTOR GETELM,DMCB,PNVAKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         CLC   PNVAKCDE,ELEMENT                                                 
         JNE   DRDTLNF              CHECK IF ITS THERE                          
         CLC   PNVAKDSQ(L'PNVAKDSQ+L'PNVAKACT),ELEMENT+2                        
         JNE   DRDTLNF                                                          
*                                                                               
DR10     DS    0H                                                               
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JZ    DR21                 CHECK IF ITS THERE                          
*                                                                               
         CLI   PNVAKACT,PNVAKACQ    ELEM ID                                     
         BNE   DR20                                                             
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         ZIC   R1,PNVAKLEN          MOVE LENGTH TO R1                           
         AHI   R1,-1                                                            
         EX    R1,DR15                                                          
         J     DR20                                                             
DR15     DS    0H                                                               
         MVC   HDRACTEL(0),PNVAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
DR20     DS    0H                                                               
*                                                                               
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY   LENGTH OF KEY TO SEARCH FOR          
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         JE    DR10                 IF FOUND GO INTO LOOP                       
*                                                                               
DR21     DS    0H                                                               
*                                                                               
         LA    R3,HDRACTEL          POINT R3 TO A ELEM                          
*                                                                               
         OC    HDAMED(L'PNVKMED),SPACES   LOWER TO UPPER CASE                   
         OI    HDAMEDH+6,X'80'                                                  
*                                                                               
         OC    HDACLT(L'PNVHCLT),SPACES   LOWER TO UPPER CASE                   
         OI    HDACLTH+6,X'80'                                                  
*                                                                               
         MVC   HDATTL(L'HDATTL),=C'INVOICE HEADER ACTIVITY'                     
*                                                                               
         OC    HDADET,HDADET                                                    
         BZ    *+10                                                             
         MVC   HDATTL(L'HDATTL),=C'INVOICE DETAIL ACTIVITY'                     
*                                                                               
         OI    HDATTLH+6,X'80'                                                  
*                                                                               
         MVC   HDAMES(L'HDAMES),=C'**  HIT ENTER FOR PRIOR CHANGES  **'         
         OI    HDAMESH+6,X'80'                                                  
*                                                                               
         LA    R2,HDAPIDH                                                       
         GOTOR TRNPID,DMCB,PNVAHPID                                             
         OI    HDAPIDH+6,X'80'                                                  
*                                                                               
         XC    HDADTE,HDADTE       INVOICE DATE                                 
         GOTO1 DATCON,DMCB,(3,PNVAHDTE),(11,HDADTE)                             
         OI    HDADTEH+6,X'80'                                                  
*                                                                               
         MVC   WRKCRST,PNVACRST    SAVE CURRENT STATUS                          
*                                                                               
         LA    R2,HDACHG1H                                                      
         GOTOR VDISCHG,DMCB,PNVAHCH1                                            
*                                                                               
*        MOVE 01 INTO CONTIFLG TO INDICATE                                      
*        THAT WE IN PROGRESS OF OUTPUTING PID                                   
*                                                                               
*                                                                               
         OC    HDADET,HDADET                                                    
         JNZ   DR25                                                             
         OI    HDAINVH+6,X'40'     LOCATE CURSOR                                
         J     DR27                                                             
*                                                                               
DR25     DS    0H                                                               
         OI    HDADETH+6,X'40'                                                  
*                                                                               
DR27     DS    0H                                                               
         MVI   CONTIFLG,X'01'                                                   
         J     DR200                                                            
*                                                                               
DR30     DS    0H                                                               
*                                                                               
         LA    R3,HDRACTEL                                                      
         GOTOR GETELM,DMCB,PNVAKEY  RE GET THE LAST ELEM                        
*                                                                               
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
         GOTOR PRVELM,DMCB,PNVAKEY                                              
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JZ    DR50                 CHECK IF ITS THERE                          
         CLC   PNVAKEY(5),HDRACTEL                                              
         JNE   DR50                                                             
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         ZIC   R1,PNVAKLEN          MOVE LENGTH TO R1                           
         AHI   R1,-1                                                            
         EX    R1,DR35                                                          
         J     DR40                                                             
DR35     DS    0H                                                               
         MVC   HDRACTEL(0),PNVAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
DR40     DS    0H                                                               
         J     DR21                 LOGIC TO OUTPUT PID AND DATE                
*                                                                               
DR50     DS    0H                                                               
*                                                                               
         MVC   HDAMES(L'HDAMES),=C'*****     NO PRIOR CHANGES    *****'         
         OI    HDAMESH+6,X'80'                                                  
*                                                                               
         OC    HDADET,HDADET                                                    
         JNZ   DR55                                                             
         OI    HDAINVH+6,X'40'       CURSOR AT INV #                            
         J     DR60                                                             
*                                                                               
DR55     DS    0H                                                               
         OI    HDADETH+6,X'40'       CURSOR AT DET SEQ #                        
*                                                                               
DR60     DS    0H                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA FOR ACT ELEM                
         XC    CONTIFLG,CONTIFLG    CLEAR FLAG, THE LAST PID                    
         J     DR200                                                            
*****                                                                           
*****                                                                           
*****                                                                           
*                                                                               
DR200    DS    0H                                                               
*                                                                               
*                                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BO    DRDELREC                                                         
*                                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
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
*                                                                               
*                                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
DRDELREC LA    R2,HDAINVH                                                       
         LHI   RF,PPERECDL         RECORD IS DELETED                            
         J     DRERR                                                            
*                                                                               
DRDTLNF  DS    0H                                                               
         LA    R2,HDADETH                                                       
         LHI   RF,PPEDTLNF         DETAIL NOT ON FILE                           
         J     DRERR                                                            
*                                                                               
DRERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       NTR1  BASE=*,LABEL=*      DISPLAY KEY                                  
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
***                                                                             
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       NTR1  BASE=*,LABEL=*      LIST RECORDS                                 
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
LRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
*                                                                               
PRX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
***********************************************************************         
***********************************************************************         
****                                                                            
****     VALIDATE INVOICE #                                                     
****                                                                            
***********************************************************************         
VVALINVN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         GOTOR GETFLD              READ STATUS INTO WORKAREA                    
*                                                                               
         CLI   FLDILEN,PNVHI#MX    INPUT SHOULD BE 1 CHAR                       
         JH    VINVTOLN            ERROR TOO LONG                               
*                                                                               
         XC    BYTE,BYTE                                                        
*                                                                               
         LA    R3,FLD                                                           
         LA    R5,PNVHI#MX                                                      
*                                                                               
VALINV00 DS    0H                                                               
         CLI   0(R3),X'40'         CHECK IF SPACE                               
         BE    VALINV30                                                         
         BH    VALINV20                                                         
         B     VALINVX                                                          
*                                                                               
VALINV20 DS    0H                                                               
         CLI   BYTE,0                                                           
         BNE   VINVSPAC                                                         
         B     VALINV40                                                         
VALINV30 DS    0H                                                               
         MVI   BYTE,1                                                           
*                                                                               
VALINV40 DS    0H                                                               
         LA    R3,1(R3)                                                         
         BCT   R5,VALINV00                                                      
         B     VALINVX                                                          
*                                                                               
VALINVX  DS    0H                                                               
         XC    BYTE,BYTE                                                        
         XIT1                                                                   
*                                                                               
VINVSPAC DS    0H                                                               
         XC    ERROR,ERROR                                                      
         LHI   RF,PPENOSPA     NO SPACE                                         
         J     VINVERR                                                          
*                                                                               
VINVTOLN DS    0H                                                               
         XC    ERROR,ERROR                                                      
         LHI   RF,PPETOLNG     TOO LONG                                         
         J     VINVERR                                                          
*                                                                               
VINVERR  DS    0H                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
         DROP                                                                   
*                                                                               
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
*          DATA SET PPPNV40C   AT LEVEL 232 AS OF 07/09/03                      
         TITLE 'PPPNV00 - PRINT NEW INVOICE CONTROLLER - BUMP'                  
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*   ROUTINE TO OUTPUT LIST OF CHANGES                                           
*   R2 POINT TO FIRST FIELD                                                     
*   P1 IS ADDRESS OF THE FIRST CHANGE INDICATOR                                 
*                                                                               
****************************************************************                
VDISCHG  NTR1  BASE=*,LABEL=*      DISPLAY LIST OF CHANGES                      
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
*        CLEAR REST OF THE SCREEN FIRST                                         
*                                                                               
VDCHG000 DS    0H                                                               
         TM    FLDATB,X'08'                                                     
         JO    VDCHG005                                                         
         BRAS  RE,CLRFLD                                                        
         BRAS  RE,BUMP                                                          
         JNE   VDCHG000                                                         
*                                                                               
VDCHG005 DS    0H                                                               
         LA    R2,HDACHG1H                                                      
*                                                                               
         L     R1,0(R1)                                                         
         L     R6,=A(CHGLIST)      POINT R6 TO TABLE OF HEADER CHG              
         OC    HDADET,HDADET                                                    
         BZ    *+8                                                              
         L     R6,=A(CHGLST2)      POINT R6 TO TABLE OF DETAIL CHG              
*                                                                               
         A     R6,RELO40                                                        
*                                                                               
         SR    R4,R4               CLEAR R4                                     
         SR    R5,R5               CLEAR R5                                     
         IC    R4,0(R1)            INSERT INDCHG1                               
         SLL   R4,8                                                             
         IC    R4,1(R1)            INSERT INDCHG2                               
         SLL   R4,8                                                             
         IC    R4,2(R1)            INSERT INDCHG3                               
         OC    HDADET,HDADET                                                    
         BZ    VDCHG010                                                         
         SLL   R4,8                                                             
         IC    R4,3(R1)            INSERT INDCHG4                               
*                                                                               
VDCHG010 DS    0H                                                               
         SRDL  R4,1                USES EVEN ODD PAIR  R4 AND R5                
         LTR   R5,R5                                                            
         JNM   VDCHG050                                                         
*                                                                               
         MVC   FLDDATA(16),0(R6)   DISPLAY CHANGE                               
         MVI   FLDILEN,16          SET FIELD LENGTH 16                          
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
         OC    HDADET,HDADET       SKIP IF DETAIL SCREEN                        
         BNZ   VDCHG040                                                         
*                                                                               
         CLC   =C'STATUS',FLDDATA  IF STATUS CHANGED                            
         BNE   VDCHG040                                                         
*                                                                               
         LA    R3,HSTATTAB         POINT TO HEADER STATUS TABLE                 
*                                                                               
VDCHGSTL DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         DONE AT END OF TABLE                         
         BE    VDCHGSTD                                                         
*                                                                               
         CLC   WRKCRST,0(R3)       MATCH CURRENT STATUS TO TABLE                
         BE    VDCHGSTF                                                         
*                                                                               
VDCHGSTC DS    0H                                                               
*                                                                               
         LA    R3,11(R3)           NEXT TABLE ENTRY                             
         B     VDCHGSTL                                                         
*                                                                               
VDCHGSTF DS    0H                                                               
*                                                                               
         MVC   FLDDATA(16),SPACES  ERASE 'STATUS'                               
         MVC   FLDDATA(10),1(R3)   DISPLAY CURRENT STATUS                       
*                                                                               
VDCHGSTD DS    0H                                                               
*                                                                               
VDCHG040 DS    0H                                                               
         BRAS  RE,BUMP                                                          
*                                                                               
VDCHG050 DS    0H                                                               
         LTR   R4,R4               END                                          
         JZ    VDISCHGX                                                         
         LA    R6,16(R6)           BUMP TO NEXT ENTRY IN A TABLE                
         J     VDCHG010            GO TO NEXT BIT                               
*                                                                               
VDISCHGX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
HSTATTAB DS    0H                                                               
         DC    CL1'P',CL10'PENDING'                                             
         DC    CL1'M',CL10'MATCHED'                                             
         DC    CL1'D',CL10'DISCREPANT'                                          
         DC    X'FF'                                                            
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CHGLIST  DS    0C                                                               
*                                                                               
         DC    CL16'                '    HDRCHG3  X'01'                         
         DC    CL16'                '             X'02'                         
         DC    CL16'                '             X'04'                         
         DC    CL16'                '             X'08'                         
         DC    CL16'                '             X'10'                         
         DC    CL16'                '             X'20'                         
         DC    CL16'                '             X'40'                         
         DC    CL16'                '             X'80'                         
         DC    CL16'PST             '    HDRCHG2  X'01'                         
         DC    CL16'GST             '             X'02'                         
         DC    CL16'TOTAL           '             X'04'                         
         DC    CL16'CD              '             X'08'                         
         DC    CL16'SPECIAL REP     '             X'10'                         
         DC    CL16'PERIOD          '             X'20'                         
         DC    CL16'STATUS          '             X'40'                         
         DC    CL16'INVOICE DATE    '             X'80'                         
         DC    CL16'                '    HDRCHG1  X'01'                         
         DC    CL16'TOTAL TYPE      '             X'02'                         
         DC    CL16'RESTORED        '             X'04'                         
         DC    CL16'INVOICE #       '             X'08'                         
         DC    CL16'PUB CODE        '             X'10'                         
         DC    CL16'CLIENT          '             X'20'                         
         DC    CL16'DELETED         '             X'40'                         
         DC    CL16'ADDED           '             X'80'                         
         EJECT                                                                  
*                                                                               
*                                                                               
CHGLST2  DS    0C                                                               
*                                                                               
         DC    CL16'                '    HDRCHG4  X'01'                         
         DC    CL16'                '             X'02'                         
         DC    CL16'                '             X'04'                         
         DC    CL16'                '             X'08'                         
         DC    CL16'                '             X'10'                         
         DC    CL16'                '             X'20'                         
         DC    CL16'                '             X'40'                         
         DC    CL16'# OF INSERTIONS '             X'80'                         
         DC    CL16'CPMS            '    HDRCHG3  X'01'                         
         DC    CL16'IMPRESSIONS     '             X'02'                         
         DC    CL16'BUY LINE NUMBER '             X'04'                         
         DC    CL16'BUY LINE DATE   '             X'08'                         
         DC    CL16'ESTIMATE        '             X'10'                         
         DC    CL16'PRODUCTE        '             X'20'                         
         DC    CL16'BUY SERIAL #    '             X'40'                         
         DC    CL16'CLIENT          '             X'80'                         
         DC    CL16'PREMIUM         '    HDRCHG2  X'01'                         
         DC    CL16'NET             '             X'02'                         
         DC    CL16'GROSS           '             X'04'                         
         DC    CL16'RATE            '             X'08'                         
         DC    CL16'SPECIAL REP     '             X'10'                         
         DC    CL16'PST CODE        '             X'20'                         
         DC    CL16'GST CODE        '             X'40'                         
         DC    CL16'CASH DISCOUNT   '             X'80'                         
         DC    CL16'# OF COLORS     '    HDRCHG1  X'01'                         
         DC    CL16'CAPTION         '             X'02'                         
         DC    CL16'SPC DESCRIPTION '             X'04'                         
         DC    CL16'PUB             '             X'08'                         
         DC    CL16'RUN DATE        '             X'10'                         
         DC    CL16'DETAIL RESTORED '             X'20'                         
         DC    CL16'DETAIL DELETED  '             X'40'                         
         DC    CL16'DETAIL ADDED    '             X'80'                         
         EJECT                                                                  
*                                                                               
         TITLE 'T41D40 - INVOICE HEADER ACTIVITY - HST'                         
***********************************************************************         
*                                                                     *         
*        DOWNLOAD INVOICE HISTORY                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
HST      NTR1  BASE=*,LABEL=*      DOWNLOAD INVOICE HISTORY                     
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT IN A LINK CALL                   
         BNE   HSTX                                                             
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4           DISPLAY REC ACTIVE USING                     
*                                                                               
         BRAS  RE,FILLKEY          FILL IN SCREEN                               
*                                                                               
*        SEND NORMAL REPLY TO CALLER                                            
*                                                                               
HSTNVLP  DS    0H                                                               
*                                                                               
*        INIT BXLE REGS FOR HISTORY TABLE                                       
*                                                                               
         LA    R3,HTBTAB           START                                        
         USING HTBENTD,R3          ESTABLISH ENTRY IN TABLE                     
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         LA    R4,HTBENTL          ENTRY LENGTH                                 
         LR    R5,R3                                                            
         BCTR  R5,0                END                                          
*                                                                               
         STM   R3,R5,HTBBXLE SET BXLE REGS                                      
*                                                                               
         ST    R3,HTBNXTA          SET A(NEXT ENTRY IN LIST)                    
*                                                                               
         MVC   WRKINVKY,LNKINVKY   SAVE INVOICE KEY                             
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         MVC   MINMKEY,QINVKEY     MASTER KEY                                   
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
*        OPEN MINIO SET                                                         
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,MINERNF      ERROR IF NOT FOUND                           
         BE    HSTNFER                                                          
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    MINSTAT,MINDELQ     ERROR IF DELETED                             
         BO    HSTNFER                                                          
*                                                                               
*        BUILD ELEM KEY TO MAKE A SEARCH FOR ELEMENTS                           
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR ELM AREA                              
         USING PNVACTHD,R6                                                      
*                                                                               
*        BUILD STARTING ACTIVITY ELEM KEY                                       
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    ASSUME HEADER DATA WANTED                   
*                                                                               
         OC    QDSQN,QDSQN          IF DETAIL SEQUENCE NUMBER GIVEN             
         BZ    *+14                                                             
         MVI   PNVAKCDE,PNVAKDTQ      SET FOR DETAIL DATA ELEMENT               
         MVC   PNVAKDSQ,QDSQN         SET SEQUENCE NUMBER                       
*                                                                               
         MVI   PNVAKLEN,1          FILTER ON ELEMENT TYPE                       
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  GET FIRST ELEMENT                           
*                                                                               
HSTLOOP  DS    0H                                                               
*                                                                               
         ICM   R6,15,MINELEM       POINT R6 TO ELEMENT                          
         BZ    HSTNEXT             NONE FOUND                                   
*                                                                               
         OC    PNVAKEY,PNVAKEY     DONE IF NO ELEMENT                           
         BZ    HSTNEXT                                                          
*                                                                               
         CLC   PNVAKCDE,ELEMENT    MUST BE OF SAME DATA TYPE                    
         BNE   HSTNEXT                                                          
*                                                                               
         OC    QDSQN,QDSQN         IF DOING DETAIL ELEMENT                      
         BZ    *+14                                                             
         CLC   PNVAKDSQ,QDSQN      MUST MATCH SEQ# - NULLS FOR HEADER           
         BNE   HSTNEXT                                                          
*                                                                               
         USING PNVDKEY,R6          ESTABLISH AS DETAIL ELEMENT                  
*                                                                               
         CLI   PNVDKTYP,PNVDKDSQ   IF A DETAIL DESCRIPTION ELM                  
         BNE   HSTBUYX                                                          
*                                                                               
         TM    PNVDSTAT,PNVDDLQ    SKIP IF DELETED                              
         BO    HSTCONT                                                          
*                                                                               
         MVC   SVPNVDEL,PNVDKEY    SAVE DETAIL ELEMENT                          
*                                                                               
         BRAS  RE,ADDBUY              ADD BUY ACTIVITY TO TABLE                 
*                                                                               
         B     HSTCONT                                                          
*                                                                               
HSTBUYX  DS    0H                                                               
*                                                                               
         USING PNVACTHD,R6                                                      
*                                                                               
         CLI   PNVAKACT,PNVAKACQ   IF AN ACTIVITY ELEMENT                       
         BNE   HSTACTX                                                          
*                                                                               
         TM    PNVASTAT,PNVADLQ    SKIP IF DELETED                              
         BO    HSTCONT                                                          
*                                                                               
         BRAS  RE,ADDACT              ADD ACTIVITY TO HISTORY TABLE             
*                                                                               
HSTACTX  DS    0H                                                               
*                                                                               
HSTCONT  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,PNVAKEY  GET NEXT ELEM                               
*                                                                               
         B     HSTLOOP              IF FOUND - PROCESS                          
*                                                                               
HSTNEXT  DS    0H                                                               
*                                                                               
         OC    QDSQN,QDSQN         DONE IF DETAIL SQN GIVEN                     
         BNZ   HSTDONE                                                          
*                                                                               
         CLI   WRKHSTYP,C'H'       DONE IF ONLY HEADER WANTED                   
         BE    HSTDONE                                                          
*                                                                               
         LA    R6,ELEMENT          POINT TO MODEL KEY                           
*                                                                               
         CLI   PNVAKCDE,PNVAKDTQ   DONE IF DETAILS JUST DONE                    
         BE    HSTDONE                                                          
*                                                                               
         MVI   PNVAKCDE,PNVAKDTQ   SET FOR DETAIL ELEMENTS                      
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT GET FIRST DETAIL ELEMENT                     
*                                                                               
         B     HSTLOOP                                                          
*                                                                               
HSTDONE  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
HSTNVCN  DS    0H                                                               
*                                                                               
         BRAS  RE,SNDHST           SEND INVOICE HISTORY                         
*                                                                               
         BRAS  RE,FILLKEY          GET NEXT HISTORY REQUEST                     
*                                                                               
         CLI   LNKSTSW,C'L'        CHECK FOR NO MORE DATA                       
         BE    HSTNVDN                DONE IF NO MORE EXIST                     
*                                                                               
         BRAS  RE,VK               VALIDATE NEW INVOICE                         
*                                                                               
         B     HSTNVLP             LOOP                                         
*                                                                               
HSTNVDN  DS    0H                                                               
*                                                                               
         MVI   DDLNKEOF,C'Y'       INDICATE ALL DONE                            
*                                                                               
*        CLOSE LINKIO                                                           
*                                                                               
         BRAS  RE,INILIOB                                                       
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD) CLOSE WORKER FILE                 
*                                                                               
HSTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
HSTNFER  DS    0H                                                               
         LHI   RF,PPEINVNF         INVOICE NOT ON FILE                          
*                                                                               
HSTERR   DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPINV40 - PRINT NEW INVOICE ACTIVITY - GETINPUT'                
***********************************************************************         
*                                                                     *         
*        SAVE LINKIO INPUT                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETINPUT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         L     R4,0(R1)            SAVE A(INPUT)                                
*                                                                               
         LA    R3,0(R4)            POINT TO START OF INPUT                      
         LHI   R1,SVINPUT-SYSD                                                  
         LA    R1,SYSD(R1)         POINT TO SAVED INPUT                         
         SR    RF,RF                                                            
         SR    RE,RE                                                            
*                                                                               
GINPLOOP DS    0H                                                               
*                                                                               
         USING WKRDATD,R3          ESTABLISH WORKER FILE ENTRY                  
*                                                                               
         ICM   RE,3,WKDTMPCD       GET MAP CODE                                 
         BZ    GINPDONE            END OF INPUT                                 
*                                                                               
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)       MOVE TO WORK AREA                            
*                                                                               
GINPCONT DS    0H                                                               
*                                                                               
         LA    R1,1(RF,R1)         NEXT SAVEAREA                                
         LA    R3,1(RF,R3)         NEXT INPUT ELEMENT                           
*                                                                               
         B     GINPLOOP                                                         
*                                                                               
GINPDONE DS    0H                                                               
*                                                                               
*        NOW THAT WE HAVE THE INPUT                                             
*              MAKE SURE WE HAVE READ TO THE END OF THE FILE                    
*                                                                               
         BRAS  RE,INILIOB                                                       
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
GINPGTLP DS    0H                                                               
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF                                 
         BO    GINPGTDN                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)                                   
*                                                                               
GINPGTCN DS    0H                                                               
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   SEARCH FOR END OF FILE                       
         BNO   GINPGTLP                                                         
*                                                                               
GINPGTDN DS    0H                                                               
*                                                                               
GETINPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPINV40 - PRINT NEW INVOICE ACTIVITY - SNDHST'                  
***********************************************************************         
*                                                                     *         
*        SEND HISTORY TO ADBUYER                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
SNDHST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
*        SORT HISTORY TABLE BY DATE                                             
*                                                                               
         LM    R3,R5,HTBBXLE       LOAD BXLE REGS FOR TABLE                     
         AHI   R5,1                                                             
*                                                                               
         CR    R5,R3               DONE IF NO ENTRIES IN TABLE                  
         BNH   SNDHSTX                                                          
*                                                                               
         LR    RF,R5                                                            
         SR    RF,R3                                                            
         SR    RE,RE                                                            
         DR    RE,R4               NUMBER OF ENTRIES IN TABLE                   
         LR    R0,RF                                                            
*                                                                               
         GOTOR XSORT,DMCB,(R3),(R0),(R4),(R4),0 SORT TABLE                      
*                                                                               
         LR    R5,R3               START OF TABLE                               
*                                                                               
         BRAS  RE,INILIOB                                                       
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         XC    SVHTBENT,SVHTBENT   INIT PREVIOUS TABLE ENTRY                    
*                                                                               
SDHLOOP  DS    0H                                                               
*                                                                               
         USING HTBENTD,R5          ESTABLISH TABLE ENTRY                        
*                                                                               
         OC    HTBENTD(HTBENTL),HTBENTD DONE AT END OF TABLE                    
         BZ    SDHDONE                                                          
*                                                                               
*        SEND INVOICE HEADER HISTORY                                            
*                                                                               
SDHHDR   DS    0H                                                               
*                                                                               
         OC    HTBDSQN,HTBDSQN     NO DETAIL SQN MEANS HEADER ENTRY             
         BNZ   SDHHDRN                                                          
*                                                                               
*        HEADER ACTIVITY RECORD CODE                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INVHSH)              
*                                                                               
*        INVOICE KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    X        
               ('LD_CHARQ',WRKINVKY),(L'WRKINVKY,0)                             
*                                                                               
*        ACTIVITY DATE                                                          
*                                                                               
         XC    HDADTE,HDADTE       MM/DD/YY FORMAT                              
         GOTO1 DATCON,DMCB,(X'43',HTBDATE),(10,HDADTE)                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTDAT),    X        
               ('LD_CHARQ',HDADTE),(L'HDADTE,0)                                 
*                                                                               
*        PERSON                                                                 
*                                                                               
         LA    R2,HDAPIDH                                                       
         GOTOR TRNPID,DMCB,HTBPID      FIND NAME FROM PID                       
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTUSR),    X        
               ('LD_CHARQ',HDAPID),(L'HDAPID,0)                                 
*                                                                               
*        ACTIVITY                                                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTTYP),    X        
               ('LD_CHARQ',HTBACT),(L'HTBACT,0)                                 
*                                                                               
SDHHDRX  DS    0H                                                               
*                                                                               
         B     SDHCONT                                                          
*                                                                               
SDHHDRN  DS    0H                                                               
*                                                                               
*        SEND LINE ITEM DETAIL INFO                                             
*                                                                               
SDHDTL   DS    0H                                                               
*                                                                               
*        LINE ITEM TABLE ENTRY                                                  
*                                                                               
         CLC   HTBENTD(HTBKEYL),SVHTBENT     SKIP IF SAME LINE ITEM             
         BE    SDHINFOX               AS LAST TIME                              
*                                                                               
*        LINE ITEM DETAIL INFO RECORD CODE                                      
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INVHSD)              
*                                                                               
*        INVOICE KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    X        
               ('LD_CHARQ',WRKINVKY),(L'WRKINVKY,0)                             
*                                                                               
*        LINE ITEM SEQUENCE NUMBER                                              
*                                                                               
         EDIT  (2,HTBDSQN),(3,WORK),ALIGN=LEFT                                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ITMSQN),    X        
               ('LD_CHARQ',WORK),((R0),0)                                       
*                                                                               
*        INSERTION DATE WITH REFERENCE NUMBER                                   
*                                                                               
SDHINS   DS    0H                                                               
*                                                                               
         OC    HTBBDTE,HTBBDTE     SKIP IF NO INSERTION DATE                    
         BZ    SDHINSX                                                          
*                                                                               
         XC    WRKDATE,WRKDATE     MM/DD/YY                                     
         GOTO1 DATCON,DMCB,(X'43',HTBBDTE),(10,WRKDATE)                         
*                                                                               
         CLI   HTBBLINE,1          SKIP IF BUYLINE IS 1                         
         BNH   SDHBLINX                                                         
*                                                                               
         MVI   WRKDATE+8,C'-'                                                   
         EDIT  (1,HTBBLINE),(3,WRKDATE+9),ALIGN=LEFT LINE NUMBER                
*                                                                               
         CLI   HTBBLINE,100        OKAY IF LESS THAN 100                        
         BL    SDHBLINX                                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,HTBBLINE       LINE NUMBER                                  
         CVD   RF,DUB                                                           
         DP    DUB,=P'10'          MULTIPLE OF 10'S                             
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,ALPHATAB(RF)                                                  
         MVC   WRKDATE+9(1),1(RF)                                               
         MVC   WRKDATE+10(1),WRKDATE+11 UNITS                                   
         MVI   WRKDATE+11,C' '     CLEAR                                        
*                                                                               
SDHBLINX DS    0H                                                               
*                                                                               
*        INSERTION DATE WITH LINE NUMBER                                        
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSDAT),    X        
               ('LD_CHARQ',WRKDATE),(11,0)                                      
*                                                                               
SDHINSX  DS    0H                                                               
*                                                                               
*        RNO INDICATOR IF PRESENT                                               
*                                                                               
         CLI   HTBRNO,C'Y'         SKIP IF NOT AN RNO                           
         BNE   SDHDRNOX                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RNOIND),    X        
               ('LD_CHARQ',HTBRNO),(L'HTBRNO,0)                                 
*                                                                               
SDHDRNOX DS    0H                                                               
*                                                                               
*        CLIENT CODE                                                            
*                                                                               
         OC    HTBCLT,HTBCLT       SKIP IF NO CLIENT                            
         BZ    SDHCLTX                                                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTCOD),    X        
               ('LD_CHARQ',HTBCLT),(L'HTBCLT,0)                                 
*                                                                               
SDHCLTX  DS    0H                                                               
*                                                                               
*        PRODUCT CODE                                                           
*                                                                               
         OC    HTBPRD,HTBPRD       SKIP IF NO PRODUCT                           
         BZ    SDHPRDX                                                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDCOD),    X        
               ('LD_CHARQ',HTBPRD),(L'HTBPRD,0)                                 
*                                                                               
SDHPRDX  DS    0H                                                               
*                                                                               
*        ESTIMATE NUMBER                                                        
*                                                                               
         OC    HTBEST,HTBEST       SKIP IF NO ESTIMATE                          
         BZ    SDHESTX                                                          
*                                                                               
         EDIT  (2,HTBEST),(3,WORK),ALIGN=LEFT                                   
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNUM),    X        
               ('LD_CHARQ',WORK),((R0),0)                                       
*                                                                               
SDHESTX  DS    0H                                                               
*                                                                               
*        PUB CODE                                                               
*                                                                               
         OC    HTBPUB,HTBPUB       SKIP IF NO PUB                               
         BZ    SDHPUBX                                                          
*                                                                               
         LA    R2,HDAPUBH          POINT TO PUB FIELD                           
         MVC   QPUB,HTBPUB         MOVE PUB TO WORKAREA                         
*                                                                               
         GOTOR DISPUB              DISPLAY PUB                                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          PUB NUMBER LENGTH                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBCOD),    X        
               ('LD_CHARQ',HDAPUB),((R0),0)                                     
*                                                                               
SDHPUBX  DS    0H                                                               
*                                                                               
SDHINFOX DS    0H                                                               
*                                                                               
*        LINE ITEM ACTIVITY                                                     
*                                                                               
SDHDACT  DS    0H                                                               
*                                                                               
         OC    HTBDATE,HTBDATE     SKIP IF NO ACTIVITY DATE                     
         BZ    SDHDACTN                                                         
*                                                                               
         CLI   HTBDATE,X'FF'       SKIP IF NO ACTIVITY DATE                     
         BE    SDHDACTN                                                         
*                                                                               
*        LINE ITEM ACTIVITY RECORD CODE                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INVHSA)              
*                                                                               
*        ACTIVITY DATE                                                          
*                                                                               
         XC    HDADTE,HDADTE       MM/DD/YY FORMAT                              
         GOTO1 DATCON,DMCB,(X'43',HTBDATE),(10,HDADTE)                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTDAT),    X        
               ('LD_CHARQ',HDADTE),(L'HDADTE,0)                                 
*                                                                               
*        PERSON                                                                 
*                                                                               
         LA    R2,HDAPIDH                                                       
         GOTOR TRNPID,DMCB,HTBPID      FIND NAME FROM PID                       
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTUSR),    X        
               ('LD_CHARQ',HDAPID),(L'HDAPID,0)                                 
*                                                                               
*        ACTIVITY                                                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTTYP),    X        
               ('LD_CHARQ',HTBACT),(L'HTBACT,0)                                 
*                                                                               
SDHDACTX DS    0H                                                               
*                                                                               
         B     SDHCONT                                                          
*                                                                               
SDHDACTN DS    0H                                                               
*                                                                               
*        OTHER INVOICES ON THE BUY                                              
*                                                                               
SDHDPNV  DS    0H                                                               
*                                                                               
         OC    HTBPNV#,HTBPNV#     SKIP IF NO OTHER INVOICES                    
         BE    SDHDPNVN                                                         
*                                                                               
*        OTHER INVOICE RECORD CODE                                              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INVHSI)              
*                                                                               
*        RETURN LINE ITEM NUMBER                                                
*                                                                               
         EDIT  (2,HTBPNVSQ),(3,WORK),ALIGN=LEFT LINE ITEM SEQ. NO.              
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ITMSQN),    X        
               ('LD_CHARQ',WORK),((R0),0)                                       
*                                                                               
*        RETURN VENDOR INVOICE NUMBER                                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#VINVNO),    X        
               ('LD_CHARQ',HTBPNV#),(L'HTBPNV#,0)                               
*                                                                               
*        RETURN INVOICE KEY                                                     
*                                                                               
         MVC   WORK(1),QMED        SET MEDIA                                    
         UNPK  WORK+1(11),HTBSER#(6) DISPLAY SERIAL NUMBER                      
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    X        
               ('LD_CHARQ',WORK),(11,0)                                         
*                                                                               
SDHDPNVX DS    0H                                                               
*                                                                               
         B     SDHCONT                                                          
*                                                                               
SDHDPNVN DS    0H                                                               
*                                                                               
SDHCONT  DS    0H                                                               
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE ENTRY IN HISTORY TABLE                  
*                                                                               
         LA    R5,HTBENTL(R5)      BUMP TO NEXT ENTRY IN TABLE                  
         B     SDHLOOP                                                          
*                                                                               
SDHDONE  DS    0H                                                               
*                                                                               
*                                                                               
SNDHSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
                                                                                
ALPHATAB DC    C' ABCDEFGHIJKLMNOP'   FOR LINE NUMBERS 100-250                  
*                                                                               
*        TABLE OF HEADER ACTIVITIES TO BE REPORTED                              
*                                                                               
SDHHDRTB DS    0D                  TABLE OF ACTIVITIES TO BE REPORTED           
         DC    AL1(PNVAHADD,0,0,0,0,0),CL18'INVOICE ADDED'                      
SDHHDRTL EQU   *-SDHHDRTB          TABLE ENTRY LENGTH                           
         DC    AL1(0,PNVAHSTA,0,0,0,0),CL18'STATUS'                             
         DC    AL1(0,PNVAHTOT,0,0,0,0),CL18'TOTAL'                              
SDHHDRTX DC    X'FF'               EOT                                          
*                                                                               
*        TABLE OF DETAIL ACTIVITIES TO BE REPORTED                              
*                                                                               
SDHDTLTB DS    0D                  TABLE OF ACTIVITIES TO BE REPORTED           
         DC    AL1(PNVADADD,0,0,0,0,0),CL18'LINE ITEM ADDED'                    
SDHDTLTL EQU   *-SDHDTLTB          TABLE ENTRY LENGTH                           
         DC    AL1(0,PNVADBS#,0,0,0,0),CL18'STATUS'                             
         DC    AL1(0,PNVADRTE,0,0,0,0),CL18'LINE ITEM RATE'                     
         DC    AL1(0,0,0,PNVADDCM,0,0),CL18'DISCREP COMMENT'                    
SDHDTLX  DC    X'FF'               EOT                                          
*                                                                               
*        TABLE OF STATUSES                                                      
*                                                                               
SDHHSTAT DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
SDHDSTAT DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPINV40 - PRINT NEW INVOICE ACTIVITY - ADDACT'                  
***********************************************************************         
*                                                                     *         
*        ADD ACTIVITY TO HISTORY TABLE                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
ADDACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         USING MINBLKD,R7          ESTABLSH MINIO CONTROL BLOCK                 
*                                                                               
         ICM   R6,15,MINELEM       POINT TO CURRENT ACTIVITY ELEMENT            
         USING PNVACTHD,R6         ESTABLISH HEADER DETAIL ELEMENT              
*                                                                               
         LA    R5,AACHDRTB         ASSUME HEADER ACTIVITY                       
         CLI   PNVAKCDE,PNVAKDTQ   SWITCH TABLES IF DETAIL ACTIVITY             
         BNE   *+8                                                              
         LA    R5,AACDTLTB                                                      
*                                  TABLE HAS ACTIVITIES TO BE REPORTED          
AACACTLP DS    0H                                                               
*                                                                               
*        ACTIVITY                                                               
*                                                                               
         CLI   0(R5),X'FF'         DONE AT END OF TABLE                         
         BE    AACACTDN                                                         
*                                                                               
         MVC   WRKACTV,PNVAHCH1    COPY ACTIVITY FOR DATE                       
*                                                                               
         NC    WRKACTV,0(R5)       AND WITH ACTIVITY WANTED                     
         BZ    AACACTCN            NOT A WANTED ACTIVITY                        
*                                                                               
         MVC   WRKACTVC,L'WRKACTV(R5)  TRANSLATION                              
*                                                                               
         CLC   =C'STATUS',WRKACTVC IF STATUS CHANGED                            
         BNE   AACSTADN                                                         
*                                                                               
*        FIND ACTUAL STATUS                                                     
*                                                                               
         LA    RF,AACHSTAT         ASSUME HEADER ACTIVITY                       
         CLI   PNVAKCDE,PNVAKDTQ   SWITCH TABLES IF DETAIL ACTIVITY             
         BNE   *+8                                                              
         LA    RF,AACDSTAT                                                      
*                                                                               
AACSTALP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    AACSTADN                                                         
*                                                                               
         CLC   PNVACRST,0(RF)      MATCH CURRENT STATUS TO TABLE                
         BE    AACSTAFD                                                         
*                                                                               
AACSTACN DS    0H                                                               
*                                                                               
         LA    RF,19(RF)           NEXT TABLE ENTRY                             
         B     AACSTALP                                                         
*                                                                               
AACSTAFD DS    0H                                                               
*                                                                               
         MVC   WRKACTVC,1(RF)      RETURN TRANSLATED STATUS                     
*                                                                               
AACSTADN DS    0H                                                               
*                                                                               
*        ADD ACTIVITY TO HISTORY TABLE                                          
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT ENTRY IN TABLE                 
         USING HTBENTD,R3          ESTABLISH ENTRY IN TABLE                     
*                                                                               
         MVC   HTBDSQN,PNVAKDSQ    LINE ITEM SEQUENCE NUMBER                    
         MVC   HTBDATE,PNVAHDTE    SAVE DATE                                    
         MVC   HTBPID,PNVAHPID     SAVE PID                                     
         MVC   HTBACT,WRKACTVC     SAVE ACTIVITY DESCRIPTION                    
*                                                                               
*        COPY DATE/CLT/PRD/EST/PUB FROM SAVED LINE ITEM ELEMENT                 
*                                                                               
         CLI   PNVAKCDE,PNVAKDTQ   IF DETAIL ACTIVITY                           
         BNE   AACCPPX                                                          
*                                                                               
         MVC   HTBBDTE,PNVDBYDT-PNVDTLD+SVPNVDEL BUY DATE                       
*                                                                               
         CLI   HTBBDTE+2,0         IF MONTHLY BUY                               
         BNE   *+8                                                              
         MVI   HTBBDTE+2,1            CONVERT TO FIRST OF MONTH                 
*                                                                               
         MVI   HTBRNO,0            ASSUME NOT AN RNO                            
         OC    PNVDSER#-PNVDTLD+SVPNVDEL,PNVDSER#-PNVDTLD+SVPNVDEL              
         BNZ   *+8                 FLAG AS RNO                                  
         MVI   HTBRNO,C'Y'                                                      
*                                                                               
         MVC   HTBCLT,PNVDCLT-PNVDTLD+SVPNVDEL CLIENT                           
         MVC   HTBPRD,PNVDPRD-PNVDTLD+SVPNVDEL PRODUCT                          
         MVC   HTBEST,PNVDEST-PNVDTLD+SVPNVDEL ESTIMATE                         
         MVC   HTBPUB,PNVDPUB-PNVDTLD+SVPNVDEL PUB                              
         MVC   HTBBLINE,PNVDLIN#-PNVDTLD+SVPNVDEL LINE NUMBER                   
*                                                                               
AACCPPX  DS    0H                                                               
*                                                                               
         LA    R3,HTBENTL(R3)      BUMP TO NEXT ENTRY IN TABLE                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
         BCTR  R3,0                                                             
         ST    R3,HTBBXEND         UPDATE END OF BXLE                           
*                                                                               
AACACTCN DS    0H                                                               
*                                                                               
         LA    R5,AACHDRTL(R5)     BUMP TO NEXT ENTRY IN TABLE                  
         B     AACACTLP                                                         
*                                                                               
AACACTDN DS    0H                                                               
*                                                                               
ADDACTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF HEADER ACTIVITIES TO BE REPORTED                              
*                                                                               
AACHDRTB DS    0D                  TABLE OF ACTIVITIES TO BE REPORTED           
         DC    AL1(PNVAHADD,0,0,0,0,0),CL18'INVOICE ADDED'                      
AACHDRTL EQU   *-AACHDRTB          TABLE ENTRY LENGTH                           
         DC    AL1(0,PNVAHSTA,0,0,0,0),CL18'STATUS'                             
         DC    AL1(0,PNVAHSRP,0,0,0,0),CL18'SPECIAL REP'                        
         DC    AL1(0,PNVAHTOT,0,0,0,0),CL18'TOTAL'                              
AACHDRX  DC    X'FF'               EOT                                          
*                                                                               
*        TABLE OF DETAIL ACTIVITIES TO BE REPORTED                              
*                                                                               
AACDTLTB DS    0D                  TABLE OF ACTIVITIES TO BE REPORTED           
         DC    AL1(PNVADADD,0,0,0,0,0),CL18'LINE ITEM ADDED'                    
AACDTLTL EQU   *-AACDTLTB          TABLE ENTRY LENGTH                           
         DC    AL1(0,PNVADBS#,0,0,0,0),CL18'STATUS'                             
         DC    AL1(0,PNVADRTE,0,0,0,0),CL18'LINE ITEM RATE'                     
         DC    AL1(0,0,0,PNVADDCM,0,0),CL18'DISCREP COMMENT'                    
AACDTLX  DC    X'FF'               EOT                                          
*                                                                               
*        TABLE OF STATUSES                                                      
*                                                                               
AACHSTAT DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
AACDSTAT DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPINV40 - PRINT NEW INVOICE ACTIVITY - ADDBUY'                  
***********************************************************************         
*                                                                     *         
*        ADD LINKED INSERTION ACTIVITY TO HISTORY TABLE               *         
*                                                                     *         
***********************************************************************         
*                                                                               
ADDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         USING MINBLKD,R7          ESTABLSH MINIO CONTROL BLOCK                 
*                                                                               
         ICM   R5,15,MINELEM       POINT TO CURRENT DETAIL ELEMENT              
         USING PNVDTLD,R5          ESTABLISH DETAIL ELEMENT                     
*                                                                               
         OC    PNVDSER#,PNVDSER#   IF NO BUY SERIAL NUMBER                      
         BNZ   ABYRNON                THEN RNO                                  
*                                                                               
*        ADD TO HISTORY TABLE                                                   
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT SLOT IN TABLE                  
         USING HTBENTD,R3          ESTABLISH ENTRY                              
*                                                                               
         MVC   HTBDSQN,PNVDKSQN    LINE ITEM SEQUENCE NUMBER                    
         MVI   HTBDATE,X'FF'       FORCE TO SORT LAST IN ACTIVITIES             
         MVC   HTBBDTE,PNVDDTE     LINE ITEM RUN DATE                           
         MVI   HTBRNO,C'Y'                                                      
         MVC   HTBCLT,PNVDCLT      CLIENT                                       
         MVC   HTBPRD,PNVDPRD      PRODUCT                                      
         MVC   HTBEST,PNVDEST      ESTIMATE                                     
         MVC   HTBPUB,PNVDPUB      PUB                                          
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE TABLE ENTRY                             
         LA    R3,HTBENTL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
ABYRNOX  DS    0H                                                               
         B     ADDBUYX                                                          
*                                                                               
ABYRNON  DS    0H                                                               
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
         GOTOR HIGH                READ FOR PASSIVE POINTER                     
*                                                                               
         CLC   PSERKEY,KEYSAVE     SKIP IF NOT FOUND                            
         BNE   ADDBUYX                                                          
*                                                                               
         MVC   AIO,AIO1            READ BUY REC INTO AIO1                       
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
*        FIND INVOICE HISTORY ELEMENTS                                          
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         USING PBUYREC,R4          ESTABLISH BUY RECORD                         
*                                                                               
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,PBYIELCQ     SET TO FIND INVOICE HISTORY ELM              
*                                                                               
         BRAS  RE,GETEL            FIND INVOICE ELEMENT                         
*                                                                               
ABYHSTLP DS    0H                                                               
*                                                                               
         BNE   ABYHSTDN            NONE FOUND                                   
*                                                                               
         USING PBYIELMD,R6         ESTABLISH INVOICE HISTORY ELM                
*                                                                               
         LA    RF,ABYSTAT          POINT TO LIST OF STATUSES                    
*                                                                               
ABYSTALP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         SKIP IF NOT IN TABLE                         
         BE    ABYSTADN                                                         
*                                                                               
         CLC   PBYISTAT,0(RF)      MATCH INVOICE MATCHING STATUS                
         BE    ABYSTAFD                                                         
*                                                                               
ABYSTACN DS    0H                                                               
         LA    RF,19(RF)           BUMP TO NEXT ENTRY IN TABLE                  
         B     ABYSTALP                                                         
*                                                                               
ABYSTAFD DS    0H                                                               
*                                                                               
*        ADD TO HISTORY TABLE                                                   
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT SLOT IN TABLE                  
         USING HTBENTD,R3          ESTABLISH ENTRY                              
*                                                                               
         MVC   HTBDSQN,PNVDKSQN    LINE ITEM SEQUENCE NUMBER                    
         MVC   HTBDATE,PBYIDATE    SET DATE                                     
         MVC   HTBPID,PBYI_PID     SET PID                                      
         MVC   HTBACT,1(RF)        STATUS DESCRIPTION                           
         MVC   HTBBDTE,PBUYKDAT    BUY LINE DATE                                
         MVC   HTBBLINE,PBUYKLIN   BUY LINE REFERENCE NUMBER                    
*                                                                               
         MVC   HTBCLT,PBUYKCLT     CLIENT                                       
         MVC   HTBPRD,PBUYKPRD     PRODUCT                                      
         MVC   HTBEST,PBUYKEST     ESTIMATE                                     
         MVC   HTBPUB,PBUYKPUB     PUB                                          
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE TABLE ENTRY                             
         LA    R3,HTBENTL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
ABYSTADN DS    0H                                                               
*                                                                               
ABYHSTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     ABYHSTLP                                                         
*                                                                               
ABYHSTDN DS    0H                                                               
*                                                                               
*        FIND TEARSHEET CHANGED                                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,PCHGELEQ     SET TO FIND CHANGE ELEMENTS                  
*                                                                               
         BRAS  RE,GETEL            FIND INVOICE ELEMENT                         
*                                                                               
ABYCHGLP DS    0H                                                               
*                                                                               
         BNE   ABYCHGDN            NONE FOUND                                   
*                                                                               
         USING PCHGELEM,R6         ESTABLISH INVOICE HISTORY ELM                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PCHGLEN        GET ELEMENT LENGTH                           
*                                                                               
         CHI   RF,PCHGNEWS         SKIP IF OLD FORMAT FOR ELEMENT               
         BL    ABYCHGCN                                                         
*                                                                               
         CHI   RF,PCHGOLDL                                                      
         BE    ABYCHGCN            OLD ELEMENT WITH COSTS                       
*                                                                               
         LA    RE,0(RF,R6)         POINT TO END OF ELEMENT                      
         SHI   RE,L'PCHGEXT        BACK UP TO START OF EXTENSION                
*                                                                               
         USING PCHG_XLS,RE         ESTABLISH EXTENSION                          
*                                                                               
         TM    PCHGIND5,PCHGTSAQ   SKIP UNLESS T/S APPROVED                     
         BNO   ABYCHGCN                                                         
*                                                                               
*        ADD TO HISTORY TABLE                                                   
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT SLOT IN TABLE                  
         USING HTBENTD,R3          ESTABLISH ENTRY                              
*                                                                               
         MVC   HTBDSQN,PNVDKSQN    LINE ITEM SEQUENCE NUMBER                    
         MVC   HTBPID,PCHGPID      SET PID                                      
         MVC   HTBACT,=CL18'TS STATUS CHANGED'                                  
*                                                                               
         MVC   HTBBDTE,PBUYKDAT    BUY LINE DATE                                
         GOTOR DATCON,DMCB,(2,PCHGDAT),(3,HTBDATE)                              
*                                                                               
         MVC   HTBBLINE,PBUYKLIN   BUY LINE REFERENCE NUMBER                    
*                                                                               
         MVC   HTBCLT,PBUYKCLT     CLIENT                                       
         MVC   HTBPRD,PBUYKPRD     PRODUCT                                      
         MVC   HTBEST,PBUYKEST     ESTIMATE                                     
         MVC   HTBPUB,PBUYKPUB     PUB                                          
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE TABLE ENTRY                             
         LA    R3,HTBENTL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
ABYCHGCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     ABYCHGLP                                                         
*                                                                               
ABYCHGDN DS    0H                                                               
*                                                                               
*        FIND PAY ELEMENTS                                                      
*                                                                               
         B     ABYPAYX             NO PAY ELEMENTS FOR NOW                      
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,X'25'        SET TO FIND PAY ELEMENTS                     
*                                                                               
         BRAS  RE,GETEL            FIND PAY ELEMENT                             
*                                                                               
ABYPAYLP DS    0H                                                               
*                                                                               
         BNE   ABYPAYDN            NONE FOUND                                   
*                                                                               
         USING PPAYELEM,R6         ESTABLISH INVOICE HISTORY ELM                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PPAYELEM+1     GET ELEMENT LENGTH                           
*                                                                               
         OC    PPDDATE,PPDDATE     SKIP UNLESS PAID                             
         BZ    ABYPAYCN                                                         
*                                                                               
*        ADD TO HISTORY TABLE                                                   
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT SLOT IN TABLE                  
         USING HTBENTD,R3          ESTABLISH ENTRY                              
*                                                                               
         MVC   HTBDSQN,PNVDKSQN    LINE ITEM SEQUENCE NUMBER                    
         MVC   HTBDATE,PPDDATE     SET CLEARANCE DATE                           
         MVC   HTBACT,=CL18'CLEARED'                                            
         MVC   HTBBDTE,PBUYKDAT    BUY LINE DATE                                
         MVC   HTBBLINE,PBUYKLIN   BUY LINE REFERENCE NUMBER                    
*                                                                               
         MVC   HTBCLT,PBUYKCLT     CLIENT                                       
         MVC   HTBPRD,PBUYKPRD     PRODUCT                                      
         MVC   HTBEST,PBUYKEST     ESTIMATE                                     
         MVC   HTBPUB,PBUYKPUB     PUB                                          
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE TABLE ENTRY                             
         LA    R3,HTBENTL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
ABYPAYCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     ABYPAYLP                                                         
*                                                                               
ABYPAYDN DS    0H                                                               
*                                                                               
ABYPAYX  DS    0H                                                               
*                                                                               
*        FIND INVOICE ELEMENTS                                                  
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         LR    R6,R4               INIT BUY ELEMENT POINTER                     
         MVI   ELCODE,PBNVELQ      SET TO FIND INVOICE ELEMENTS                 
*                                                                               
         BRAS  RE,GETEL            FIND INVOICE ELEMENT                         
*                                                                               
ABYPNVLP DS    0H                                                               
*                                                                               
         BNE   ABYPNVDN            NONE FOUND                                   
*                                                                               
         USING PBNVELMD,R6         ESTABLISH INVOICE HISTORY ELM                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PBNVLEN        GET ELEMENT LENGTH                           
*                                                                               
         CLC   PBNVSER#,QSER#      SKIP IF THIS INVOICE                         
         BE    ABYPNVCN                                                         
*                                                                               
*        ADD TO HISTORY TABLE                                                   
*                                                                               
         L     R3,HTBNXTA          POINT TO NEXT SLOT IN TABLE                  
         USING HTBENTD,R3          ESTABLISH ENTRY                              
*                                                                               
         MVC   HTBDSQN,PNVDKSQN    LINE ITEM SEQUENCE NUMBER                    
         MVI   HTBDATE,X'FF'       FORCE TO SORT LAST FOR LINE ITEM             
         MVC   HTBBDTE,PBUYKDAT    BUY LINE DATE                                
         MVC   HTBBLINE,PBUYKLIN   BUY LINE REFERENCE NUMBER                    
         MVC   HTBSER#,PBNVSER#    INVOICE SERIAL#                              
         MVC   HTBPNVSQ,PBNVDSQN   INVOICE DETAIL SEQUENCE NUMBER               
         MVC   HTBPNV#,PBNVINV#    INVOICE NUMBER                               
*                                                                               
         MVC   HTBCLT,PBUYKCLT     CLIENT                                       
         MVC   HTBPRD,PBUYKPRD     PRODUCT                                      
         MVC   HTBEST,PBUYKEST     ESTIMATE                                     
         MVC   HTBPUB,PBUYKPUB     PUB                                          
*                                                                               
         MVC   SVHTBENT,HTBENTD    SAVE TABLE ENTRY                             
         LA    R3,HTBENTL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    HTBENTD(HTBENTL),HTBENTD INIT ENTRY                              
*                                                                               
         ST    R3,HTBNXTA          UPDATE A(NEXT AVAILABLE SLOT)                
*                                                                               
ABYPNVCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT                                    
         B     ABYPNVLP                                                         
*                                                                               
ABYPNVDN DS    0H                                                               
*                                                                               
ADDBUYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF STATUSES                                                      
*                                                                               
ACCHSTAT DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
ABYSTAT  DS    0H                                                               
         DC    CL1'P',CL18'PENDING'                                             
         DC    CL1'M',CL18'MATCHED'                                             
         DC    CL1'D',CL18'DISCREPANT'                                          
         DC    X'FF'                                                            
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV40 - PRINT NEW INVOICE ACTIVITY - FILLKEY'                 
***********************************************************************         
*                                                                     *         
*        READ NEXT INVOICE KEY AND FILL IN KEY FIELDS                 *         
*                                                                     *         
* LNKINVKY  -  LONG FORM OF INVOICE KEY                               *         
*        FILLS IN VARIOUS Q FIELDS                                    *         
*        OPENS MINIO SET                                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
FILLKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         ICM   R3,15,NXTNVKYA      POINT TO NEXT INVKEY IN WKR FILE             
         BNZ   FKEY1STX                                                         
*                                  FIRST TIME                                   
         USING WKRDATD,R3          ESTABLISH WORKER FILE ENTRY                  
*                                                                               
         LHI   R3,SVINPUT-SYSD                                                  
         LA    R3,SYSD(R3)            POINT TO SAVED INPUT                      
         MVI   LNKSTSW,C'1'           INDICATE FIRST TIME                       
*                                                                               
         ST    R3,NXTNVKYA            SET STARTING KEY                          
*                                                                               
         B     FKEY10                                                           
*                                                                               
FKEY1STX DS    0H                                                               
*                                                                               
         MVI   LNKSTSW,C'2'        NOT FIRST TIME                               
*                                                                               
FKEYLOOP DS    0H                                                               
*                                                                               
FKEY10   DS    0H                                                               
*                                                                               
         XC    LNKINVKY,LNKINVKY   INIT INVOICE KEY                             
         XC    LNKHSTID,LNKHSTID   INIT HISTORY TYPE                            
         XC    QDSQN,QDSQN         INIT DETAIL SEQUENCE NUMBER                  
*                                                                               
         OC    WKDTMPCD,WKDTMPCD   DONE AT END OF DATA                          
         BZ    FKEYDONE                                                         
*                                                                               
         CLC   WKDTMPCD,=AL2(D#INVKEY)  MUST BE FOR INVKEY                      
         BNE   FKEYDONE                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BNP   FKEYDONE              NO DATA                                    
*                                                                               
         CHI   RF,L'LNKINVKY       ERROR IF TOO LONG                            
         BH    FKEYDONE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LNKINVKY(0),WKDTDATA MOVE TEXT TO WORK FIELD                     
*                                                                               
         OC    LNKINVKY,LNKINVKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE INV KEY!                           
*                                                                               
         SR    R2,R2                                                            
*                                                                               
         CLI   LNKSTSW,C'1'        SKIP IF FIRST TIME                           
         BE    FKEY35                                                           
*                                                                               
         GOTOR VGETMCP,DMCB,(L'LNKINVKY,LNKINVKY)  BREAK OUT KEY                
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
FKEY30   DS    0H                                                               
*                                                                               
         MVC   HDAMED,SPACES                                                    
         MVC   HDAMED(L'QMED),QMED                                              
         MVI   HDAMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   HDACLT,SPACES                                                    
         MVC   HDACLT(L'QCLT),QCLT                                              
         MVI   HDACLTH+5,L'QCLT                                                 
*                                                                               
*        SET INVOICE NUMBER ON SCREEN                                           
*                                                                               
         MVC   HDAINV,SPACES                                                    
         MVC   HDAINV,QINV         DISPLAY INVOICE NUMBER                       
         MVI   HDAINVH+5,L'QINV                                                 
*                                                                               
FKEYNV#X DS    0H                                                               
*                                                                               
FKEY35   DS    0H                                                               
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WKR FILE               
*                                                                               
*        SKIP IF NOT HISTORY ID                                                 
*                                                                               
         CLC   WKDTMPCD,=AL2(D#HSTYID)  SKIP IF NOT HISTID                      
         BNE   FKEYHSTX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FKEYHSTX              NO DATA                                    
*                                                                               
         CHI   RF,L'LNKHSTID       ERROR IF TOO LONG                            
         BH    FKEYCONT                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LNKHSTID(0),WKDTDATA  SAVE HISTORY TYPE                          
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WORKER FILE            
*                                                                               
FKEYHSTX DS    0H                                                               
*                                                                               
         CLC   WKDTMPCD,=AL2(D#ITMSQN)  SKIP IF NOT DETAIL SEQUENCE #           
         BNE   FKEYSQNX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BZ    FKEYSQNX              NO DATA                                    
*                                                                               
         CHI   RF,L'HDADET         ERROR IF TOO LONG                            
         BH    FKEYCONT                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QDSQN(0),WKDTDATA   SAVE DETAIL SEQUENCE NUMBER                  
*                                                                               
         MVC   HDADET,QDSQN        DISPLAY DETAIL SEQUENCE NUMBER               
         MVI   HDADETH+5,L'QDSQN                                                
*                                                                               
         AR    R3,R0               BUMP TO NEXT FIELD IN WORKER FILE            
*                                                                               
FKEYSQNX DS    0H                                                               
*                                                                               
FKEYCONT DS    0H                                                               
*                                                                               
         ST    R3,NXTNVKYA         SAVE POINTER TO NEXT FLD IN WKR              
*                                                                               
         B     FILLKEYX                                                         
*                                                                               
FKEYDONE DS    0H                                                               
*                                                                               
         MVI   LNKSTSW,C'L'        SET TO LAST INV FOUND                        
*                                                                               
FKEYLAST DS    0H                                                               
*                                                                               
FILLKEYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPPNV40 - PRINT NEW INVOICE ACTIVITY - VGETMCP'                 
***********************************************************************         
*                                                                     *         
*        FROM INVOICE KEY GET MEDIA/CLT/PUB FIELDS                    *         
*                                                                     *         
* WRKINVKY  -  MEDIA AND INVOICE SERIAL NUMBER (CHAR FORMAT)          *         
* ELEMENT   -  WILL RETURN LOOKED UP INVOICE HEADER ELEMENT           *         
*                                                                     *         
***********************************************************************         
*                                                                               
VGETMCP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         L     R2,0(R1)            POINT TO INCOMING KEYM                       
*                                                                               
         MVC   WRKINVKY,0(R2)                                                   
*                                                                               
         OC    WRKINVKY,SPACES                                                  
         CLC   WRKINVKY,SPACES                                                  
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE INVOICE KEY!                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2*L'PNVKSER#),WRKINVKY+L'QMED                               
         MVI   WORK+2*L'PNVKSER#,C'0'                                           
         PACK  DUB,WORK(2*L'PNVKSER#+1)                                         
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
         GOTOR MININIT             INIT MINIO BLOCK                             
         MVI   MINDELSW,C'Y'       NEED TO PROCESS DELETED TOO                  
*                                                                               
         LA    RE,MINMKEY          INVOICE MASTER KEY                           
         USING PNVKEY,RE                                                        
         MVC   PNVKAGY,QAGY        SET AGENCY                                   
         MVC   PNVKMED,WRKINVKY    SET MEDIA                                    
         MVI   PNVKRCD,PNVKRCDQ    SET RECORD CODE                              
         MVC   PNVKSER#,DUB+2                                                   
         MVI   PNVKELMK,X'FF'                                                   
         MVC   PNVKELMK+1(L'PNVKELMK-1),PNVKELMK                                
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING PNVHKEY,R6                                                       
         MVI   PNVHKCDE,PNVHKIDQ   SET HEADER ELM CODE                          
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         ICM   R6,15,MINELEM       POINT TO FOUND ELEMENT                       
         MVC   QINV,PNVHINV#       SAVE INVOICE NUMBER                          
*                                                                               
VGMCPX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* INITIALIZE LIOB ADDRESSES FOR GETS/PUTS                                       
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
INILIOB  L     R3,ATIA                                                          
         USING LIOBD,R3                                                         
         MVI   LIOBINDS,LIOBINRM+LIOBIMLT                                       
         ST    R9,LIOBASB1                                                      
         ST    RA,LIOBASB2                                                      
INILIOBX BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
***********************************************************************         
*                                                                     *         
*        WORKER RECORD DATA    DSECT                                  *         
*                                                                     *         
***********************************************************************         
WKRDATD  DSECT                     WORKER RECORD DATA                           
WKDTRID  DS    XL1                 RECORD ID                                    
WKDTRLEN DS    XL2                 RECORD LENGTH                                
WKDTMPCD DS    XL2                 MAP CODE                                     
WKDTTYPE DS    XL1                 DATA TYPE                                    
WKDTHDLQ EQU   *-WKRDATD           HEADER LENGTH                                
WKDTDATA DS    0C                  DATA                                         
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
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
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PPPNVFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVFED                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVFDD          INVOICE HEADER MAINT SCREEN                  
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVF8D          INVOICE HEADER ACTIVITY                      
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO40   DS    F                   RELOACTION FACTOR                            
*                                                                               
WRKCRST  DS    XL1                 CURRENT STATUS WORKAREA                      
CONTIFLG DS    CL1                 CONTINUATION FLAG                            
LSPUB    DS    CL8                 AREA FOR PUB NUMBER EBCDIC                   
CURDET#  DS    CL(L'HDADET)        AREA FOR DET SEQ #                           
CURMKEY  DS    CL(L'QINVKEY)       AREA FOR MASTER KEY                          
HDRACTEL DS    CL256               AREA FOR HEADER ACTIVITY ELEM                
WRKHSTYP DS    CL1                 HISTORY TYPE                                 
WRKACTV  DS    XL6                 WORKAREA FOR ACTIVITIES                      
WRKACTVC DS    CL18                TRANSLATED ACTIVITY                          
WRKINVKY DS    CL(L'QMED+2*L'PNVKSER#)                                          
NXTNVKYA DS    A                   A(NEXT INVOICE KEY)                          
LNKSTSW  DS    XL1                 FIRST/LAST TIME SWITCH                       
LNKHSTID DS    CL1                 TYPE OF HISTORY WANTED                       
WRKDATE  DS    CL20                WORKAREA FOR DATES                           
QINV     DS    CL(L'PNVHINV#)      INVOICE NUMBER SAVEAREA                      
SVHTBENT DS    XL(HTBENTL)         PREVIOUS TABLE ENTRY                         
SVPNVDEL DS    XL256               LINE ITEM ELEMENT SAVEAREA                   
*                                                                               
*        HISTORY TABLE                                                          
*                                                                               
         DS    0D                                                               
HTBBXLE  DS    0XL12               BXLE PARAMETERS FOR TABLE                    
HTBBXSTR DS    A                   A(START OF TABLE)                            
HTBBXLEN DS    F                   LENGTH OF TABLE ENTRY                        
HTBBXEND DS    A                   A(END OF TABLE)                              
*                                                                               
HTBNXTA  DS    A                   A(NEXT SLOT IN TABLE)                        
*                                                                               
HTBTAB   DS    0D                  START OF HISTORY TABLE                       
         DS    300XL(HTBENTL)      HISTORY TABLE                                
HTBEND   EQU   *-1                 END OF TABLE                                 
*                                                                               
         ORG                                                                    
*                                                                               
         TITLE 'PPPNV40 - PRINT NEW INVOICE ACTIVITY - HTBENTD'                 
***********************************************************************         
*                                                                     *         
*        ENTRY IN HISTORY TABLE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HTBENTD  DSECT                     ENTRY IN HISTORY TABLE                       
HTBDSQN  DS    XL2                 DETAIL SEQUENCE NUMBER                       
HTBKEYL  EQU   *-HTBENTD           KEY LENGTH                                   
HTBDATE  DS    XL3                 ACTIVITY DATE                                
HTBPID   DS    XL2                 PID                                          
HTBACT   DS    CL18                ACTIVITY                                     
HTBCLT   DS    CL3                 CLIENT CODE                                  
HTBPRD   DS    CL3                 PRODUCT CODE                                 
HTBEST   DS    XL2                 ESTIMATE CODE                                
HTBPUB   DS    XL6                 PUBCODE                                      
HTBBDTE  DS    XL3                 BUY DATE                                     
HTBBLINE DS    XL1                 BUYLINE REFERENCE NUMBER                     
HTBRNO   DS    XL1                 C'Y' IF RNO                                  
HTBSER#  DS    XL5                 INVOICE SERIAL# - PWOS                       
HTBPNVSQ DS    XL2                 INVOICE DETAIL SEQUENCE NUMBER               
HTBPNV#  DS    CL14                INVOICE NUMBER                               
*                                                                               
HTBENTL  EQU   *-HTBENTD           LENGTH OF ENTRY IN TABLE                     
*                                                                               
         TITLE 'PPPNV40 - PRINT NEW INVOICE ACTIVITY - DSECTS'                  
*                                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
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
*PPMAPEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPMAPEQUS         PRINT SYSTEM MAP CODE EQUATES                
         EJECT                                                                  
         PRINT ON                                                               
*DDLINKD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDLINKD           LINKIO DSECT                                 
         EJECT                                                                  
         PRINT ON                                                               
*DDLINKIOD                         LINKIO CONTROL BLOCK                         
LIOBD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDLINKIOD                                                      
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
***DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
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
**PAN#1  DC    CL21'048PPPNV40   10/12/17'                                      
         END                                                                    
