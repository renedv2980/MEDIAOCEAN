*          DATA SET PPPNV10    AT LEVEL 247 AS OF 06/12/18                      
*PHASE T41D10A                                                                  
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* KWAN 12/21/15 Correct invoice origin from Prisma restore            *         
*                                                                     *         
* KWAN 07/06/15 Prevent PrintPak from deleting Prisma invoices        *         
*                                                                     *         
* KWAN 03/27/15 Prevent PrintPak from changing Prisma invoices        *         
*                                                                     *         
* KWAN 08/01/14 PRISMA INVOICE                                        *         
*                                                                     *         
* KWAN 09/15/11 ELECTRONIC INVOICE                                    *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D10 - INVOICE HEADER MAINT/LIST     *                        
*                                                                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41D00 (PNV CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41DFD (MAINTENANCE)                           *         
*               SCREEN T41DFE (LIST)                                  *         
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
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST '                            
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41D10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PPPNV10X-PPPNV10D,T41D10,RR=RE                                   
*                                                                               
         LR    R0,RC               SAVE ADDRESS TO LOCAL STORAGE                
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
         ST    RE,RELO10                                                        
         ST    R0,ALOCALWS         ADDRESS OF LOCAL STORAGE AREA                
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VJESMAIL,CJESMAIL   SAVE JESMAIL ADDRESS                         
         DROP  RF                                                               
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVC   QRECORD,=CL8'INVOICE'                                            
         MVI   SVHINVSR,0          INIT INVOICE HEADER SOURCE                   
*                                                                               
*****    MVI   ERROR,0             CLEAR ERROR CODE                             
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST - CKMODE'                    
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKMODE   DS    0H                                                               
*                                                                               
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
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
         MVC   PAGEDAS+60(4),LASTLIST                                           
*                                                                               
         B     CKMODEX                                                          
*                                                                               
CKMDPFKX DS    0H                                                               
*                                                                               
CKMODE01 CLI   MODE,VALREC         VALREC?                                      
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     CKMODE10                                                         
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
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL         RECDEL?                                      
         BNE   *+12                                                             
         BRAS  RE,VDELREC                                                       
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECREST        RESREC?                                      
         JNE   CKMODE08                                                         
         BRAS  RE,RS                                                            
         CLI   SVINVSRC,INVPRM_Q   Prisma invoice?                              
         JE    CKMODE01                                                         
         CLI   SVINVSRC,INVRAD_Q   Radia invoice?                               
         JE    CKMODE01                                                         
         B     CKMODEX                                                          
*                                                                               
CKMODE08 CLI   MODE,PRINTREP       PRINT REPORT                                 
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST - VK'                        
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
         CLI   ACTNUM,ACTABDEL     ACTION ABDELETE                              
         BNE   *+8                                                              
         BRAS  RE,DK               DISPLAY THE KEY FIRST                        
*                                                                               
         MVI   CHGSWTCH,0          INITIALIZE CHANGE SWITCH                     
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT PRTDIR KEY                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         JE    VKL                                                              
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         LA    R2,HDRMEDH          POINT TO MEDIA FIELD                         
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA                               
         BE    *+8                                                              
         OI    CHGSWTCH,CHGMED        MEDIA HAS CHANGED                         
*                                                                               
         CLI   FLDILEN,0           MEDIA IS REQUIRED                            
         BE    VKMEDER                                                          
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,HDRCLTH          POINT TO CLIENT FIELD                        
*                                                                               
         GOTOR VALCLT              VALIDATE CLIENT                              
         BE    *+8                                                              
         OI    CHGSWTCH,CHGCLT        CLIENT HAS CHANGED                        
*                                                                               
         CLI   FLDILEN,0           CLIENT IS REQUIRED                           
         BE    VKCLTER                                                          
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
         LA    R2,HDRPUBH          POINT TO PUB FIELD                           
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
         BE    *+8                                                              
         OI    CHGSWTCH,CHGPUB        PUB HAS CHANGED                           
*                                                                               
         CLI   FLDILEN,0           PUB IS REQUIRED                              
         BE    VKPUBER                                                          
*                                                                               
         LA    R2,HDRINVH          POINT TO INVOICE FIELD                       
*                                                                               
         GOTOR VVALINVN            VALIDATE INVOICE #                           
*                                                                               
         CLI   FLDILEN,0           INVOICE IS REQUIRED                          
         BE    VKINVER                                                          
****     CLI   FLDILEN,11          MAX LENGTH IS 11                             
****     BH    VKINV1ER                                                         
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
         ZIC   R1,FLDILEN              R2 POINTS TO THE HEADER                  
         AHI   R1,-1                                                            
         EX    R1,VK90                                                          
         J     VK95                                                             
*                                                                               
VK90     MVC   PNV3INV#(0),HDRINV                                               
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
         CLI   ACTNUM,ACTADD       NO, ACTION ADD?                              
         JE    VK200               YES, OKAY                                    
         J     VKINV2ER            NO, RECORD NOT FOUND                         
*                                                                               
VK110    DS    0H                                                               
         CLI   SVINVSRC,INVPRM_Q   PRISMA INVOICE?                              
         JE    *+12                                                             
         CLI   SVINVSRC,INVRAD_Q   Radia invoice?                               
         JNE   VK116                                                            
         TM    PNV3CNTL,X'80'      DELETED?                                     
         JZ    VK116                                                            
         CLI   ACTNUM,ACTADD       ADD?                                         
         JNE   VK116                                                            
         MVI   MODE,RECREST        SET TO RESTORE RECORD MODE                   
         MVI   ACTNUM,ACTREST      SET TO RESTORE RECORD ACTION                 
         MVC   AIO,AIO2            READ INTO IOA2                               
         GOTOR GETREC              READ IN INVOICE MASTER RECORD                
         L     RE,AIO              POINT TO FOUND RECORD                        
         USING PNVKEY,RE           ESTABLISH MASTER RECORD                      
         MVC   QINVKEY,PNVKEY      SAVE MASTER KEY                              
         DROP  RE                                                               
         J     VK200                                                            
*                                                                               
VK116    CLI   ACTNUM,ACTADD            ACTION ADD?                             
         JE    VKDUPKER                                                         
*                                                                               
         TM    PNV3CNTL,X'80'      CHECK IF DELETED                             
         BNO   VKACT1                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BE    VKCNTCHG                                                         
*                                                                               
VKACT1   DS    0H                                                               
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
         LA    R2,HDRSER#H                                                      
*                                                                               
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),QSER#(L'QSER#+1)  UNPACK                       
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   FLDDATA(2*L'PNVKSER#),WORK   DISPLAY SERIAL NUMBER               
         MVI   FLDILEN,2*L'PNVKSER#  SET FIELD LENGTH                           
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
         MVC   KEY,SVKEY           RESTORE PRTDIR POINTERS                      
         GOTOR HIGH                                                             
*                                                                               
         J     VKX                                                              
*                                                                               
VK200    DS    0H                                                               
*****    MVC   QINVKEY,KEY         SAVE KEY INTO QINVKEY                        
*                                  CASE WHEN IT NOT FOUND ON AN ADD             
         MVC   KEY,KEYSAVE         MOVE OLD KEYSAVE INTO KEY SO                 
         J     VKX                 GENCON WILL NOT COMPLAIN                     
*                                                                               
VKL      DS    0H                  LIST KEY VALIDATION                          
*                                                                               
*                                                                               
*        SET NUMBER OF LINES AVAILABLE FOR LIST                                 
*                                                                               
         MVI   NLISTS,(HDLLINLH-HDLLIN1H)/(HDLLIN2H-HDLLIN1H)+1                 
*                                                                               
         LA    R2,HDLMEDH          HEADER OF MEDIA FEILD ON LIST SCREAN         
         OI    VALOPT,VALNAMXQ     DON'T DISPLAY MEDIA NAME IN NEXT FLD         
*                                                                               
         GOTOR VALMED                                                           
*                                                                               
         MVC   HDLMEDN(L'HDLMEDN),MEDNM                                         
         OI    HDLMEDNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
*                                                                               
*                                                                               
         LA    R2,HDLCLTH          HEADER OF CLIENT FIELD                       
         XC    QCLT,QCLT           CLEAR Q VALUE                                
*                                                                               
         XC    HDLCLTN,HDLCLTN     CLEAR NAME                                   
         OI    HDLCLTNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         JE    VKL10               CHECK NEXT FILTER                            
*                                                                               
         OI    VALOPT,VALNAMXQ     DO NOT DISPLAY NAME IN NXT FLD               
*                                                                               
         GOTOR VALCLT              VALIDATE CLT                                 
*                                                                               
         MVC   HDLCLTN(L'HDLCLTN),CLTNM                                         
         OI    HDLCLTNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
VKL10    DS    0H                                                               
         LA    R2,HDLPUBH          HEADER OF PUB    FIELD                       
         XC    QPUB,QPUB           CLEAR Q VALUE                                
*                                                                               
         XC    HDLPUBN,HDLPUBN     CLEAR NAME                                   
         OI    HDLPUBNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         JE    VKL20               CHECK NEXT FILTER                            
*                                                                               
         OI    VALOPT,VALNAMXQ     DO NOT DISPLAY NAME IN NXT FLD               
*                                                                               
         GOTOR VALPUB              VALIDATE PUB                                 
*                                                                               
         MVC   HDLPUBN(L'HDLPUBN),PUBNM                                         
         OI    HDLPUBNH+6,X'80'    DISPLAY NAME                                 
*                                                                               
VKL20    DS    0H                                                               
         LA    R2,HDLPERH          HEADER OF PERIOD FIELD                       
         XC    BSTART(L'BSTART+L'BEND),BSTART  CLEAR Q VALUE                    
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         JE    VKL30               CHECK NEXT FILTER                            
*                                                                               
         GOTOR VALPER              VALIDATE PERIOD                              
*                                                                               
         LA    R3,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R3                                                       
         MVC   HDLPER(L'PVALCPER),PVALCPER                                      
         OI    HDLPERH+6,X'80'                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
VKL30    DS    0H                                                               
         LA    R2,HDLSER#H                                                      
         XC    LQSER#,LQSER#                                                    
*                                                                               
         CLI   FLDILEN,0           CHECK IF ENTERED                             
         JE    VKX                                                              
         ZIC   R1,FLDILEN          R1=LENGTH OF ENTERED SER#                    
         AHI   R1,-1                                                            
         EX    R1,VKL40                                                         
         J     VKL50                                                            
VKL40    PACK  DUB,FLDDATA(0)                                                   
VKL50    DS    0H                                                               
         MP    DUB,=P'10'                                                       
         MVC   LQSER#,DUB+2                                                     
*                                                                               
VKX      DS    0H                                                               
         CLI   ACTNUM,ACTABDEL    IF ACTION ABDELETE                            
         BNE   *+8                                                              
         BRAS  RE,DR              DISPLAY REC                                   
*                                                                               
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
VKCNTCHG LHI   RF,PPECNTCH        CAN'T CHANGE DELETED REC                      
         J     VKERR                                                            
*                                                                               
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
*                                                                               
         LA    R2,HDRSER#H         POINT TO SERIAL NUMBER FIELD                 
         BRAS  RE,CLRFLD           CLEAR INVOICE SERIAL NUMBER FIELD            
*                                                                               
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST - VR'                        
***********************************************************************         
*                                                                     *         
*        VALIDATE INVOICE HEADER FIELDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
*        IF ACTION ABDELETE                                                     
*                                                                               
         CLI   ACTNUM,ACTABDEL     IF ACTION ABDELETE                           
         BNE   *+12                                                             
         BRAS  RE,VDELREC                                                       
         B     VRX                    ALL DONE                                  
*                                                                               
* VALIDATE INVOICE DATE                                                         
*                                                                               
         LA    R2,HDRDTEH                                                       
*                                                                               
         CLI   FLDILEN,0           REQUIRED FIELD                               
         JE    VRREQERR                                                         
*                                                                               
         GOTO1 PERVAL,DMCB,(HDRDTEH+5,HDRDTE),(X'60',WORK)                      
         LA    R3,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R3                                                       
         CLI   DMCB+4,PVRCINV1                                                  
         JE    VRDTEERR                                                         
         MVC   QDATE,PVALBSTA      SAVE INVOICE DATE   NEED TO ADD              
*                                                                               
         GOTO1 DATCON,DMCB,(3,PVALBSTA),(11,HDRDTE)                             
         OI    HDRDTEH+6,X'80'                                                  
*                                                                               
         DROP  R3                                                               
***                                                                             
* VALIDATE STATUS                                                               
***                                                                             
         LA    R2,HDRSTAH           STATUS FIELD                                
*                                                                               
         BRAS  RE,VVALSTA                                                       
***                                                                             
* VALIDATE PERIOD                                                               
***                                                                             
         LA    R2,HDRPERH            PERIOD                                     
*                                                                               
         CLI   FLDILEN,0             PERIOD IS REQUIRED                         
         JE    VRREQERR                                                         
*                                                                               
         GOTOR VALPER                VALIDATE PERIOD                            
*                                                                               
         LA    R3,WORK               PERVAL OUTPUT AREA                         
         USING PERVALD,R3                                                       
         MVC   HDRPER(L'PVALCPER),PVALCPER                                      
         OI    HDRPERH+6,X'80'                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
***                                                                             
* VALIDATE REP CODE                                                             
***                                                                             
         LA    R2,HDRREPH            SPECIAL REP                                
*                                                                               
         MVI   FLDOPT,C'Y'           FIELD IS OPTIONAL                          
*                                                                               
         GOTOR VALREP                VALIDATE REP CODE                          
*                                                                               
*                                                                               
***                                                                             
* VALIDATE CD                                                                   
***                                                                             
         LA    R2,HDRCDH                                                        
         CLI   FLDILEN,0             IF FIELD EMPTY DEFAULT N                   
         JE    VR70                                                             
         CLI   HDRCD,C'Y'            IF YES LEAVE IT ALONE                      
         JE    VR71                                                             
         CLI   HDRCD,C'N'                                                       
         JE    VR71                                                             
         J     VRCDERR                                                          
VR70     DS    0H                    DEFAULT C'N'                               
         MVI   HDRCD,C'N'                                                       
         OI    HDRCDH+6,X'80'                                                   
VR71     DS    0H                                                               
*                                                                               
***                                                                             
* VALIDATE TOTAL TYPE                                                           
***                                                                             
         LA    R2,HDRGRSH                                                       
         CLI   FLDILEN,0             IF FIELD EMPTY DEFAULT TO N                
         JE    VR75                                                             
         CLI   HDRGRS,C'G'            IF YES LEAVE IT ALONE                     
         JE    VR80                                                             
         CLI   HDRGRS,C'N'                                                      
         JE    VR80                                                             
         J     VRCDERR                                                          
VR75     DS    0H                    DEFAULT C'N'                               
         MVI   HDRGRS,C'N'                                                      
         OI    HDRGRSH+6,X'80'                                                  
*                                                                               
VR80     DS    0H                                                               
***                                                                             
* VALIDATE INVOICE MONEY                                                        
***                                                                             
*                                                                               
         LA    R2,HDRTOTH                                                       
         XC    QMONEY,QMONEY       INIT MONEY FIELD                             
*                                                                               
         CLI   FLDILEN,0             OKAY IF NOT ENTERED                        
         BE    VRTOTX                                                           
*                                                                               
         ZIC   R0,FLDILEN            MOVE LENGTH TO R0                          
*                                                                               
         GOTO1 CASHVAL,DMCB,(2,FLDDATA),(R0),0,0,0,0                            
*                                                                               
         CLI   0(R1),0               TEST FOR ERRORS                            
         JNE   VRMONERR                                                         
         L     R0,4(R1)                                                         
         C     R0,=F'999999999'      MAX IS NOW 9,999,999.99                    
         JH    VRMNMERR                                                         
*                                                                               
         CVD   R0,DUB                CONVERT R0 TO DEC                          
         ZAP   QMONEY,DUB            SAVE MONEY  (BE SMART)                     
*                                                                               
VRTOTX   DS    0H                                                               
*                                                                               
***                                                                             
* Validate invoice tax amount                                                   
***                                                                             
*                                                                               
         LA    R2,HDRTAXH                                                       
         XC    QTAX__,QTAX__       Init tax amount                              
         CLI   FLDILEN,0           Tax amount entered?                          
         JE    VRTAXX                                                           
         LLC   R0,FLDILEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLDDATA),(R0),0,0,0,0                            
         CLI   0(R1),0                                                          
         JNE   VRNUMERR                                                         
         L     R0,4(R1)                                                         
         C     R0,=F'999999999'    Max of 9,999,999.99?                         
         JH    VRMNMERR                                                         
         STCM  R0,15,QTAX__                                                     
VRTAXX   DS    0H                                                               
*                                                                               
* END OF VALIDATING ALL FIELDS ON A SCREEN                                      
*                                                                               
*                                                                               
*                                                                               
***                                                                             
*                                                                               
* BUILD MINMKEY                                                                 
*                                                                               
*                                                                               
***                                                                             
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ADDING                              
         JE    VR90                                                             
*                                                                               
         MVC   PNVKEY,QINVKEY      IF NOT ADD THEN WE ALREADY HAVE              
*                                  KEY FROM VK                                  
         J     VR95                CONTINUE WITH OPEN MINIO                     
*                                                                               
VR90     DS    0H                                                               
         MVC   PNVKAGY,QAGY        SET AGENCY                                   
         MVC   PNVKMED,QMED        SET MEDIA                                    
         MVI   PNVKRCD,PNVKRCDQ    SET RECORD CODE                              
*                                                                               
         GOTOR NXTSER#             GET SERIAL NUMBER                            
*                                                                               
         MVC   PNVKSER#,QSER#      SET SERIAL NUMBER                            
         MVC   QINVKEY,PNVKEY      SAFE MASTER KEY                              
*                                                                               
VR95     DS    0H                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    VR100                                                            
         CLI   MINERR,4            SHOULD NOT FIND ONE                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR100    DS    0H                                                               
*                                                                               
*                                                                               
VR110    DS    0H                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ADD                                 
         JE    VR150                                                            
*                                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BO    VRCNTCHG                                                         
*                                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING PNVHDRD,R3                                                       
         MVI   PNVHKEY,PNVHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,PNVHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JNZ   VR155                CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
VR150    DS    0H                   IF ADD WE LOAD ELEMENT                      
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT      CLEAR                                       
*                                                                               
VR155    DS    0H                                                               
         USING PNVHDRD,R3                                                       
         XC    QHDRCHG1,QHDRCHG1    CLEAR CHG BYTE                              
         XC    QHDRCHG2,QHDRCHG2    CLEAR CHG BYTE                              
         XC    QHDRCRST,QHDRCRST    CLEAR CURRENT STATUS                        
         MVI   PNVHKCDE,PNVHKIDQ    HEADER ELEMENT CODE                         
         MVI   PNVHKLEN,PNVHDRLQ    HEADER ELEMENT LENGTH                       
*                                                                               
         XC    PNVHINV#,PNVHINV#    CLEAR INV#                                  
         MVC   PNVHINV#,HDRINV      INVOICE NUMBER                              
         OC    PNVHINV#,SPACES                                                  
         MVC   PNVHCLT,QCLT         CLIENT   - COULD BE **                      
         MVC   PNVHPUB,QPUB         PUB CODE                                    
*                                                                               
*        ABOVE IS A DATA THAT CAN'T BE CHANGE (KEY DATA)                        
*                                                                               
         MVC   SVHINVSR,PNVHIVSR    SAVE INVOICE HEADER SOURCE                  
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ADD                                 
         JNE   VR157                                                            
         MVI   PNVHIVSR,INVADB_Q    DEFAULT TO ADBUYER                          
         CLI   SVINVSRC,C' '        HAVE INVOICE SOURCE?                        
         JNH   *+10                                                             
         MVC   PNVHIVSR,SVINVSRC    STAMP IT IN RECORD                          
         MVC   PNVHPSID,SVIVPSID    Set Prisma invoice ID                       
*                                                                               
VR157    CLI   ACTNUM,ACTADD       CHECK IF ADD                                 
         JE    VR158                                                            
         CLI   SVINVSRC,INVPRM_Q    Prisma invoice?                             
         JE    VR158                                                            
         CLI   SVINVSRC,INVRAD_Q    Radia invoice?                              
         JE    VR158                                                            
         CLI   SVHINVSR,INVPRM_Q    Prisma invoice?                             
         JE    *+12                                                             
         CLI   SVHINVSR,INVRAD_Q    Radia invoice?                              
         JNE   VR158                                                            
         LHI   RF,PPPRSMER                                                      
         J     VDRECERR                                                         
*                                                                               
VR158    CLC   PNVHDATE,QDATE       COPM DATA                                   
         BE    VR160                SKIP IF EQUAL                               
         OI    QHDRCHG2,QHDRINVD    IF NOT TURN THE BIT ON                      
         MVC   PNVHDATE,QDATE       INVOICE DATE - BINARY                       
*                                                                               
VR160    DS    0H                                                               
         CLC   PNVHSTAT,HDRSTA      INVOICE STATUS                              
         BE    VR165                SKIP IF EQUAL                               
         OI    QHDRCHG2,QHDRSTAT    IF NOT TURN THE BIT ON                      
         MVC   QHDRCRST,HDRSTA         SAVE NEW STATUS AS CURRENT               
         MVC   PNVHSTAT,HDRSTA         INVOICE STATUS                           
*                                                                               
VR165    DS    0H                                                               
         CLC   PNVHSTRT,BSTART      PERIOD START DATE - BI                      
         BNE   VR170                                                            
         CLC   PNVHEND,BEND         PERIOD END   DATE - BI                      
         BE    VR175                SKIP IF EQUAL                               
VR170    OI    QHDRCHG2,QHDRPER     IF NOT TURN THE BIT ON                      
         MVC   PNVHSTRT,BSTART      PERIOD START DATE - BI                      
         MVC   PNVHEND,BEND         PERIOD END   DATE - BI                      
*                                                                               
VR175    DS    0H                                                               
         CLC   PNVHSREP,QREP        SPECIAL REP - OPTIONAL                      
         BE    VR180                SKIP IF EQUAL                               
         OI    QHDRCHG2,QHDRSREP    IF NOT TURN THE BIT ON                      
         MVC   PNVHSREP,QREP        SPECIAL REP - OPTIONAL                      
*                                                                               
VR180    DS    0H                                                               
         CLC   PNVHCD,HDRCD         CD                                          
         BE    VR181                SKIP IF EQUAL                               
         OI    QHDRCHG2,QHDRCD      IF NOT TURN THE BIT ON                      
         MVC   PNVHCD,HDRCD         CD                                          
*                                                                               
VR181    DS    0H                                                               
         CLC   PNVH$TYP,HDRGRS      GRS                                         
         BE    VR185                SKIP IF EQUAL                               
         OI    QHDRCHG1,QHDRGRS     IF NOT TURN THE BIT ON                      
         MVC   PNVH$TYP,HDRGRS      GRS                                         
*                                                                               
VR185    DS    0H                                                               
         CLC   PNVHTOT,QMONEY       INVOICE MONEY                               
         BE    VR190                SKIP IF EQUAL                               
         OI    QHDRCHG2,QHDRTOT     IF NOT TURN THE BIT ON                      
         MVC   PNVHTOT,QMONEY       INVOICE MONEY                               
*                                                                               
VR190    DS    0H                                                               
         CLC   PNVHTOT,QTAX__       Invoice tax amount changed?                 
         JE    *+10                                                             
         MVC   PNVHTAX,QTAX__                                                   
*                                                                               
         CLI   ACTNUM,ACTADD        IF ADD DO ADD NOT WRITE                     
         JE    VR300                                                            
*                                                                               
         GOTOR WRTELM,DMCB,PNVHKEY  WRITE ELEMENT TO RECORD                     
*                                                                               
         OC    QHDRCHG1,QHDRCHG1                                                
         JNZ   VR195                                                            
         OC    QHDRCHG2,QHDRCHG2                                                
         JZ    VR310                                                            
*                                                                               
VR195    DS    0H                                                               
*                                                                               
         LA    R5,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING PNVACTHD,R5          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY-1  ACTIVITY ELEMENT LENG               
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ELEMENT                         
         JNZ   VR200                CHECK IF ITS THERE                          
*                                                                               
*        THIS CODE WILL WORK ONLY ON TST SYSTEM                                 
*        CAUSE ONLY TST HAVE RECORDS WITHOUT ACTIVITY ELEM                      
*                                                                               
         LA    R5,HDRACTEL                                                      
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         SR    R1,R1                                                            
         AHI   R1,1                                                             
         STCM  R1,3,PNVAKCSQ        STORE SEQ #                                 
         J     VR230                JUMP TO ADD ELEM LOGIC                      
*                                                                               
*        ABOVE CODE WILL ADD FIRST ACTIVITY ELEM FOR "BAD" RECORDS              
*                                                                               
VR200    DS    0H                                                               
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ELEMENT                         
         JZ    VR211                CHECK IF ITS THERE                          
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         ZIC   R1,PNVAKLEN          MOVE LENGTH TO R1                           
         AHI   R1,-1                                                            
         EX    R1,VR205                                                         
         J     VR210                                                            
VR205    DS    0H                                                               
         MVC   HDRACTEL(0),PNVAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
VR210    DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         JE    VR200                IF FOUND GO INTO LOOP                       
*                                                                               
VR211    DS    0H                                                               
*                                                                               
*        AT THIS POINT HDRACTEL HAS THE LAST ACTIVITY ELEM                      
*                                                                               
*                                                                               
         LA    R5,HDRACTEL                                                      
         CLC   PNVAHPID,SVPNVPID    CHECK IF PID MATCHED                        
         JNE   VR220                                                            
         CLC   PNVAHDTE,BTODAY      CHECK IF LAST CHANGED WAS MADE TOD          
         JNE   VR220                                                            
*                                                                               
*        SAME PERSON MAKES MORE CHANGES                                         
*                                                                               
         OC    PNVAHCH1,QHDRCHG1                                                
         OC    PNVAHCH2,QHDRCHG2                                                
*                                                                               
         TM    QHDRCHG2,PNVAHSTA  IF STATUS CHANGED                             
         JZ    *+10                                                             
         MVC   PNVACRST,QHDRCRST      SAVE CURRENT STATUS                       
*                                                                               
         XC    QHDRCHG1,QHDRCHG1                                                
         XC    QHDRCHG2,QHDRCHG2                                                
         XC    QHDRCRST,QHDRCRST                                                
*                                                                               
*        DO GETELEM AND DO A WRITE                                              
*                                                                               
         GOTOR GETELM,DMCB,PNVAKEY  GET ELEMENT TO RECORD                       
         OC    MINELEM,MINELEM                                                  
         JNZ   VR215                                                            
*                                                                               
         DC    H'0'                 MUST BE THERE                               
*                                                                               
VR215    DS    0H                                                               
*                                                                               
         GOTOR WRTELM,DMCB,PNVAKEY  GET ELEMENT TO RECORD                       
         J     VR310                                                            
VR220    DS    0H                                                               
*                                                                               
*        BUILD A NEW ACTIVITY ELEM                                              
*                                                                               
         SR    R1,R1                CLEAR R1                                    
         ICM   R1,3,PNVAKCSQ        INSERT SEQ NUMBER INTO R1                   
         AHI   R1,1                 INCREAMENT SEQ BY ONE                       
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR THE AREA                              
         STCM  R1,3,PNVAKCSQ        SEQ NUMBER INCREAMENTED BY ONE              
*                                                                               
VR230    DS    0H                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVACTLQ    ACTIVITY ELEMENT LENG                       
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
         MVC   PNVAHPID,SVPNVPID    PID OF CHANGER                              
         MVC   PNVAHDTE,BTODAY      DATE OF CHANGE - BINARY                     
*                                                                               
         TM    QHDRCHG1,PNVAHADD   IF ADDING HEADER                             
         BNO   VR231                                                            
*                                                                               
         OI    PNVAHCH1,PNVAHADD      INDICATE ADDING                           
*                                                                               
         CLI   QHDRCRST,0             IF STATUS ENTERED                         
         BE    *+14                                                             
         OI    PNVAHCH2,PNVAHSTA         INDICATE CHANGED STATUS                
         MVC   PNVACRST,QHDRCRST         SAVE CURRENT STATUS                    
*                                                                               
         B     VR232                                                            
*                                                                               
VR231    DS    0H                                                               
*                                                                               
         MVC   PNVAHCH1,QHDRCHG1                                                
         MVC   PNVAHCH2,QHDRCHG2                                                
*                                                                               
         TM    QHDRCHG2,PNVAHSTA  IF STATUS CHANGED                             
         BNO   *+10                                                             
         MVC   PNVACRST,QHDRCRST      SAVE CURRENT STATUS                       
*                                                                               
VR232    DS    0H                                                               
*                                                                               
         J     VR305                                                            
*                                                                               
         DROP  R5                                                               
*                                                                               
VR300    DS    0H                                                               
*                                                                               
         GOTOR ADDELM,DMCB,PNVHKEY  ADD HEADER ELEMENT TO RECORD                
*                                                                               
         LA    R5,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING PNVACTHD,R5          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVACTLQ    ACTIVITY ELEMENT LENG                       
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
         MVI   PNVAKCSQ+1,X'01'     SEQ NUMBER 1                                
         MVC   PNVAHPID,SVPNVPID    PID OF CHANGER                              
         MVC   PNVAHDTE,BTODAY      DATE OF CHANGE - BINARY                     
*                                                                               
*        IF ACTION IS ADD THERE IS ONLY ONE BIT THAT IS CHANGED                 
*                                                                               
         OI    PNVAHCH1,PNVAHADD    INVOICE HEADER ADDED                        
*                                                                               
         CLI   QHDRCRST,0          IF THERE IS A STATUS                         
         BE    VR305                                                            
*                                                                               
         MVC   PNVACRST,QHDRCRST      SAVE CURRENT STATUS                       
         OI    PNVAHCH2,PNVAHSTA      INDICATE STAUS CHANGED                    
*                                                                               
VR305    DS    0H                                                               
         GOTOR ADDELM,DMCB,PNVAKEY  ADD HEADER ACTIVITY ELEMENT                 
         XC    QHDRCHG1,QHDRCHG1    CLEAR                                       
         XC    QHDRCHG2,QHDRCHG2    CLEAR                                       
         XC    QHDRCRST,QHDRCRST                                                
*                                                                               
VR310    DS    0H                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         CLI   MINERR,0            SHOULD BE NO ERROR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         GOTOR PSSVS               BUILD ALL POINTERS                           
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
*                                                                               
VRX      DS    0H                                                               
         BRAS  RE,DR               RECORD VALIDATED, REDISPLAY IT               
         BRAS  RE,PRCEML           PROCESS E-MAIL                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
VRREQERR LHI   RF,PPEFLDNE         FIELD IS REQUIRED                            
         J     VRERR                                                            
*                                                                               
VRCDERR  LHI   RF,PPECDINV         INVALID CD (Y/N)                             
         J     VRERR                                                            
*                                                                               
VRDTEERR LHI   RF,PPEDTENV         INVALID SINGLE DATE                          
         J     VRERR                                                            
*                                                                               
VRMONERR LHI   RF,PPEMONNV         INVALID MONEY                                
         J     VRERR                                                            
*                                                                               
VRMNMERR LHI   RF,PPEMNMNV         MAX IS 9,999,999.99                          
         J     VRERR                                                            
*                                                                               
VRCNTCHG LA    R2,CONACTH                                                       
         LHI   RF,PPECNTCH        CAN'T CHANGE DELETED REC                      
         J     VRERR                                                            
*                                                                               
VRNUMERR LHI   RF,PPNOTNUM         Data is not a valid numeric entry            
         J     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D10 - INVOICE HEADER MAINT/LIST - DR'                        
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
***                                                                             
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
DRAC     USING PNVKEY,R4           DISPLAY ACTIVE USING                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       CHECK IF ACTION DISPLAY                      
         JNE   DR100                                                            
DR100    DS    0H                                                               
*                                                                               
         MVC   DRAC.PNVKEY,QINVKEY SHOULD HAVE KEY OF MASTER KEY                
         MVC   QSER#,DRAC.PNVKSER#                                              
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING PNVHDRD,R3                                                       
         MVI   PNVHKEY,PNVHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,PNVHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JNZ   *+6                  CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
         OC    HDRMED(L'PNVKMED),SPACES   LOWER TO UPPER CASE                   
         OI    HDRMEDH+6,X'80'                                                  
*                                                                               
         OC    HDRCLT(L'PNVHCLT),SPACES   LOWER TO UPPER CASE                   
         OI    HDRCLTH+6,X'80'                                                  
*                                                                               
         LA    R2,HDRINVH                                                       
         XC    HDRINV,HDRINV                                                    
         MVC   HDRINV(L'PNVHINV#),PNVHINV#     INVOICE NUMBER                   
         OI    HDRINVH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRDTEH                                                       
         XC    HDRDTE,HDRDTE       INVOICE DATE                                 
         GOTO1 DATCON,DMCB,(3,PNVHDATE),(11,HDRDTE)                             
         OI    HDRDTEH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRSTAH                                                       
         XC    HDRSTA,HDRSTA                                                    
         MVC   HDRSTA(L'PNVHSTAT),PNVHSTAT     INVOICE STATUS                   
         OI    HDRSTAH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRSTAH                                                       
*                                                                               
         GOTOR VDISSTA,DMCB,HDRSTA  DISPLAY STATUS NAME                         
*                                                                               
         LA    R2,HDRPERH                                                       
         XC    HDRPER,HDRPER       INVOICE PERIOD                               
         GOTO1 DATCON,DMCB,(3,PNVHSTRT),(11,HDRPER)                             
         MVI   HDRPER+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(3,PNVHEND),(11,HDRPER+9)                            
         OI    HDRPERH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         OC    PNVHSREP,PNVHSREP   CHECK IF ANYTHING THERE                      
         JZ    DR110               IF NO CLEAR FIELD                            
*                                                                               
         MVC   QREP,PNVHSREP       MOVE DATA TO QREP                            
*                                                                               
         LA    R2,HDRREPH          POINT TO THE FIELD HEADER                    
*                                                                               
         GOTOR DISREP              DISPLAY REP CODE AND NAME                    
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         J     DR120                                                            
*                                                                               
DR110    DS    0H                                                               
         LA    R2,HDRREPH          POINT TO THE FIELD HEADER                    
         XC    HDRREP,HDRREP       REP CODE                                     
         OI    HDRREPH+6,X'80'                                                  
         XC    HDRREPN,HDRREPN       REP NAME                                   
         OI    HDRREPNH+6,X'80'                                                 
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
DR120    DS    0H                                                               
         LA    R2,HDRGSTH          POINT TO THE FIELD HEADER                    
         XC    HDRGST,HDRGST       GST CODE                                     
         OI    HDRGSTH+6,X'80'                                                  
         XC    HDRPST,HDRPST       PST CODE                                     
         OI    HDRPSTH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRTOTH          POINT TO THE FIELD HEADER                    
         XC    HDRTOT,HDRTOT       MONEY                                        
         OI    HDRTOTH+6,X'80'                                                  
*                                                                               
         OC    PNVHTOT,PNVHTOT     CHECK FOR NO DATA                            
         BZ    DR121                                                            
*                                                                               
         EDIT  (P6,PNVHTOT),(12,HDRTOT),2,ALIGN=LEFT,FLOAT=-                    
*                                                                               
DR121    DS    0H                                                               
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRCDH                                                        
         XC    HDRCD,HDRCD                                                      
         MVC   HDRCD(L'PNVHCD),PNVHCD     CD                                    
         OI    HDRCDH+6,X'80'                                                   
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRGRSH                                                       
         XC    HDRGRS,HDRGRS                                                    
         MVC   HDRGRS(L'PNVH$TYP),PNVH$TYP  TOTAL TYPE                          
         OI    HDRGRSH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRTAXH          Tax amount                                   
         XC    HDRTAX,HDRTAX                                                    
         OC    PNVHTAX,PNVHTAX     Have tax amount?                             
         JZ    DR125                                                            
         EDIT  PNVHTAX,HDRTAX,2,ALIGN=LEFT,FLOAT=-                              
         OI    HDRTAXH+6,X'80'                                                  
DR125    BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRGRSH                                                       
         XC    HDRGRS,HDRGRS                                                    
         MVC   HDRGRS(L'PNVH$TYP),PNVH$TYP  TOTAL TYPE                          
         OI    HDRGRSH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
         LA    R2,HDRSER#H                POINT TO HEADER FIELD                 
*                                                                               
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),QSER#(L'QSER#+1)  UNPACK                       
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   FLDDATA(2*L'PNVKSER#),WORK   DISPLAY SERIAL NUMBER               
         MVI   FLDILEN,2*L'PNVKSER#  SET FIELD LENGTH                           
*                                                                               
         MVC   HDRSER#+13(5),=C'(ADB)'                                          
         CLI   PNVHIVSR,INVIPS_Q                                                
         JNE   *+10                                                             
         MVC   HDRSER#+13(5),=C'(IPS)'                                          
         CLI   PNVHIVSR,INVPRM_Q                                                
         JNE   *+10                                                             
         MVC   HDRSER#+13(5),=C'(PRM)'                                          
         CLI   PNVHIVSR,INVRAD_Q                                                
         JNE   *+10                                                             
         MVC   HDRSER#+13(5),=C'(RAD)'                                          
*                                                                               
         OI    FLDOIND,FOUTTRN     RE-DISPLAY SERIAL NUMBER                     
*                                                                               
         BRAS  RE,SETLEN           SET INPUT LENGTH                             
*                                                                               
*                                                                               
         J     DR200                                                            
*****                                                                           
*****                                                                           
*****                                                                           
*                                                                               
DR200    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTABDEL     ACTION ABDELETE                              
         BE    *+20                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BO    DRDELREC                                                         
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
*                                                                               
         CLI   MINERR,0             SHOULD BE NO ERROR                          
         BE    *+6                                                              
         DC    H'0'                                                             
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
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
DRDELREC LA    R2,HDRINVH                                                       
         LHI   RF,PPERECDL         RECORD IS DELETED                            
         J     DRERR                                                            
*                                                                               
DRERR    DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
STATUSTB DS    0H                                                               
         DC    CL1'P',CL10'PENDING'                                             
         DC    CL1'M',CL10'MATCHED'                                             
         DC    CL1'D',CL10'DISCREPANT'                                          
****     DC    CL1'C',CL10'CLEARED'          NOT VALID ANYMORE                  
         DC    X'FF'                                                            
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
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         CLI   ACTNUM,ACTABDEL     OR ACTION ABDELETE                           
         BNE   DK00                                                             
*                                                                               
*                                                                               
AB       USING PNVRECD,MINMKEY     ESTABLISH MASTER KEY                         
*                                                                               
         MVC   AB.PNVKAGY,QAGY        SET AGENCY CODE                           
         MVC   AB.PNVKMED,QMED        SET MEDIA                                 
         MVI   AB.PNVKRCD,PNVKRCDQ    SET RECORD CODE                           
         MVC   AB.PNVKSER#,QSER#      SET INVOICE SERIAL NUMBER                 
         B     DK05                                                             
*                                                                               
         DROP  AB                                                               
*                                                                               
*                                                                               
DK00     DS    0H                                                               
         MVC   MINMKEY(L'PNVKEY),KEY  MOVE KEY TO MASTER KEY                    
         MVC   QINVKEY(L'PNVKEY),KEY  SAVE KEY  IN Q AREA                       
         MVC   QMED,KEY+L'QAGY                                                  
*                                                                               
*                                                                               
DK05     DS    0H                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    DK10                                                             
         DC    H'0'                                                             
*                                                                               
DK10     DS    0H                                                               
*                                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING PNVHDRD,R3                                                       
         MVI   PNVHKEY,PNVHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,PNVHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JNZ   DK20                 CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
DK20     DS    0H                                                               
*                                                                               
         MVC   QCLT,PNVHCLT                                                     
         MVC   QPUB,PNVHPUB                                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***                                                                             
* DISPLAY MEDIA                                                                 
***                                                                             
         LA    R2,HDRMEDH          POINT TO MEDIA FIELD HEADER                  
*                                                                               
         GOTOR DISMED              DISPLAY MED AND MEDIA NAME                   
*                                                                               
***                                                                             
* DISPLAY CLIENT                                                                
***                                                                             
         LA    R2,HDRCLTH          POINT TO CLIENT FIELD HEADER                 
*                                                                               
         GOTOR DISCLT              DISPLAY CLT AND CLIENT NAME                  
*                                                                               
***                                                                             
* DISPLAY PUB NUMBER                                                            
***                                                                             
         LA    R2,HDRPUBH          POINT TO PUB FIELD HEADER                    
*                                                                               
         GOTOR DISPUB              DISPLAY PUB AND PUB NAME                     
*                                                                               
         MVC   HDRINV(L'PNVHINV#),PNVHINV#                                      
         OI    HDRINVH+6,X'80'                                                  
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
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
         LA    R3,KEY                                                           
         OC    KEY,KEY                                                          
         BNZ   LR15                CONTINUE LISTING                             
         XC    KEY,KEY                                                          
         USING PNV2KEYD,R3         PASSIVE POITER                               
*                                                                               
         MVC   PNV2AGY,QAGY        AGENCY                                       
         MVC   PNV2MED(L'PNV2MED),QMED                                          
         MVI   PNV2RCD,PNV2RCDQ    X'B2' PERIOD PASSIVE                         
*                                                                               
         DROP  R3                                                               
*                                                                               
LR10     DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                FIRST RECORD                                 
         J     LR30                                                             
*                                                                               
LR15     DS    0H                                                               
         MVC   KEY,LSSVKEY                                                      
         J     LR10                                                             
*                                                                               
LR20     OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR SEQ                 NEXT RECORD                                  
*                                                                               
LR30     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         JNE   LR37                                                             
*                                                                               
         OC    QCLT,QCLT           CHECK IF CLT FILTER IS THERE                 
         JNZ   LR33                NO CONTINUE                                  
*                                                                               
LR31     DS    0H                                                               
         OC    QPUB,QPUB           CHECK IF PUB FILTER IS THERE                 
         JNZ   LR34                NO CONTINUE                                  
*                                                                               
LR32     DS    0H                                                               
         OC    BSTART,BSTART       CHECK IF PERIOD FILTER IS THERE              
         JNZ   LR35                NO CONTINUE                                  
         J     LR40                                                             
*                                                                               
LR33     DS    0H                                                               
         CLC   KEY+PNV2CLT-PNV2KEY(L'QCLT),QCLT                                 
         JNE   LR20                GO TO SEQ                                    
         J     LR31                IF MATCH CONTINUE                            
*                                                                               
LR34     DS    0H                                                               
         CLC   KEY+PNV2PUB-PNV2KEY(L'QPUB),QPUB                                 
         JNE   LR20                GO TO SEQ                                    
         J     LR32                IF MATCH CONTINUE                            
*                                                                               
LR35     DS    0H                                                               
         CLC   KEY+PNV2SDTE-PNV2KEY(L'BSTART),BEND                              
         JH    LR20                GO TO SEQ                                    
         CLC   KEY+PNV2EDTE-PNV2KEY(L'BEND),BSTART                              
         JL    LR20                GO TO SEQ                                    
         J     LR40                IF MATCH CONTINUE                            
*                                                                               
LR37     DS    0H                                                               
         XC    KEY,KEY                                                          
         J     LRX                                                              
*                                                                               
LR40     DS    0H                                                               
*                                                                               
         MVC   LSSVKEY,KEY         SAVE KEY FOR READHI LATER ON                 
*                                                                               
         GOTOR GETREC              READ A RECORD PURPOSE IS TO GET              
         CLI   DMCB+8,2                                                         
         JE    *+14                                                             
         CLI   DMCB+8,0            MASTER KEY                                   
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    LQSER#,LQSER#                                                    
         JZ    LR50                                                             
*                                                                               
         CLC   KEY+20(L'QSER#),LQSER#   COMP FILTER FOR SER#                    
         JNL   LR50                     IF HIGHER GO AHEAD                      
         MVC   KEY,LSSVKEY              RESTORE KEY                             
         GOTOR HIGH                     READHI                                  
         J     LR20                     SEQ                                     
*                                                                               
LR50     DS    0H                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         L     R3,AIO                                                           
         MVC   MINMKEY(L'PNVKEY),0(R3)                                          
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
*                                                                               
* OPEN MINIO SET                                                                
*                                                                               
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    LR100                                                            
         DC    H'0'                                                             
*                                                                               
LR100    DS    0H                                                               
*                                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR                                       
         USING PNVHDRD,R3                                                       
         MVI   PNVHKEY,PNVHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,PNVHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         JNZ   LR110                CHECK IF ITS THERE                          
         DC    H'0'                                                             
*                                                                               
LR110    DS    0H                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         XC    LISTAR(DLISTEQU),LISTAR                                          
         USING DLISTD,R5                                                        
*                                                                               
         MVI   DLISTSTT,C' '                                                    
*                                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BNO   *+8                                                              
         MVI   DLISTSTT,C'D'                                                    
******   MVI   MINSTAT,X'00'                                                    
*                                                                               
*                                                                               
******   MVC   DLISTCLI,PNVHCLT     CLIENT                                      
         MVC   DLISTCLI,LSSVKEY+PNV2CLT-PNV2KEY                                 
*                                                                               
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,LSSVKEY+PNV2PUB-PNV2KEY),(C'S',LSPUB)           
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         JNE   LR120                                                            
         DC    H'0'                SHOULD HAVE VALID PUB CODE                   
*                                                                               
LR120    DS    0H                                                               
*                                                                               
         GOTOR DISPBNM,DMCB,PNVHPBCD,DLISTPBN                                   
*                                                                               
         MVC   DLISTPUB,LSPUB                                                   
         MVC   DLISTINV,PNVHINV#                                                
*                                                                               
*******BUG01    CLC   DLISTINV(5),=C'K1234'                                     
*******         BNE   *+6                                                       
*******         DC    H'0'                                                      
*                                                                               
***                                                                             
***      INPUT INDICATORS                                                       
***            X'80' - RETURN DATE IN P2(1)                                     
***            X'10' - START AND END DATES GIVEN                                
***            X'03' - DATES IN BINARY FORMAT                                   
***      OUTPUT INDICATORS                                                      
***            17    - MMMDD/YY-MMMDD/YY                                        
***                                                                             
*                                                                               
         GOTO1 DATCON,PNVPARMS,(X'93',PNVHSTRT),(17,DLISTPER)                   
*                                                                               
         OC    LQSER#,LQSER#                                                    
         JZ    LR130                                                            
*                                                                               
         XC    DLISTPER,DLISTPER                                                
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'QSER#+1),PNVKSER#(L'QSER#+1)  UNPACK                    
         MVI   WORK+2*L'QSER#,C' '  KILL EXTRA BYTE                             
         MVC   DLISTPER(2*L'PNVKSER#),WORK   DISPLAY SERIAL NUMBER              
*                                                                               
LR130    DS    0H                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
*                                                                               
         DROP  R3                                                               
         DROP  R5                                                               
*                                                                               
         MVC   KEY,LSSVKEY                                                      
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMDSKADD,KEY+27                                                  
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
         B     LR20                                                             
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
RS       NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKEY,QINVKEY      MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    RS05                                                             
         DC    H'0'                                                             
*                                                                               
RS05     DS    0H                                                               
         TM    MINSTAT,MINDELQ     CHECK IF DELETED                             
         BNO   RSMSTBDL                                                         
*                                                                               
         GOTO1 VMINIO,DMCB,('MINRSF',(R7))                                      
         CLI   MINERR,0                                                         
         BE    RS09                                                             
         CLI   MINERR,MINENDEL     RECORD SET NOT DELETED, 2 USERS              
         BE    RSMSTBDL                                                         
         DC    H'0'                                                             
*                                                                               
*                                                                               
RS09     DS    0H                                                               
         GOTOR PSSVS               IT WILL RESTORE PASSIVE POINTER              
*                                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
*                                                                               
*        RE-OPEN AND CREATE ACTIVITY ELEM                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKEY,QINVKEY      MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   SVINVSRC,C' '        Have invoice source?                        
         JNH   RS09_30                                                          
         LA    R5,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING PNVHDRD,R5                                                       
         MVI   PNVHKEY,PNVHKIDQ     Header elemet ID                            
         GOTOR GETELM,DMCB,PNVHKEY                                              
         ICM   R5,15,MINELEM                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PNVHIVSR,SVINVSRC    Reset to correct invoice origin             
         GOTOR WRTELM,DMCB,PNVHKEY  Write element to record                     
         DROP  R5                                                               
*                                                                               
*        CREATE A RESTORE ACTIVITY FOR INVOICE                                  
*                                                                               
RS09_30  LA    R5,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING PNVACTHD,R5          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY-1  ACTIVITY ELEMENT LENG               
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ACTIVITY ELEM                   
         CLI   PNVAKCDE,X'00'                                                   
         JNE   RS10                 CHECK IF ITS THERE                          
*                                                                               
*        THIS CODE WILL WORK ONLY ON TST SYSTEM                                 
*        CAUSE ONLY TST HAVE RECORDS WITHOUT ACTIVITY ELEM                      
*                                                                               
         LA    R5,HDRACTEL                                                      
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         SR    R1,R1                                                            
         AHI   R1,1                                                             
         STCM  R1,3,PNVAKCSQ        STORE SEQ #                                 
         J     RS50                 JUMP TO ADD ELEM LOGIC                      
*                                                                               
*        ABOVE CODE WILL ADD FIRST ACTIVITY ELEM FOR "BAD" RECORDS              
*                                                                               
RS10     DS    0H                                                               
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ELEMENT                         
         CLI   PNVAKCDE,X'00'                                                   
         JE    RS40                 CHECK IF ITS THERE                          
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         ZIC   R1,PNVAKLEN          MOVE LENGTH TO R1                           
         AHI   R1,-1                                                            
         EX    R1,RS20                                                          
         J     RS30                                                             
RS20     DS    0H                                                               
         MVC   HDRACTEL(0),PNVAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
RS30     DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         JE    RS10                 IF FOUND GO INTO LOOP                       
*                                                                               
RS40     DS    0H                                                               
*                                                                               
*        BUILD A NEW ACTIVITY ELEM                                              
*                                                                               
         LA    R5,HDRACTEL                                                      
*                                                                               
         SR    R1,R1                CLEAR R1                                    
         ICM   R1,3,PNVAKCSQ        INSERT SEQ NUMBER INTO R1                   
         AHI   R1,1                 INCREAMENT SEQ BY ONE                       
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR THE AREA                              
         STCM  R1,3,PNVAKCSQ        SEQ NUMBER INCREAMENTED BY ONE              
*                                                                               
*                                                                               
RS50     DS    0H                                                               
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVACTLQ    ACTIVITY ELEMENT LENG                       
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
         MVC   PNVAHPID,SVPNVPID    PID OF CHANGER                              
         MVC   PNVAHDTE,BTODAY      DATE OF CHANGE - BINARY                     
*                                                                               
         OI    PNVAHCH1,PNVAHRST                                                
*                                                                               
         GOTOR ADDELM,DMCB,PNVAKEY  ADD HEADER ACTIVITY ELEMENT                 
*                                                                               
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         CLI   SVINVSRC,INVPRM_Q   Prisma invoice?                              
         JE    *+12                                                             
         CLI   SVINVSRC,INVRAD_Q   Radia invoice?                               
         JNE   RS60                                                             
         MVI   MODE,VALREC         Go validate record                           
         MVI   ACTNUM,ACTCHA       Set mode back to CHANGE record               
         J     RSX                                                              
*                                                                               
RS60     CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         JNE   RSX                                                              
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
RSX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
RSMSTBDL LA    R2,CONACTH                                                       
         LHI   RF,PPEMSTDL         CAN'T RESTORE A LIVE RECORD                  
         J     RSRECERR                                                         
*                                                                               
RSRECERR DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
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
DISPBNM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         L     R3,0(R1)            LOAD ADDRESS OF PUB CODE                     
         L     R5,4(R1)            LOAD ADDRESS OF OUTPUT AREA                  
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),0(R3)    MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   0(L'DLISTPBN,R5),PUBNAME                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'MYKEY),MYKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY(L'MYKEY),MYKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
***********************************************************************         
VDELREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
***                                                                             
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
DREC0003 MVC   PNVKEY,QINVKEY      MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    DREC0005                                                         
         DC    H'0'                                                             
*                                                                               
DREC0005 DS    0H                                                               
*                                                                               
         TM    MINSTAT,MINDELQ                                                  
         BO    DRCNTDEL                                                         
*                                                                               
         CLI   SVINVSRC,INVPRM_Q    Prisma invoice?                             
         JE    DREC0007                                                         
         CLI   SVINVSRC,INVRAD_Q    Radia invoice?                              
         JE    DREC0007                                                         
*                                                                               
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING PNVHDRD,R3                                                       
         MVI   PNVHKEY,PNVHKIDQ     Inoice header elem ID                       
         GOTOR GETELM,DMCB,PNVHKEY                                              
         ICM   R3,15,MINELEM        Have invoice header?                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SVHINVSR,PNVHIVSR    Save invoice header source                  
         DROP  R3                                                               
*                                                                               
         CLI   SVHINVSR,INVPRM_Q    Prisma invoice?                             
         JE    *+12                                                             
         CLI   SVHINVSR,INVRAD_Q    Radia invoice?                              
         JNE   DREC0007                                                         
         LHI   RF,PPPRSMER                                                      
         J     VDRECERR                                                         
*                                                                               
DREC0007 LA    R3,ELEMENT          BUILD ELM                                    
         XC    ELEMENT,ELEMENT     CLEAR ELM AREA                               
         USING PNVDTLD,R3          DETAIL ELEM DSECT                            
         MVI   PNVDKEY,PNVDKIDQ    ELEM ID                                      
         MVI   PNVDKLEN,1          RESET LENGTH TO ONE                          
*                                                                               
         GOTOR GETELM,DMCB,PNVDKEY GET ELEMENT TO RECORD                        
*                                                                               
         ICM   R3,15,MINELEM       POINT R3 TO ELEMENT                          
         CLI   PNVDKEY,X'00'                                                    
         BE    DREC0050            CHECK IF ITS THERE                           
*                                                                               
DREC0010 DS    0H                                                               
         CLI   PNVDKTYP,PNVDKDSQ   CHECK IF DETAIL ELEM                         
         BNE   DREC0015                                                         
         TM    PNVDSTAT,PNVDDLQ    CHECK IF DELETED                             
         BNO   DREC0020                                                         
*                                                                               
DREC0015 DS    0H                                                               
         MVI   PNVDKLEN,1          RESET LENGTH TO ONE                          
*                                                                               
         GOTOR NXTELM,DMCB,PNVDKEY GET ELEMENT TO RECORD                        
*                                                                               
         ICM   R3,15,MINELEM       POINT R3 TO ELEMENT                          
         CLI   PNVDKEY,X'00'                                                    
         BNE   DREC0010            CHECK IF ITS THERE                           
         B     DREC0050                                                         
*                                                                               
DREC0020 DS    0H                                                               
         EDIT  PNVDKSQN,(5,ELEMENT),0,ALIGN=LEFT                                
         LA    RF,ELEMENT                                                       
         STCM  RF,7,PTXTADR        SET ADR FOR ERROR                            
         MVI   PTXTLEN,6           SET LENGTH                                   
         B     DRECNDEL            GO TO ERROR ROUTINE                          
*                                                                               
DREC0050 DS    0H                                                               
*        CREATE A DELETE ACTIVITY FOR INVOICE                                   
*                                                                               
*                                                                               
         LA    R5,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING PNVACTHD,R5          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVAKCSQ-PNVAKEY-1  ACTIVITY ELEMENT LENG               
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ACTIVITY ELEM                   
         CLI   PNVAKCDE,X'00'                                                   
         JNE   DREC0060             CHECK IF ITS THERE                          
*                                                                               
*        THIS CODE WILL WORK ONLY ON TST SYSTEM                                 
*        CAUSE ONLY TST HAVE RECORDS WITHOUT ACTIVITY ELEM                      
*                                                                               
         LA    R5,HDRACTEL                                                      
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         SR    R1,R1                                                            
         AHI   R1,1                                                             
         STCM  R1,3,PNVAKCSQ        STORE SEQ #                                 
         J     DREC0100             JUMP TO ADD ELEM LOGIC                      
*                                                                               
*        ABOVE CODE WILL ADD FIRST ACTIVITY ELEM FOR "BAD" RECORDS              
*                                                                               
DREC0060 DS    0H                                                               
*                                                                               
         ICM   R5,15,MINELEM        POINT R5 TO ELEMENT                         
         CLI   PNVAKCDE,X'00'                                                   
         JE    DREC0090             CHECK IF ITS THERE                          
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR SAVE AREA                             
         ZIC   R1,PNVAKLEN          MOVE LENGTH TO R1                           
         AHI   R1,-1                                                            
         EX    R1,DREC0070                                                      
         J     DREC0080                                                         
DREC0070 DS    0H                                                               
         MVC   HDRACTEL(0),PNVAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
DREC0080 DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         JE    DREC0060             IF FOUND GO INTO LOOP                       
*                                                                               
DREC0090 DS    0H                                                               
*                                                                               
*        BUILD A NEW ACTIVITY ELEM                                              
*                                                                               
         LA    R5,HDRACTEL                                                      
*                                                                               
         SR    R1,R1                CLEAR R1                                    
         ICM   R1,3,PNVAKCSQ        INSERT SEQ NUMBER INTO R1                   
         AHI   R1,1                 INCREAMENT SEQ BY ONE                       
*                                                                               
         XC    HDRACTEL,HDRACTEL    CLEAR THE AREA                              
         STCM  R1,3,PNVAKCSQ        SEQ NUMBER INCREAMENTED BY ONE              
*                                                                               
*                                                                               
DREC0100 DS    0H                                                               
*                                                                               
         MVI   PNVAKCDE,PNVAKHDQ    HEADER    ACTIVITY                          
         MVI   PNVAKLEN,PNVACTLQ    ACTIVITY ELEMENT LENG                       
         MVI   PNVAKACT,PNVAKACQ    ACTIVITY ELEMENT HEADER CODE                
         MVC   PNVAHPID,SVPNVPID    PID OF CHANGER                              
         MVC   PNVAHDTE,BTODAY      DATE OF CHANGE - BINARY                     
*                                                                               
         OI    PNVAHCH1,PNVAHDEL                                                
*                                                                               
         GOTOR ADDELM,DMCB,PNVAKEY  ADD HEADER ACTIVITY ELEMENT                 
*                                                                               
*        NEEDS TO CLOSED BEFORE DELETED                                         
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*****************************************************************               
*****************************************************************               
*****************************************************************               
*        RE-OPEN MINIO SET AND DO DELETE                                        
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKEY,QINVKEY      MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         GOTO1 VMINIO,DMCB,('MINDLF',(R7))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         BRAS  RE,DELPSSV          DELETE PASSIVE POINTERS                      
*                                                                               
*        CALL LINKIO INTERFACE IF NEEDED                                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF IN A LINK CALL                            
         BNE   DRECLNKX                                                         
*                                                                               
         GOTOR LNKPUT,DMCB,(RC)       SEND DATA BACK TO CALLER                  
*                                                                               
DRECLNKX DS    0H                                                               
*                                                                               
VDELRECX DS    0H                                                               
         XIT1                                                                   
*                                                                               
DRCNTDEL LHI   RF,PPERECDL                                                      
         J     VDRECERR                                                         
*                                                                               
DRECNDEL LHI   RF,PPEDETDL         DETAILS MUST BE ALL DELETED                  
         J     VDRECERR                                                         
*                                                                               
VDRECERR DS    0H                                                               
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
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
*                                                                               
***********************************************************************         
*        DELPSSV- DELETE PASSIVE KEYS                                           
*                                                                               
*               - ON INPUT R7 POINTS TO MINIOBLOCK                              
***********************************************************************         
*                                                                               
DELPSSV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE MASTER KEY                 
         USING PNVKEY,R4                                                        
*                                                                               
         MVC   PNVKEY,QINVKEY      MOVE MASTER KEY                              
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         GOTOR VMINIO,PNVPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         JE    DPSS0005                                                         
         DC    H'0'                                                             
*                                                                               
DPSS0005 DS    0H                                                               
         LA    R3,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR ELM AREA                              
         USING PNVHDRD,R3           INVOICE HEADER ELEM                         
         MVI   PNVHKEY,PNVHKIDQ     ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,PNVHKEY  GET ELEMENT TO RECORD                       
*                                                                               
         ICM   R3,15,MINELEM        POINT R3 TO ELEMENT                         
         CLI   PNVHKEY,X'00'                                                    
         BNE   DPSS0010                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
DPSS0010 DS    0H                                                               
         BRAS  RE,BLDPSSV                                                       
*                                                                               
         L     R3,MINELEM          RESTORE R3                                   
*                                                                               
         XC    KEY,KEY             GET PASS1 KEY                                
         LA    R4,KEY              ESTABLISH PASS1 RECORD KEY                   
         USING PNV1KEY,R4                                                       
*                                                                               
         MVC   KEY(L'XKEY1),XKEY1  MOVE KEY OF PASS1 TO THE KEY                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE     RECORD MUST BE FOUND                         
         BNE   DPSSNFND                                                         
*                                                                               
         TM    PNV1CNTL,X'80'      CHECK IF ITS ALREADY DELETED                 
         BO    DPSS0015                                                         
*                                                                               
         OI    PNV1CNTL,X'80'      TURN DELETE BIT ON                           
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
DPSS0015 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             GET PASS2 KEY                                
         LA    R4,KEY              ESTABLISH PASS2 RECORD KEY                   
         USING PNV2KEY,R4                                                       
*                                                                               
         MVC   KEY(L'XKEY2),XKEY2  MOVE KEY OF PASS2 TO THE KEY                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE     PASSIVE MUST BE FOUND                        
         BNE   DPSSNFND                                                         
*                                                                               
         TM    PNV2CNTL,X'80'      CHECK IF ITS ALREADY DELETED                 
         BO    DPSS0020                                                         
*                                                                               
         OI    PNV2CNTL,X'80'      TURN BIT ON                                  
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
DPSS0020 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             GET PASS3 KEY                                
         LA    R4,KEY              ESTABLISH PASS3 RECORD KEY                   
         USING PNV3KEY,R4                                                       
*                                                                               
         MVC   KEY(L'XKEY3),XKEY3  MOVE KEY OF PASS3 TO THE KEY                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE     PASSIVE MUST BE FOUND                        
         BNE   DPSSNFND                                                         
*                                                                               
         TM    PNV3CNTL,X'80'      CHECK IF ITS ALREADY DELETED                 
         BO    DPSS0025                                                         
*                                                                               
         OI    PNV3CNTL,X'80'      TURN BIT ON                                  
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
DPSS0025 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             GET PASS6 KEY                                
         LA    R4,KEY              ESTABLISH PASS6 RECORD KEY                   
         USING PNV6KEY,R4                                                       
*                                                                               
         MVC   KEY(L'XKEY6),XKEY6  MOVE KEY OF PASS6 TO THE KEY                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE     HAVE CREATION PASSIVE?                       
         JNE   DPSS0030            OLD INVOICES MIGHT NOT HAVE IT               
*                                                                               
         TM    PNV6CNTL,X'80'      CHECK IF ITS ALREADY DELETED                 
         BO    DPSS0030                                                         
*                                                                               
         OI    PNV6CNTL,X'80'      TURN BIT ON                                  
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
DPSS0030 DS    0H                                                               
*                                                                               
*                                                                               
DELPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DPSSNFND LHI   RF,PPEPSSNF         DETAILS MUST BE ALL DELETED                  
         J     DPSSERR                                                          
*                                                                               
DPSSERR  DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR         CLEAR ERROR FIELD                            
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
***********************************************************************         
*        RSTPSSV- RESTORE PASSIVE KEYS                                          
*                                                                               
*               - ON INPUT R7 POINTS TO MINIOBLOCK                              
***********************************************************************         
*                                                                               
RSTPSSV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        XC    MINEKEY,MINEKEY                                                  
*        MVI   MINEKEY,SNVMMELQ                                                 
*        BRAS  RE,MINIOHI                                                       
*        CLI   MINERR,0                                                         
*        BNE   RSTPX                                                            
*                                                                               
*        L     R3,MINELEM                                                       
*        BRAS  RE,BLDPSSV                                                       
*                                                                               
*        GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY2,XKEY3,0         
*        CLC   XKEY2(SNVKMINK-SNVKEY),XKEY3                                     
*        BNE   RSTPX                                                            
*        NI    XKEY3+32,X'7F'                                                   
*        GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEY3,XKEY3,0                  
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
RSTPX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DROP                                                                   
*                                                                               
***********************************************************************         
***********************************************************************         
* BLDPSSV - R3 IS EXPECTED TO ADDRESS INVOICE HEADER ELEM                       
* PASSIVE KEY IS BUILT IN XKEY1                                                 
* PASSIVE KEY IS BUILT IN XKEY2                                                 
* PASSIVE KEY IS BUILT IN XKEY3                                                 
* PASSIVE KEY IS BUILT IN XKEY6 - CREATION DATE PASSIVE                         
***********************************************************************         
***********************************************************************         
*                                                                               
BLDPSSV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING PNVHDRD,R3                                                       
*                                                                               
         XC    XKEY1,XKEY1                                                      
         XC    XKEY2,XKEY2                                                      
         XC    XKEY3,XKEY3                                                      
         XC    XKEY6,XKEY6         INVOICE CREATION DATE PASSIVE                
*                                                                               
         LA    R5,XKEY1                                                         
         USING PNV1KEYD,R5                                                      
*                                                                               
         MVC   PNV1AGY,QAGY                                                     
         MVC   PNV1MED,QMED                                                     
         MVI   PNV1RCD,PNV1RCDQ                                                 
         MVC   PNV1CLT,QCLT                                                     
         MVC   PNV1PUB,QPUB                                                     
         MVC   PNV1SER#,QSER#                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R5,XKEY2                                                         
         USING PNV2KEYD,R5                                                      
*                                                                               
         MVC   PNV2AGY,QAGY                                                     
         MVC   PNV2MED,QMED                                                     
         MVI   PNV2RCD,PNV2RCDQ                                                 
         MVC   PNV2CLT,QCLT                                                     
         MVC   PNV2PUB,QPUB                                                     
         MVC   PNV2SDTE,PNVHSTRT                                                
         MVC   PNV2EDTE,PNVHEND                                                 
         MVC   PNV2SER#,QSER#                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R5,XKEY3                                                         
         USING PNV3KEYD,R5                                                      
*                                                                               
         MVC   PNV3AGY,QAGY                                                     
         MVC   PNV3MED,QMED                                                     
         MVI   PNV3RCD,PNV3RCDQ                                                 
         MVC   PNV3CLT,QCLT                                                     
         MVC   PNV3PBCD,QPUB                                                    
         MVC   PNV3INV#,PNVHINV#                                                
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R5,XKEY6                                                         
         USING PNV6KEYD,R5                                                      
*                                                                               
         MVC   PNV6AGY,QAGY                                                     
         MVC   PNV6MED,QMED                                                     
         MVI   PNV6RCD,PNV6RCDQ                                                 
         MVC   PNV6CDTE,BTODAY                                                  
         MVC   PNV6SRCE,PNVHIVSR                                                
         MVC   PNV6CLT,QCLT                                                     
         MVC   PNV6PUB,QPUB                                                     
         MVC   PNV6SER#,QSER#                                                   
*                                                                               
         DROP  R5                                                               
BLDPX    DS    0H                                                               
         XIT1                                                                   
         DROP                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
***********************************************************************         
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
*                                             *** *** *** * *** *** ***         
*                                             *** *** *** * *** *** ***         
*        VALIDATE STATUS                      *** *** *** * *** *** ***         
*                                                         *                     
*                                             *** *** *** * *** *** ***         
*                                             *** *** *** * *** *** ***         
*NTRY    R2==> STATUS FIELD ON SCREEN                     *                     
*                                             *** *** *** * *** *** ***         
*                                             *** *** *** * *** *** ***         
*                                             *** *** *** * *** *** ***         
***********************************************************************         
VVALSTA  NTR1  BASE=*,LABEL=*                                                   
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
         CLI   FLDILEN,1              INPUT SHOULD BE 1 CHAR                    
         JNE   VSTAINVE                                                         
         L     R3,=A(STATUSTB)          POINT R3 TO TABLE                       
         A     R3,RELO10                                                        
         USING STATUSTD,R3                                                      
*                                                                               
VALSTA10 DS    0H                                                               
         CLI   STATCODE,X'FF'       END OF TABLE?                               
         JE    VSTAINVE                                                         
         CLC   FLD(L'STATCODE),STATCODE                                         
         JE    VALSTA20                                                         
         LA    R3,STATEQU(R3)       BUMP TO THE ENTRY                           
         J     VALSTA10                                                         
*                                                                               
VALSTA20 DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP                                                          
*                                                                               
         BRAS  RE,CLRFLD                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN                                                       
*                                                                               
         CHI   RF,L'STATNAME                                                    
         BNH   *+8                                                              
         LHI   RF,L'STATNAME                                                    
*                                                                               
         STC   RF,FLDOLEN                                                       
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),STATNAME                                              
*                                                                               
         XIT1                                                                   
*                                                                               
VSTAINVE DS    0H                                                               
         XC    ERROR,ERROR                                                      
         LHI   RF,PPESTANV     INVALID STATUS CODE                              
         B     VSTAERR                                                          
*                                                                               
*                                                                               
VSTAERR  DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DISPLAY STATUS NAME                                                    
*                                                                               
*                                                                               
*                                                                     *         
*NTRY    R2==> STATUS FIELD ON SCREEN                                 *         
*                                                                     *         
*EXIT                                                                           
***********************************************************************         
VDISSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
*        GOTOR GETFLD              READ STATUS INTO WORKAREA                    
*                                                                               
         L     R6,0(R1)            POINT TO STATUS CODE ON SCREEN               
*                                                                               
         L     R3,=A(STATUSTB)          POINT R3 TO TABLE                       
         A     R3,RELO10                                                        
         USING STATUSTD,R3                                                      
*                                                                               
DISSTA10 DS    0H                                                               
         CLI   STATCODE,X'FF'       END OF TABLE?                               
         JE    DSTAINVE                                                         
         CLC   0(L'STATCODE,R6),STATCODE                                        
         JE    DISSTA20                                                         
         LA    R3,STATEQU(R3)       BUMP TO THE ENTRY                           
         J     DISSTA10                                                         
*                                                                               
DISSTA20 DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP                                                          
*                                                                               
         BRAS  RE,CLRFLD                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN                                                       
*                                                                               
         CHI   RF,L'STATNAME                                                    
         BNH   *+8                                                              
         LHI   RF,L'STATNAME                                                    
*                                                                               
         STC   RF,FLDOLEN                                                       
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),STATNAME                                              
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
         XIT1                                                                   
*                                                                               
DSTAINVE DS    0H                                                               
         XC    ERROR,ERROR                                                      
         LHI   RF,PPESTANV     INVALID STATUS CODE                              
         B     DSTAERR                                                          
*                                                                               
*                                                                               
DSTAERR  DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
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
***********************************************************************         
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
***********************************************************************         
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
PRCEML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         J     PRCEML_X            Disable e-mail for now                       
*                                                                               
         CLI   SVINVSRC,INVPRM_Q   Prisma invocie?                              
         JE    PRCEML_X                                                         
         CLI   SVINVSRC,INVRAD_Q   Radia invoice?                               
         JE    PRCEML_X                                                         
*                                                                               
         CLI   SVHINVSR,INVPRM_Q   Prisma invoice?                              
         JE    *+12                                                             
         CLI   SVHINVSR,INVRAD_Q   Radia invoice?                               
         JNE   PRCEML_X                                                         
*                                                                               
         CLI   ACTNUM,ACTCHA       Change?                                      
         JE    PRCEML12                                                         
         CLI   ACTNUM,ACTDEL       Delete?                                      
         JE    PRCEML12                                                         
         CLI   ACTNUM,ACTABDEL     AdBuyer delete?                              
         JNE   PRCEML_X                                                         
*                                                                               
* PRINTPAK APPLICATION CHANGED PRISMA INVOICE HEADER                            
*                                                                               
PRCEML12 L     R5,ALOCALWS         POINT TO LOCAL STORAGE AREA                  
         USING PPPNV10D,R5                                                      
*                                                                               
         XCEFL EMLFLDS,'EMLFLDLN'  INIT E-MAIL FIELDS                           
*                                                                               
         MVC   EMLTOADR(L'EMLSNDTO),EMLSNDTO                                    
         MVI   EMLTOADR+60,X'FF'   END OF E-MAIL LIST (NOT AN ARRAY)            
         MVI   EMLTOEND,X'FF'      END OF "TO" LIST                             
         MVC   EMLCCADR(L'EMLSNDCC),EMLSNDCC                                    
         MVI   EMLCCADR+60,X'FF'   END OF E-MAIL LIST (NOT AN ARRAY)            
         MVI   EMLCCEND,X'FF'      END OF "CC" LIST                             
         MVI   EMLBCEND,X'FF'      END OF "BCC" LIST                            
*                                                                               
         MVC   EMLSUBJ,SPACE150                                                 
         CLI   ACTNUM,ACTCHA       Change?                                      
         JNE   *+18                                                             
         MVC   EMLSUBJ(L'EMLSUBCH),EMLSUBCH                                     
         LA    RE,EMLSUBJ+L'EMLSUBCH                                            
         J     *+14                                                             
         MVC   EMLSUBJ(L'EMLSUBDL),EMLSUBDL                                     
         LA    RE,EMLSUBJ+L'EMLSUBDL                                            
         MVI   0(RE),C'['                                                       
         MVC   1(L'QAGY,RE),QAGY                                                
         MVI   1+L'QAGY(RE),C']'                                                
*                                                                               
         LA    RE,EMLDATA          Start of variable string                     
         LA    R0,40                                                            
         MVC   0(100,RE),SPACE150                                               
         LA    RE,100(RE)          Init to spaces                               
         JCT   R0,*-10                                                          
*                                                                               
         LA    R3,EMLDATA                                                       
         MVC   00(02,R3),=AL2(L'EMLDESTX)                                       
         MVC   02+00(L'EMLDESTX,R3),EMLDESTX                                    
         AHI   R3,2+L'EMLDESTX     Point to 2nd line                            
*                                                                               
         MVC   00(02,R3),=AL2(10)  Length of 2nd line - Media                   
         MVC   02+00(07,R3),=C'Media: '                                         
         MVC   02+07(01,R3),QMED                                                
         AHI   R3,2+10             Point to 3rd line                            
*                                                                               
         MVC   00(02,R3),=AL2(15)  Length of 3rd line - Client                  
         MVC   02+00(08,R3),=C'Client: '                                        
         MVC   02+08(03,R3),QCLT                                                
         AHI   R3,2+15             Point to 4th line                            
*                                                                               
         MVC   00(02,R3),=AL2(30)  Length of 4th line - Vendor (pub)            
         MVC   02+00(08,R3),=C'Vendor: '                                        
         MVC   WORK,SPACE150                                                    
         GOTOR VPUBEDIT,DMCB,(0,QPUB),(C'S',WORK)                               
         MVC   02+08(20,R3),WORK                                                
         AHI   R3,2+30             Point to 5th line                            
*                                                                               
         MVC   00(02,R3),=AL2(30)  Length of 5th line - Invoice #               
         MVC   02+00(11,R3),=C'Invoice #: '                                     
         MVC   02+11(L'PNVHINV#,R3),HDRINV                                      
         AHI   R3,2+30             Point to 6th line                            
*                                                                               
         MVC   00(02,R3),=AL2(28)  Length of 6th line - Invoice Ser#            
         MVC   02+00(18,R3),=C'Invoice Serial #: '                              
         MVC   02+18(10,R3),HDRSER#                                             
         AHI   R3,2+28             Point to 7th line                            
*                                                                               
         MVC   00(02,R3),=AL2(02)  Length of 7th line - <blank line>            
         MVC   2(2,R3),SPACE150                                                 
         AHI   R3,2+2              Point to 8th line                            
*                                                                               
         MVC   00(02,R3),=AL2(30)  Length of 8th line - Changed Date            
         XC    EMLWORK1,EMLWORK1                                                
         GOTO1 DATCON,DMCB,(3,BTODAY),(21,EMLWORK1)                             
         CLI   ACTNUM,ACTCHA       Change?                                      
         JNE   *+20                                                             
         MVC   02+00(L'EMLCHGDT,R3),EMLCHGDT                                    
         MVC   02+L'EMLCHGDT(10,R3),EMLWORK1                                    
         J     *+16                                                             
         MVC   02+00(L'EMLDELDT,R3),EMLDELDT                                    
         MVC   02+L'EMLDELDT(10,R3),EMLWORK1                                    
         AHI   R3,2+30             Point to 9th line                            
*                                                                               
         MVC   00(02,R3),=AL2(50)  Length of 9th line - Changed By              
         XC    EMLWORK1,EMLWORK1                                                
         SR    R2,R2               Not pointing to a screen field               
         GOTOR TRNPID,DMCB,(0,SVPNVPID),(40,EMLWORK1)                           
         CLI   ACTNUM,ACTCHA       Change?                                      
         JNE   *+20                                                             
         MVC   02+00(L'EMLCHGBY,R3),EMLCHGBY                                    
         MVC   02+L'EMLCHGBY(40,R3),EMLWORK1                                    
         J     *+16                                                             
         MVC   02+00(L'EMLDELBY,R3),EMLDELBY                                    
         MVC   02+L'EMLDELBY(40,R3),EMLWORK1                                    
         AHI   R3,2+50             Point to 10th line                           
*                                                                               
         XC    0(2,R3),0(R3)       End of message                               
*                                                                               
         LA    R1,SMTPC            Establish email parameter list               
         USING SMTPD,R1                                                         
         XC    SMTPC(36),SMTPC     INIT PARAMETER BLOCK                         
         LA    RF,EMLTOADR                                                      
         ST    RF,SMTPTO           Set To address                               
         LA    RF,EMLCCADR                                                      
         ST    RF,SMTPCC           Set Cc address                               
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          Set Subject address                          
         LA    RF,EMLDATA                                                       
         ST    RF,SMTPDATA         Set Body of email address                    
         LA    RF,SMTPMAIL                                                      
         ST    RF,SMTPVRLN         Indicate date is variable                    
*                                                                               
         MVC   EMLJMOPT,JMOPTION   'LONGER FROM'/'SENDER'/'REPLYTO'             
         LA    RF,EMLJMOPT                                                      
         ST    RF,SMTPOPTS                                                      
         MVI   EMLJMOPT+L'FROMLONG+L'SENDER,X'FF'    NO REPLY FIELD             
*                                                                               
         GOTOR VJESMAIL,(R1)       SEND E-MAIL                                  
         DROP  R1                                                               
*                                                                               
PRCEML_X XIT1                                                                   
*                                                                               
SMTPMAIL DC    CL10'*SMTPMAIL*'                                                 
JMOPTION DS    0H                                                               
FROMLONG DC    CL10'*FROMLONG*'                                                 
SENDER   DC    CL10'*SENDER***'                                                 
REPLYTO  DC    CL10'*REPLYTO**'                                                 
         DC    X'FF'                                                            
JMOPTNQ  EQU   *-JMOPTION                                                       
*                                                                               
SPACE150 DC    150C' '                                                          
EMLSNDTO DC    C'PrismaL2Support@mediaocean.com'                                
EMLSNDCC DC    C'PrismaINVnotification@mediaocean.com'                          
EMLSNDKW DC    C'kwang@mediaocean.com'                                          
EMLSNDFW DC    C'fwhittaker@mediaocean.com'                                     
EMLSUBCH DC    C'Prisma Invoice changed by PrintPak '                           
EMLSUBDL DC    C'Prisma Invoice deleted by PrintPak '                           
EMLDESTX DC    C'The following invoice has been changed outside of Pris+        
               ma:'                                                             
EMLCHGDT DC    C'Changed on Date: '                                             
EMLDELDT DC    C'Deleted on Date: '                                             
EMLCHGBY DC    C'Changed By: '                                                  
EMLDELBY DC    C'Deleted By: '                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
STATUSTD DSECT                                                                  
STATCODE DS    CL1                                                              
STATNAME DS    CL10                                                             
STATEQU  EQU   *-STATUSTD                                                       
*                                                                               
DLISTD   DSECT                                                                  
DLISTSTT DS    CL1                                                              
         DS    CL1                                                              
DLISTCLI DS    CL3                                                              
         DS    CL4                                                              
DLISTPUB DS    CL8                                                              
         DS    CL2                                                              
DLISTPBN DS    CL20                                                             
         DS    CL2                                                              
DLISTINV DS    CL14                                                             
         DS    CL1                                                              
DLISTPER DS    CL17                                                             
DLISTEQU EQU   *-DLISTD                                                         
*                                                                               
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
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVWRKD                                                      
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO10   DS    F                   RELOACTION FACTOR                            
ALOCALWS DS    F                   ADDRESS OF LOCAL STORAGE AREA                
VJESMAIL DS    A                   ADDRESS OF JESMAIL FROM COMFACS              
*                                                                               
HDRACTEL DS    CL256               SAVE AREA FOR HEADER ACTIVITY ELEM           
LSSVKEY  DS    CL(L'KEY)           SAVED KEY FROM LIST ROUTINE                  
MYKEY    DS    CL25                                                             
XKEY1    DS    CL25                                                             
XKEY2    DS    CL25                                                             
XKEY3    DS    CL25                                                             
XKEY6    DS    CL25                                                             
LSPUB    DS    CL8                 AREA FOR PUB NUMBER EBCDIC                   
         DS    CL20                NEED MORE ROOM FOR PUBEDIT OUTPUT            
LQSER#   DS    CL(L'QSER#)                                                      
QDATE    DS    CL3                                                              
QMONEY   DS    PL6                                                              
QTAX__   DS    XL(L'PNVHTAX)                                                    
CHGSWTCH DS    CL1                 C'Y' - A KEY FIELD HAS BEEN CHGD             
CHGMED   EQU   X'80'               AGENCY CHANGED                               
CHGCLT   EQU   X'40'               CLIENT CHANGED                               
CHGPUB   EQU   X'20'               PUB CHANGED                                  
CHGINV   EQU   X'10'               INVOICE NUMBER CHANGED                       
CHGHDR   EQU   X'08'               DETAIL SEQUENCE NUMBER CHNAGED               
*                                                                               
QHDRCRST DS    XL1                 CURRENT STATUS ON CHANGE                     
*                                     IN STATUS                                 
*                                                                               
SVHINVSR DS    XL(L'PNVHIVSR)      SAVED INVOICE HEADER SOURCE                  
*                                                                               
         ORG                                                                    
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
PPPNV10D DSECT                                                                  
*                                                                               
* JESMAIL PARAMETER BLOCK                                                       
*                                                                               
         DS    0F                                                               
SMTPC    DS    XL(SMTPDQ)          PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
EMLWORK1 DS    CL64                                                             
EMLWORK2 DS    CL64                                                             
EMLELEM1 DS    CL256                                                            
EMLELEM2 DS    CL256                                                            
*                                                                               
* E-MAIL FIELDS                                                                 
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL120               TO:E-MAIL ADDRESS                            
EMLTOEND DS    XL1                 X'FF' END OF LIST                            
EMLCCADR DS    CL180               CC: E-MAIL ADDRESS (UP TO THREE)             
EMLCCEND DS    XL1                 X'FF' END OF LIST                            
EMLBCADR DS    CL60                BCC: E-MAIL ADDRESS                          
EMLBCEND DS    XL1                 X'FF' END OF LIST                            
EMLFMADR DS    CL150               FROM: E-MAIL ADDRESS                         
EMLRPLY  DS    CL150               UP TO 2 REPLY TO EMAIL ADDRESSES             
EMLSUBJ  DS    CL100               SUBJECT                                      
EMLJMOPT DS    CL(JMOPTNQ)         EMAIL OPTIONS                                
EMLDATA  DS    CL4000              VARIABLE STRING - SEE FAJESMAILD             
EMLFLDLN EQU   *-EMLFLDS                                                        
*                                                                               
WKAIO1_  DS    XL4096                                                           
*                                                                               
PPPNV10X EQU   *                                                                
*                                                                               
       ++INCLUDE FAJESMAILD                                                     
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'247PPPNV10   06/12/18'                                      
         END                                                                    
