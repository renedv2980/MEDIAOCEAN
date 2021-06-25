*          DATA SET PPESR05    AT LEVEL 016 AS OF 03/26/08                      
*PHASE T42005A                                                                  
*                                                                               
*        TITLE 'T42005 - LNK/ESR COMMUNICATION MODULE - CHANGE LOG'             
         TITLE 'T42005 - LNK/ESR COMMUNICATION MODULE - CHANGE LOG'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* BOBY 07/04    BIG BANG                                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42005 - LNK/ESR COMMUNICATION MODULE - INIT'                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T42005 - LNK/ESR COMMUNICATION MODULE                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T42000 (ESR CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     COMMUNICATES BTW LINK (AB) AND ESR     PROGRAM        *         
*                                                                     *         
*  INPUTS       NO SCREENS                                            *         
*                                                                     *         
*  OUTPUTS      ALL DATA WILL BE RETURNED TO LINK                     *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- LIOBD (LINK INTERFACE BLOCK)                    *         
*               R4 -- LOCAL WORKING STORAGE AREA                      *         
*               R5 -- GLOBAL LITERALS                                 *         
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
         TITLE 'T42005 - LNK/ESR COMMUNICATION MODULE'                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
T42005   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T42005                                                         
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         BASR  R5,0                                                             
         AHI   R5,GLOBALS-*                                                     
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         TITLE 'PPESR05 - PRINT NEW ESR CONTROLLER - VCOMMON'                   
***********************************************************************         
*                                                                     *         
*        COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES               *         
*                                                                     *         
***********************************************************************         
*                                                                               
VCOMMON  DS    0H                                                               
*                                                                               
         SRL   RF,25               SHIFT ROUTINE ID TO RIGHT NIBBLE             
         LA    RE,VBRANCH(RF)      GET A(ROUTINE)                               
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LA    RF,T42005(RF)                                                    
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
         XIT1                                                                   
*                                                                               
* COMMON ROUTINE ADDRESSES                                                      
*                                                                               
VBRANCH  DS    0X                  ALIGNMENT                                    
         DC    AL2(VLIOINI-T42005) LINKIO INITIALIZATION                        
         DC    AL2(VLIOGTH-T42005) LINKIO ESR    "GET" (REC AND ACT)            
         DC    AL2(VLIOGTD-T42005) LINKIO DATA   "GET" (DATA)                   
         DC    AL2(VLIOPUT-T42005) LINKIO PUT                                   
         DC    AL2(VLIOERR-T42005) LINKIO ERROR                                 
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - LINKIO INITIALIZATION - VLIOINI'                       
***********************************************************************         
*                                                                     *         
* LINKIO INITIALIZATION, WILL SET LINK CALL SWITCH                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOINI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         MVI   DDLNKSW,0           INIT LINK CALL SWITCH                        
*                                                                               
*        CHECK IF WE ARE BEING CALLED BY GLOBBER                                
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',GLOBAREA,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF       SKIP IF NOT FOUND                            
         BE    VLIOINIX                                                         
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'PRILINPRIESR',GLOBAREA                                        
         BNE   VLIOINIX            SKIP IF NOT FOR US                           
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD  INITIALIZE                           
*                                                                               
*        SET ADDRESSES                                                          
*                                                                               
         LA    R0,LIOBD+L'LIOB                                                  
         ST    R0,LIOBAREC         INCOMING RECORD                              
*                                  MULTIPLE OUTPUT RECS MODE                    
*                                  USER PROVIDES RECORD CODES                   
         MVI   LIOBINDS,LIOBINRM+LIOBIMLT                                       
*                                                                               
         AHI   R0,8000             MAX SIZE FOR LINKIO REC USES                 
         ST    R0,LIOBABUF         LINKIO BUFFER                                
*                                                                               
         MVC   LIOBACOM,ACOMFACS   A(COMFACS)                                   
*                                                                               
         LA    RF,MAP                                                           
         STCM  RF,15,LIOBAMAP      MAP TABLE                                    
*                                                                               
         MVI   LIOBMSYS,4          PRINT SYSTEM MSGS                            
*                                                                               
         XC    LIOBRECL,LIOBRECL   DEFAULT TO SIZE OF MAX BUFFER                
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAINI',LIOBD)  INIT LINKIO                      
         BNE   VLIOINIX            PROBLEMS                                     
*                                                                               
         MVI   DDLNKSW,C'Y'        YES, IT IS A LINK CALL                       
*                                                                               
VLIOINIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - RECORD AND ACTION FIELDS - VLIOGTH'                    
***********************************************************************         
*                                                                     *         
* THIS LINKIO "GET" WILL SET RECORD AND ACTION FIELDS                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOGTH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   DDLNKSW,C'Y'        LINK CALL?                                   
         BE    *+6                                                              
         DC    H'0'                IT HAS TO BE A LINK CALL!                    
*                                                                               
         GOTOR INILIOB             INIT LINKIO BASE ADDRESSES                   
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   DONE AT END OF FILE                          
         BO    VLGTHDON                                                         
*                                                                               
         CLI   DDLNKEOF,C'Y'       NO MORE REQ RECS TO PROCESS?                 
         BNE   *+6                                                              
         DC    H'0'                SOMETHING FROM HERE...                       
*                                                                               
         CLI   DDLNKEOF,C'N'       NOT YET EOF?                                 
         BE    VLGTH20             ALREADY DID A LINKIO "GET"                   
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         OI    LIOBINDS,LIOBIRET   RETURN TO CALLER (BREAK)                     
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)  GET LINKIO DATA                  
         BH    VLGTHDON                                                         
*                                                                               
VLGTH20  MVI   LIOSCRSW,0          RESET SCREEN SWITCH                          
*                                                                               
*        FIND MAP IN RECORD/ACTION TABLE                                        
*                                                                               
         LA    RE,RECACTAB                                                      
*                                                                               
VLGTHLP  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    VLGTHDN                                                          
*                                                                               
         CLC   LIOBMAP#,0(RE)      MATCH MAP NUMBER                             
         BE    VLGTHFD                                                          
*                                                                               
VLGTHCN  DS    0H                                                               
*                                                                               
         LA    RE,RECACTLQ(RE)     NEXT ENTRY IN TABLE                          
         B     VLGTHLP                                                          
*                                                                               
VLGTHFD  DS    0H                                                               
*                                                                               
         MVC   DDLNKACT,2+0(RE)    SAVE ACTION                                  
         MVC   DDLNKREC,2+1(RE)    SAVE RECORD                                  
*                                                                               
VLGTHDN  CLI   DDLNKACT,0          SUCCESSFULLY SET?                            
         BNE   *+6                                                              
         DC    H'0'                INVALID REQ RECORD CODE                      
*                                                                               
         MVC   CONREC,SPACES       INIT RECORD AND ACTION FIELDS                
         MVC   CONACT,SPACES                                                    
*                                                                               
*        SET RECORD FIELD ON SCREEN                                             
*                                                                               
         CLI   DDLNKREC,DDLK_INS   RECORD IS ESR?                               
         BNE   *+14                                                             
         MVC   CONREC(06),=C'ESR   '                                            
         MVI   CONRECH+5,6                                                      
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   RECORD IS FAX?                               
         BNE   *+14                                                             
         MVC   CONREC(03),=C'FAX'                                               
         MVI   CONRECH+5,3                                                      
*                                                                               
         CLI   DDLNKREC,DDLK_STA   RECORD IS STATUS?                            
         BNE   *+14                                                             
         MVC   CONREC(06),=C'STATUS'                                            
         MVI   CONRECH+5,6                                                      
*                                                                               
*        SET ACTION FIELD ON SCREEN                                             
*                                                                               
         CLI   DDLNKACT,DDLK_DEL   ACTION IS DELETE?                            
         BNE   *+14                                                             
         MVC   CONACT(06),=C'ABDEL'                                             
         MVI   CONACTH+5,5                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_ADD   ACTION IS ADD?                               
         BNE   *+14                                                             
         MVC   CONACT(06),=C'ABADD'                                             
         MVI   CONACTH+5,5                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_SND   ACTION IS SEND?                              
         BNE   *+14                                                             
         MVC   CONACT(04),=C'SEND'                                              
         MVI   CONACTH+5,4                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_INQ   ACTION IS INQUIRY?                           
         BNE   *+18                                                             
         MVC   CONACT(06),=C'STATUS'                                            
         MVI   CONACTH+5,6                                                      
         OI    LIOBINDS,LIOBIMLT   MULTIPLE OUTPUT RECORDS                      
*                                                                               
         CLI   DDLNKACT,DDLK_DWN   ACTION IS DOWNLOAD                           
         BNE   *+18                                                             
         MVC   CONACT(06),=C'HISTORY'                                           
         MVI   CONACTH+5,7                                                      
         CLI   LIOBINDS,LIOBIMLT   MULTIPLE OUTPUT RECORDS                      
*                                                                               
         MVI   LIOSCRSW,LIOS_TOP   TOP SCREEN IS SET                            
*                                                                               
         B     VLIOGTHX                                                         
*                                                                               
VLGTHDON MVI   DDLNKEOF,C'Y'       NO MORE REQ RECS                             
*                                                                               
         B     VLIOGTHX            SKIP UNLESS MULTIPLE REQUESTS                
*                                                                               
VLIOGTHX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - DATA FIELDS - VLIOGTD'                                 
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS LINKIO "GET" WILL SET DATA FIELDS                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOGTD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         GOTOR INILIOB             INITIALIZE LINKIO BASE REGISTERS             
*                                                                               
         CLI   DDLNKSW,C'Y'        LINK CALL?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DDLNKEOF,C'Y'       NO MORE REQ RECS TO PROCESS?                 
         BNE   *+6                                                              
         DC    H'0'                SOMETHING FROM HERE...                       
*                                                                               
         CLI   DDLNKACT,0          ACTION SWITCH IS SET?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         MVI   LIOSCRSW,0          RESET SCREEN SWITCH                          
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD)                                   
         BH    VLGTDONE                                                         
         BE    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
         MVI   LIOSCRSW,LIOS_BOT   BOTTOM SCREEN IS SET                         
*                                                                               
*        FILL IN FIELDS ON FAX SCREEN                                           
*                                                                               
         CLI   DDLNKREC,DDLK_INS   ESR    RECORD?                               
         BE    *+8                                                              
         CLI   DDLNKREC,DDLK_STA   STATUS RECORD?                               
         BE    *+8                                                              
         CLI   DDLNKREC,DDLK_FAX   FAX    RECORD?                               
         BNE   VLIOGTDX                                                         
*                                                                               
         CLI   DDLNKACT,DDLK_INQ   DONE IF ACTION INQUIRY                       
         BE    VLIOGTDX                                                         
*                                                                               
         CLI   DDLNKACT,DDLK_DWN   DONE IF ACTION DOWNLOAD                      
         BE    VLIOGTDX                                                         
*                                                                               
         CLI   DDLNKACT,DDLK_DEL   DONE IF ACTION DELETE                        
         BE    VLIOGTDX                                                         
*                                                                               
         CLI   DDLNKREC,DDLK_STA   DONE IF STATUS RECORD                        
         BNE   *+8                                                              
         CLI   DDLNKACT,DDLK_ADD   AND ADD                                      
         BE    VLIOGTDX                                                         
*                                                                               
         OC    LNKESRKY,LNKESRKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ESR KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSSR#,DMCB,(L'LNKESRKY,LNKESRKY)  BREAK OUT KEY                 
*                                                                               
         CLI   DDLNKREC,DDLK_INS   ESR    RECORD?                               
         BNE   VLGTDINN                                                         
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         MVC   HDRMED,SPACES                                                    
         MVC   HDRMED(L'QMED),QMED                                              
         MVI   HDRMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   HDRCLT,SPACES                                                    
         MVC   HDRCLT(L'QCLT),QCLT                                              
         MVI   HDRCLTH+5,L'QCLT                                                 
*                                                                               
*        PUB IS ALREADY ON SCREEN                                               
*                                                                               
*                                                                               
*        SET SR# ON SCREEN                                                      
*                                                                               
         MVC   HDRSR#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QSR#SRYR       GET SR# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRSR#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QSR#SRSQ       GET SR# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                ASSUME 4 DIGIT SQN                           
         CP    DUB,=P'9999'        IF 5 DIGIT #                                 
         BNH   *+8                                                              
         LA    RF,5                   RESET LENGTH REGISTER                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  HDRSR#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,HDRSR#H+5        SET FIELD LENGTH                             
*                                                                               
*        SET REVISION # ON SCREEN                                               
*                                                                               
         MVC   HDRREV#,SPACES                                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QREV#          GET REVISION #                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRREV#(3),DUB                                                   
*                                                                               
         MVI   HDRREV#H+5,3        SET INPUT LENGTH                             
*                                                                               
VLGTDINX DS    0H                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTDINN DS    0H                                                               
*                                                                               
*        STATUS SCREEN                                                          
*                                                                               
VLGTDST  DS    0H                                                               
*                                                                               
         CLI   DDLNKREC,DDLK_STA   STATUS RECORD?                               
         BNE   VLGTDSTN                                                         
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         MVC   SSTMED,SPACES                                                    
         MVC   SSTMED(L'QMED),QMED                                              
         MVI   SSTMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   SSTCLT,SPACES                                                    
         MVC   SSTCLT(L'QCLT),QCLT                                              
         MVI   SSTCLTH+5,L'QCLT                                                 
*                                                                               
*        PUB ALREADY ON SCREEN                                                  
*                                                                               
*                                                                               
*        SET INSERTION ORDER # ON SCREEN                                        
*                                                                               
         MVC   SSTSR#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QSR#SRYR       GET SR# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SSTSR#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QSR#SRSQ       GET SR# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                ASSUME 4 DIGIT SQN                           
         CP    DUB,=P'9999'        IF 5 DIGIT #                                 
         BNH   *+8                                                              
         LA    RF,5                   RESET LENGTH REGISTER                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  SSTSR#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,SSTSR#H+5        SET FIELD LENGTH                             
*                                                                               
*        SET REVISION # ON SCREEN                                               
*                                                                               
         MVC   SSTREV#,SPACES                                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QREV#          GET REVISION #                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SSTREV#(3),DUB                                                   
*                                                                               
         MVI   SSTREV#H+5,3        SET INPUT LENGTH                             
*                                                                               
VLGTDSTX DS    0H                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTDSTN DS    0H                                                               
*                                                                               
*        FAX    SCREEN                                                          
*                                                                               
VLGTDFX  DS    0H                                                               
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   FAX RECORD?                                  
         BNE   VLGTDFXN                                                         
*                                                                               
*        SET MEDIA ON SCREEN                                                    
*                                                                               
         MVC   SFXMED,SPACES                                                    
         MVC   SFXMED(L'QMED),QMED                                              
         MVI   SFXMEDH+5,L'QMED                                                 
*                                                                               
*        SET CLIENT ON SCREEN                                                   
*                                                                               
         MVC   SFXCLT,SPACES                                                    
         MVC   SFXCLT(L'QCLT),QCLT                                              
         MVI   SFXCLTH+5,L'QCLT                                                 
*                                                                               
*        PUB ALREADY ON SCREEN                                                  
*                                                                               
*                                                                               
*        SET INSERTION ORDER # ON SCREEN                                        
*                                                                               
         MVC   SFXSR#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QSR#SRYR       GET SR# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SFXSR#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QSR#SRSQ       GET IO# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                ASSUME 4 DIGIT SQN                           
         CP    DUB,=P'9999'        IF 5 DIGIT #                                 
         BNH   *+8                                                              
         LA    RF,5                   RESET LENGTH REGISTER                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  SFXSR#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,SFXSR#H+5        SET FIELD LENGTH                             
*                                                                               
*        SET REVISION # ON SCREEN                                               
*                                                                               
         MVC   SFXREV#,SPACES                                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QREV#          GET REVISION #                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SFXREV#(3),DUB                                                   
*                                                                               
         MVI   SFXREV#H+5,3        SET INPUT LENGTH                             
*                                                                               
VLGTDFXX DS    0H                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTDFXN DS    0H                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTDONE MVI   DDLNKEOF,C'Y'       NO MORE REQ RECS                             
*                                                                               
VLIOGTDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - RETURN DATA TO PC'                                     
*                                                                               
***********************************************************************         
*                                                                     *         
*        INTERFACE TO LINKIO TO RETURN DATA TO PC                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOPUT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   DDLNKREC,DDLK_INS   IF ESR    RECORD                             
         BNE   *+8                                                              
         CLI   DDLNKACT,DDLK_DEL   AND ACTION IS DELETE                         
         BNE   *+12                                                             
         LHI   R2,E#SRDLRP            STATUS REPLY                              
         B     VLIOPUT1               SEND RETURN                               
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   IF FAX RECORD                                
         BNE   *+8                                                              
         CLI   DDLNKACT,DDLK_SND   AND ACTION IS SEND                           
         BNE   *+12                                                             
         LHI   R2,E#SRFXRP            FAX    REPLY                              
         B     VLIOPUT1               SEND RETURN                               
*                                                                               
         B     VLIOPUTX            ELSE SKIP                                    
*                                                                               
VLIOPUT1 DS    0H                                                               
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
*        MAKE SURE WE HAVE READ TO THE END OF THE FILE                          
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    VLIOPUT2                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD) GET END OF DATA                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   MUST BE EOF                                  
         BO    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
VLIOPUT2 DS    0H                                                               
*                                                                               
*        RETURN RECORD CODE                                                     
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(R2))                  
*                                                                               
*        RETURN IO NUMBER                                                       
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    +        
               ('LD_CHARQ',LNKESRKY),(L'LNKESRKY,0)                             
*                                                                               
*        CLOSE THE WORKER FILE                                                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD)                                   
*                                                                               
VLIOPUTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - RETURN ERRORS'                                         
***********************************************************************         
*                                                                     *         
* INTERFACE TO LINKIO TO RETURN ERROR, R2 POINTS TO ERR FIELD         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         XC    HALF,HALF           FOR UNKNOW ERROR FIELD                       
*                                                                               
*        SET MAP POINTER                                                        
*                                                                               
         CLI   DDLNKREC,DDLK_INS   ESR ESR    RECORD?                           
         BNE   *+12                                                             
         LA    R6,ESRINS           POINT TO ESR ESR    FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_STA   ESR STATUS RECORD?                           
         BNE   *+12                                                             
         LA    R6,ESRSTA           POINT TO ESR STATUS FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   ESR FAX    RECORD?                           
         BNE   *+12                                                             
         LA    R6,ESRFAX           POINT TO ESR FAX FIELDS                      
         B     VLERR20                                                          
*                                                                               
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
VLERR20  DS    0H                                                               
*                                                                               
*        FIND MAP TABLE ENTRY OF FIELD IN ERROR                                 
*                                                                               
         LR    RF,R2               FIELD IN ERROR                               
         LA    RE,T420FFD                                                       
         SR    RF,RE                                                            
         ST    RF,FULL             DISPLACEMENT TO FIELD IN ERROR               
*                                                                               
         USING LIODD,R6                                                         
*                                                                               
VLERRLP  DS    0H                                                               
*                                                                               
         OC    0(2,R6),0(R6)       END OF MAP TABLE?                            
         BZ    VLERRNF             DONE                                         
*                                                                               
         TM    LIODIND1,LIODITFH   TWA FIELD ESR?                               
         BNO   VLERRCN                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LIODDISP                                                    
         C     RE,FULL             MATCH DISPLACEMENT OF ERROR FIELD?           
         BNE   VLERRCN                                                          
*                                                                               
         MVC   HALF,LIODDMAP       MAP CODE OF ERROR FLD TO BE REPLIED          
*                                                                               
         B     VLERRDN                                                          
*                                                                               
VLERRCN  DS    0H                                                               
*                                                                               
         LA    R6,LIODL(R6)        POINT TO NEXT ENTRY IN MAP TABLE             
         B     VLERRLP                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
*        FIELD IN ERROR NOT IN MAP TABLE                                        
*        CHECK KEY FIELDS                                                       
*                                                                               
VLERRNF  DS    0H                                                               
*                                                                               
*        ESR    SCREEN                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_INS   RECORD IS ESR?                               
         BNE   VLERRINN                                                         
*                                                                               
         LA    RE,HDRMEDH          MEDIA                                        
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#MEDCOD)                                              
         B     VLERRINX                                                         
*                                                                               
         LA    RE,HDRCLTH          CLIENT                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#CLTCOD)                                              
         B     VLERRINX                                                         
*                                                                               
         LA    RE,HDRSR#H          SR#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRINX                                                         
*                                                                               
         LA    RE,HDRREV#H         REVISION #                                   
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRINX                                                         
*                                                                               
         LA    RE,HDRPERH          PERIOD                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#STEND)                                               
         B     VLERRINX                                                         
*                                                                               
VLERRINX DS    0H                                                               
         B     VLERRDN                                                          
*                                                                               
VLERRINN DS    0H                                                               
*                                                                               
*        STATUS SCREEN                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_STA   RECORD IS STATUS?                            
         BNE   VLERRSTN                                                         
*                                                                               
         LA    RE,SSTMEDH          MEDIA                                        
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#MEDCOD)                                              
         B     VLERRSTX                                                         
*                                                                               
         LA    RE,SSTCLTH          CLIENT                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#CLTCOD)                                              
         B     VLERRSTX                                                         
*                                                                               
         LA    RE,SSTSR#H          SR#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRSTX                                                         
*                                                                               
         LA    RE,SSTREV#H         REVISON #                                    
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRSTX                                                         
*                                                                               
         LA    RE,SSTPERH          PERIOD                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#STEND)                                               
         B     VLERRSTX                                                         
*                                                                               
VLERRSTX DS    0H                                                               
         B     VLERRDN                                                          
*                                                                               
VLERRSTN DS    0H                                                               
*                                                                               
*        FAX    SCREEN                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   RECORD IS FAX                                
         BNE   VLERRSTN                                                         
*                                                                               
         LA    RE,SFXMEDH          MEDIA                                        
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#MEDCOD)                                              
         B     VLERRFXX                                                         
*                                                                               
         LA    RE,SFXCLTH          CLIENT                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#CLTCOD)                                              
         B     VLERRFXX                                                         
*                                                                               
         LA    RE,SFXSR#H          SR#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRFXX                                                         
*                                                                               
         LA    RE,SFXREV#H         REVISON #                                    
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#ESRLKY)                                              
         B     VLERRFXX                                                         
*                                                                               
VLERRFXX DS    0H                                                               
         B     VLERRDN                                                          
*                                                                               
VLERRFXN DS    0H                                                               
*                                                                               
VLERRDN  DS    0H                  RETURN FIELD IN ERROR                        
*                                                                               
*        FIND MAP IN ERROR MAPCODE TABLE                                        
*                                                                               
         LA    RE,RECERRTB                                                      
         XC    FULL,FULL                                                        
*                                                                               
VLRECLP  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    VLRECDN                                                          
*                                                                               
         CLC   LIOBMAP#,0(RE)      MATCH MAP NUMBER                             
         BE    VLRECFD                                                          
*                                                                               
VLRECCN  DS    0H                                                               
*                                                                               
         LA    RE,RECERRLQ(RE)     NEXT ENTRY IN TABLE                          
         B     VLRECLP                                                          
*                                                                               
VLRECFD  DS    0H                                                               
*                                                                               
         MVC   FULL,2(RE)          SAVE ERROR RECORD CODE                       
*                                                                               
VLRECDN  OC    FULL,FULL           SUCCESSFULLY SET?                            
         BNZ   *+6                                                              
         DC    H'0'                INVALID REQ RECORD CODE                      
*                                                                               
*        MAKE SURE WE HAVE READ TO THE END OF THE FILE                          
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   TEST FOR EOF ALREADY FOUND                   
         BO    VLIOERR2                                                         
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD) GET END OF DATA                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   MUST BE EOF                                  
         BO    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
VLIOERR2 DS    0H                                                               
*                                                                               
*        RETURN ERROR RECORD CODE                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,FULL           GET MAPCODE                                  
*                                                                               
*        MAPCODE PARAMETER MUST BE THE MAPCODE VALUE                            
*        NOT A(VALUE)                                                           
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    +        
               ('LD_CHARQ',LNKESRKY),(L'LNKESRKY,0)                             
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    +        
               ('LD_UBINQ',HALF),(L'HALF,0)                                     
*                                                                               
*        RETURN ERROR NUMBER                                                    
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',CONHEAD),(L'CONHEAD,0)                               
*                                                                               
         CLI   DDLNKACT,DDLK_INQ   IF INQUIRY                                   
         BE    *+8                                                              
         CLI   DDLNKACT,DDLK_DWN   OR DOWNLOAD                                  
         BE    VLIOERRX                EXIT WITHOUT CLOSING FILE                
*                                                                               
*        CLOSE THE WORKER FILE                                                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD)                                   
*                                                                               
VLIOERRX DS    0H                                                               
*                                                                               
         B     *+6                                                              
         DC    H'0'                TESTING                                      
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42005 - BREAK OUT LONG IO KEY - FILLKEY'                       
*                                                                               
***********************************************************************         
*                                                                     *         
* LNKESRKY  -  LONG FORM OF WEBIO KEY                                 *         
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
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         OC    LNKESRKY,LNKESRKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ESR KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSSR#,DMCB,(L'LNKESRKY,LNKESRKY)  BREAK OUT KEY                 
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVI   MINDELSW,C'Y'       NEED TO PROCESS DELETED TOO                  
*                                                                               
         LA    RE,MINMKEY          ESR MASTER KEY                               
         USING ESRKEY,RE                                                        
*                                                                               
         MVC   ESRKAGY,QAGY        SET AGENCY                                   
         MVC   ESRKMED,QMED        SET MEDIA                                    
         MVI   ESRKRCD,ESRKRCDQ    SET RECORD CODE                              
         MVC   ESRKCLT,QCLT        SET CLIENT                                   
         MVC   ESRKPUB,QPUB        SET PUB                                      
         MVC   ESRKSR#,QSR#        SET IO NUMBER                                
         MVC   ESRKRV#,QREV#       SET REVISION NUMBER                          
         MVI   ESRKELMK,X'FF'                                                   
         MVC   ESRKELMK+1(L'ESRKELMK-1),ESRKELMK                                
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)                               
*                                                                               
FILLKEYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42005 - INIT LINKIO BASE ADDRESSES - INILIOB'                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE LIOB BASE ADDRESSES                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
INILIOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         LHI   R3,LNKBUFF-SYSD                                                  
         LA    R3,SYSD(R3)                                                      
         USING LIOBD,R3            ESTABLISH LINKIO INTERFACE BLOCK             
*                                                                               
         ST    R9,LIOBASB1         SET WORK BASE ADDRESS                        
         ST    RA,LIOBASB2         SET SCREEN BASE ADDRESS                      
*                                                                               
INILIOBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
                                                                                
         TITLE 'T42005 - GLOBAL CONSTANTS - GLOBALS'                            
***********************************************************************         
*                                                                     *         
*        GLOBAL CONSTANTS                                             *         
*                                                                     *         
***********************************************************************         
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         TITLE 'T42005 - RECORD/ACTION TABLE - RECACTAB'                        
***********************************************************************         
*                                                                     *         
*        RECORD/ACTION TABLE                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACTAB DS    0H                                                               
         DC    AL2(M#ULSRDL),AL1(DDLK_DEL),AL1(DDLK_INS) DELETE/ESR             
RECACTLQ EQU   *-RECACTAB                                                       
         DC    AL2(M#ULSRST),AL1(DDLK_ADD),AL1(DDLK_STA) ADD/STATUS             
         DC    AL2(M#ULSRFX),AL1(DDLK_SND),AL1(DDLK_FAX) SEND/FAX               
         DC    AL2(M#ULSRIQ),AL1(DDLK_INQ),AL1(DDLK_STA) INQ/STATUS             
         DC    AL2(M#ULSRDW),AL1(DDLK_DWN),AL1(DDLK_STA) DWNLOAD/STATUS         
         DC    X'FF'               EOT                                          
                                                                                
*                                                                               
         TITLE 'T42005 - RECORD/ERROR TABLE - RECERRTB'                         
***********************************************************************         
*                                                                     *         
*        RECORD/ERROR  TABLE                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECERRTB DS    0H                                                               
         DC    AL2(M#ULSRDL),AL2(E#SRDLER)  DELETE/ESR                          
RECERRLQ EQU   *-RECERRTB                                                       
         DC    AL2(M#ULSRST),AL2(E#SRSTER)  ADD/STATUS                          
         DC    AL2(M#ULSRFX),AL2(E#SRFXER)  SEND/FAX                            
         DC    AL2(E#SRFXLN),AL2(E#SRFXER)  SEND/FAX LINE                       
         DC    AL2(M#ULSRIQ),AL2(E#SRIQER)  INQ/STATUS                          
         DC    AL2(M#ULSRDW),AL2(E#SRDWER)  DWNLOAD/STATUS                      
         DC    X'FF'               EOT                                          
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAP      DS    0XL(LIORL)                                                       
         DC    AL2(M#ULSRDL,E#SRDLRP,ESRINS-MAP)   ESR                          
         DC    AL2(M#ULSRFX,E#SRFXRP,ESRFAX-MAP)   FAX                          
         DC    AL2(E#SRFXLN,E#SRFXRP,ESRFXL-MAP)   FAX LINE OF TEXT             
         DC    AL2(M#ULSRST,E#SRSTRP,ESRSTA-MAP)   STATUS                       
         DC    AL2(M#ULSRIQ,E#SRIQRP,ESRINQ-MAP)   STATUS INQUIRY               
         DC    AL2(M#ULSRDW,E#SRDWRP,ESRDWN-MAP)   IO     DOWNLOAD              
MAPX     DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        ESR SCREEN                                                             
*                                                                               
ESRINS   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#ESRLKY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
******                                                                          
******   DC    AL2(D#ESRLKY),AL1(LIOBSB1Q)                                      
******   DC    AL2(LNKESRKY-SYSD),AL1(L'LNKESRKY)                               
******   DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
ESRINSX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRSTA   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#ESRLKY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#HSTYID),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKHSTID-SYSD),AL1(L'LNKHSTID)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#IORSTA),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTSTAH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
ESRSTAX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRFAX   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#ESRLKY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKESRKY-SYSD),AL1(L'LNKESRKY)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#HSTYID),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKHSTID-SYSD),AL1(L'LNKHSTID)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#MANSEN),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKMANSD-SYSD),AL1(L'LNKMANSD)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#E_MAIL),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXEMLH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#RECPNM),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAFXNM-SYSD),AL1(L'LNKAFXNM)                               
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#TYPE_2),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXTYP1H-T420FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ADRFAX),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXFAX1H-T420FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SUPCOS),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXSCS1H-T420FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
******                                                                          
******   DC    AL2(D#LINTXT),AL1(LIOBSB1Q)                                      
******   DC    AL2(LNKAFXLN-SYSD),AL1(L'LNKAFXLN)                               
******   DC    AL1(LIODINDX,0)                                                  
*                                                                               
ESRFAXX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRFXL   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#LINTXT),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAFXLN-SYSD),AL1(L'LNKAFXLN)                               
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
ESRFXLX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRINQ   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#HISTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKREQTP-SYSD),AL1(L'LNKREQTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INQMET),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKSTATP-SYSD),AL1(L'LNKSTATP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#DTYRPY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKDATTP-SYSD),AL1(L'LNKDATTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#FLTTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKFLTTP-SYSD),AL1(L'LNKFLTTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ESRLKY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
ESRINQX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRDWN   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#HISTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKREQTP-SYSD),AL1(L'LNKREQTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INQMET),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKSTATP-SYSD),AL1(L'LNKSTATP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#DTYRPY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKDATTP-SYSD),AL1(L'LNKDATTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#FLTTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKFLTTP-SYSD),AL1(L'LNKFLTTP)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#MEDCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAFLT-SYSD),AL1(L'LNKAFLT)                                 
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTCLTH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(QFPRD-SYSD),AL1(L'QFPRD)                                     
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB2Q)                                       
         DC    AL2(SSTPERH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T420FFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#EOR),AL1(LIOBSB1Q)                                         
         DC    AL2(LNKEOR-SYSD),AL1(1)                                          
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
ESRDWNX DC     AL2(0)                                                           
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
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
       ++INCLUDE PPESRFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPESRFDD          ESR ESR MAINT SCREEN                         
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPESRFCD          ESR FAX    MAINT SCREEN                      
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPESRFBD          ESR STATUS MAINT SCREEN                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPESRWRKD                                                      
*                                                                               
         ORG                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE         PRINT SYSTEM RECORD LAYOUTS                  
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS         COMMON FACILITIES                            
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK          MINIO CONTROL BLOCK                          
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS         PRINT SYSTEM RECORD LAYOUTS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDHDR          FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPMAPEQUS                                                      
         EJECT                                                                  
*                                                                               
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDLINKD                                                        
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PPESR05   03/26/08'                                      
         END                                                                    
