*          DATA SET PPWIO05    AT LEVEL 009 AS OF 12/17/07                      
*PHASE T41E05A                                                                  
*                                                                               
*        TITLE 'T41E05 - LNK/WIO COMMUNICATION MODULE - CHANGE LOG'             
         TITLE 'T41E05 - LNK/WIO COMMUNICATION MODULE - CHANGE LOG'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* BOBY 07/04    BIG BANG                                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41E05 - LNK/WIO COMMUNICATION MODULE - INIT'                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41E05 - LNK/WIO COMMUNICATION MODULE                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41E00 (WIO CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     COMMUNICATES BTW LINK (AB) AND WIO     PROGRAM        *         
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
         TITLE 'T41E05 - LNK/WIO COMMUNICATION MODULE'                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
T41E05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41E05                                                         
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
         TITLE 'PPWIO05 - PRINT NEW WIO CONTROLLER - VCOMMON'                   
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
         LA    RF,T41E05(RF)                                                    
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
         XIT1                                                                   
*                                                                               
* COMMON ROUTINE ADDRESSES                                                      
*                                                                               
VBRANCH  DS    0X                  ALIGNMENT                                    
         DC    AL2(VLIOINI-T41E05) LINKIO INITIALIZATION                        
         DC    AL2(VLIOGTH-T41E05) LINKIO INSORD "GET" (REC AND ACT)            
         DC    AL2(VLIOGTD-T41E05) LINKIO DATA   "GET" (DATA)                   
         DC    AL2(VLIOPUT-T41E05) LINKIO PUT                                   
         DC    AL2(VLIOERR-T41E05) LINKIO ERROR                                 
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E05 - LINKIO INITIALIZATION - VLIOINI'                       
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
         CLC   =C'PRILINPRIWIO',GLOBAREA                                        
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
         LHI   RE,8000             MAXIMUM REPLY RECORD LENGTH                  
         STCM  RE,3,LIOBRECL                                                    
*                                                                               
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
         TITLE 'T41E05 - RECORD AND ACTION FIELDS - VLIOGTH'                    
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
         CLI   DDLNKREC,DDLK_INS   RECORD IS INSORD?                            
         BNE   *+14                                                             
         MVC   CONREC(06),=C'INSORD'                                            
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
         TITLE 'T41E05 - DATA FIELDS - VLIOGTD'                                 
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
         CLI   DDLNKREC,DDLK_INS   INSORD RECORD?                               
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
         OC    LNKWIOKY,LNKWIOKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE WIO KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSIO#,DMCB,(L'LNKWIOKY,LNKWIOKY)  BREAK OUT KEY                 
*                                                                               
         CLI   DDLNKREC,DDLK_INS   INSORD RECORD?                               
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
*        SET IO# ON SCREEN                                                      
*                                                                               
         MVC   HDRIO#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QIO#IOYR       GET IO# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  HDRIO#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
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
         UNPK  HDRIO#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,HDRIO#H+5        SET FIELD LENGTH                             
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
         MVC   SSTIO#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QIO#IOYR       GET IO# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SSTIO#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
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
         UNPK  SSTIO#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,SSTIO#H+5        SET FIELD LENGTH                             
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
         MVC   SFXIO#,SPACES                                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,QIO#IOYR       GET IO# YEAR                                 
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SFXIO#(2),DUB                                                    
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         ICM   RF,7,QIO#IOSQ       GET IO# SEQ NUMBER                           
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
         UNPK  SFXIO#+2(0),DUB                                                  
*                                                                               
         SRL   RF,4                MOVE BACK TO RIGHT NYBBLE                    
         AHI   RF,1                RESTORE TRUE LENGTH                          
         AHI   RF,2                ADD LENGTH OF YEAR                           
         STC   RF,SFXIO#H+5        SET FIELD LENGTH                             
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
         TITLE 'T41E05 - RETURN DATA TO PC'                                     
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
         CLI   DDLNKREC,DDLK_INS   IF INSORD RECORD                             
         BNE   *+8                                                              
         CLI   DDLNKACT,DDLK_DEL   AND ACTION IS DELETE                         
         BNE   *+12                                                             
         LHI   R2,E#IODLRP            STATUS REPLY                              
         B     VLIOPUT1               SEND RETURN                               
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   IF FAX RECORD                                
         BNE   *+8                                                              
         CLI   DDLNKACT,DDLK_SND   AND ACTION IS SEND                           
         BNE   *+12                                                             
         LHI   R2,E#IOFXRP            FAX    REPLY                              
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
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    +        
               ('LD_CHARQ',LNKWIOKY),(L'LNKWIOKY,0)                             
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
         TITLE 'T41E05 - RETURN ERRORS'                                         
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
         CLI   DDLNKREC,DDLK_INS   WIO INSORD RECORD?                           
         BNE   *+12                                                             
         LA    R6,WIOINS           POINT TO WIO INSORD FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_STA   WIO STATUS RECORD?                           
         BNE   *+12                                                             
         LA    R6,WIOSTA           POINT TO WIO STATUS FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_FAX   WIO FAX    RECORD?                           
         BNE   *+12                                                             
         LA    R6,WIOFAX           POINT TO WIO FAX FIELDS                      
         B     VLERR20                                                          
*                                                                               
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
VLERR20  DS    0H                                                               
*                                                                               
*        FIND MAP TABLE ENTRY OF FIELD IN ERROR                                 
*                                                                               
         LR    RF,R2               FIELD IN ERROR                               
         LA    RE,T41EFFD                                                       
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
         TM    LIODIND1,LIODITFH   TWA FIELD INSORD?                            
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
*        INSORD SCREEN                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_INS   RECORD IS INSORD?                            
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
         LA    RE,HDRIO#H          IO#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
         B     VLERRINX                                                         
*                                                                               
         LA    RE,HDRREV#H         REVISION #                                   
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
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
         LA    RE,SSTIO#H          IO#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
         B     VLERRSTX                                                         
*                                                                               
         LA    RE,SSTREV#H         REVISON #                                    
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
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
         LA    RE,SFXIO#H          IO#                                          
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
         B     VLERRFXX                                                         
*                                                                               
         LA    RE,SFXREV#H         REVISON #                                    
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#IOLKEY)                                              
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
VLIOERR1 DS    0H                                                               
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAGET',LIOBD) GET END OF DATA                   
*                                                                               
         TM    LIOBFLG2,LIOBFEOF   KEEP LOOKING FOR END OF FILE                 
         BNO   VLIOERR1                                                         
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
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    +        
               ('LD_CHARQ',LNKWIOKY),(L'LNKWIOKY,0)                             
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
         TITLE 'T41E05 - BREAK OUT LONG IO KEY - FILLKEY'                       
*                                                                               
***********************************************************************         
*                                                                     *         
* LNKWIOKY  -  LONG FORM OF WEBIO KEY                                 *         
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
         OC    LNKWIOKY,LNKWIOKY                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE WIO KEY!                           
*                                                                               
         SR    R2,R2                                                            
         GOTOR PRSIO#,DMCB,(L'LNKWIOKY,LNKWIOKY)  BREAK OUT KEY                 
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR MININIT             INIT MINIO BLOCK                             
*                                                                               
         MVI   MINDELSW,C'Y'       NEED TO PROCESS DELETED TOO                  
*                                                                               
         LA    RE,MINMKEY          WIO MASTER KEY                               
         USING WIOKEY,RE                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVI   WIOKRCD,WIOKRCDQ    SET RECORD CODE                              
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
         MVC   WIOKIO#,QIO#        SET IO NUMBER                                
         MVC   WIOKRV#,QREV#       SET REVISION NUMBER                          
         MVI   WIOKELMK,X'FF'                                                   
         MVC   WIOKELMK+1(L'WIOKELMK-1),WIOKELMK                                
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)                               
*                                                                               
FILLKEYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E05 - INIT LINKIO BASE ADDRESSES - INILIOB'                  
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
                                                                                
         TITLE 'T41E05 - GLOBAL CONSTANTS - GLOBALS'                            
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
         TITLE 'T41E05 - RECORD/ACTION TABLE - RECACTAB'                        
***********************************************************************         
*                                                                     *         
*        RECORD/ACTION TABLE                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACTAB DS    0H                                                               
         DC    AL2(M#ULIODL),AL1(DDLK_DEL),AL1(DDLK_INS) DELETE/INSORD          
RECACTLQ EQU   *-RECACTAB                                                       
         DC    AL2(M#ULIOST),AL1(DDLK_ADD),AL1(DDLK_STA) ADD/STATUS             
         DC    AL2(M#ULIOFX),AL1(DDLK_SND),AL1(DDLK_FAX) SEND/FAX               
         DC    AL2(M#ULIOIQ),AL1(DDLK_INQ),AL1(DDLK_STA) INQ/STATUS             
         DC    AL2(M#ULIODW),AL1(DDLK_DWN),AL1(DDLK_STA) DWNLOAD/STATUS         
         DC    X'FF'               EOT                                          
                                                                                
*                                                                               
         TITLE 'T41E05 - RECORD/ERROR TABLE - RECERRTB'                         
***********************************************************************         
*                                                                     *         
*        RECORD/ERROR  TABLE                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECERRTB DS    0H                                                               
         DC    AL2(M#ULIODL),AL2(E#IODLER)  DELETE/INSORD                       
RECERRLQ EQU   *-RECERRTB                                                       
         DC    AL2(M#ULIOST),AL2(E#IOSTER)  ADD/STATUS                          
         DC    AL2(M#ULIOFX),AL2(E#IOSTER)  SEND/FAX                            
         DC    AL2(E#IOFXLN),AL2(E#IOSTER)  SEND/FAX                            
         DC    AL2(M#ULIOIQ),AL2(E#IOIQER)  INQ/STATUS                          
         DC    AL2(M#ULIODW),AL2(E#IODWER)  DWNLOAD/STATUS                      
         DC    X'FF'               EOT                                          
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAP      DS    0XL(LIORL)                                                       
         DC    AL2(M#ULIODL,E#IODLRP,WIOINS-MAP)   INSORD                       
         DC    AL2(M#ULIOFX,E#IOFXRP,WIOFAX-MAP)   FAX                          
         DC    AL2(E#IOFXLN,E#IOFXRP,WIOFXL-MAP)   FAX LINE OF TEXT             
         DC    AL2(M#ULIOST,E#IOSTRP,WIOSTA-MAP)   STATUS                       
         DC    AL2(M#ULIOIQ,E#IOIQRP,WIOINQ-MAP)   STATUS INQUIRY               
         DC    AL2(M#ULIODW,E#IODWRP,WIODWN-MAP)   IO     DOWNLOAD              
MAPX     DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        INSORD SCREEN                                                          
*                                                                               
WIOINS   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#IOLKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
******                                                                          
******   DC    AL2(D#IOLKEY),AL1(LIOBSB1Q)                                      
******   DC    AL2(LNKWIOKY-SYSD),AL1(L'LNKWIOKY)                               
******   DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
WIOINSX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WIOSTA   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#IOLKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#HSTYID),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKHSTID-SYSD),AL1(L'LNKHSTID)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#IORSTA),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTSTAH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
WIOSTAX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WIOFAX   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#IOLKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKWIOKY-SYSD),AL1(L'LNKWIOKY)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#HSTYID),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKHSTID-SYSD),AL1(L'LNKHSTID)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CPYFXP),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKCPYFX-SYSD),AL1(L'LNKCPYFX)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#MANSEN),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKMANSD-SYSD),AL1(L'LNKMANSD)                               
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#E_MAIL),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXEMLH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#RECPNM),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAFXNM-SYSD),AL1(L'LNKAFXNM)                               
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#TYPE_2),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXTYP1H-T41EFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ADRFAX),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXFAX1H-T41EFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SUPCOS),AL1(LIOBSB2Q)                                      
         DC    AL2(SFXSCS1H-T41EFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
******                                                                          
******   DC    AL2(D#LINTXT),AL1(LIOBSB1Q)                                      
******   DC    AL2(LNKAFXLN-SYSD),AL1(L'LNKAFXLN)                               
******   DC    AL1(LIODINDX,0)                                                  
*                                                                               
WIOFAXX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WIOFXL   DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#LINTXT),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAFXLN-SYSD),AL1(L'LNKAFXLN)                               
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
WIOFXLX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WIOINQ   DS    0XL(LIODL)                                                       
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
         DC    AL2(D#IOLKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(LNKAIO-SYSD),AL1(L'LNKAIO)                                   
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
WIOINQX DC     AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WIODWN   DS    0XL(LIODL)                                                       
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
         DC    AL2(SSTCLTH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(QFPRD-SYSD),AL1(L'QFPRD)                                     
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB2Q)                                       
         DC    AL2(SSTPERH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(SSTPUBH-T41EFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#EOR),AL1(LIOBSB1Q)                                         
         DC    AL2(LNKEOR-SYSD),AL1(1)                                          
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
WIODWNX DC     AL2(0)                                                           
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
       ++INCLUDE PPWIOFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFDD          WIO INSORD MAINT SCREEN                      
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFCD          WIO FAX    MAINT SCREEN                      
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPWIOFBD          WIO STATUS MAINT SCREEN                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPWIOWRKD                                                      
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
**PAN#1  DC    CL21'009PPWIO05   12/17/07'                                      
         END                                                                    
