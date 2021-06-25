*          DATA SET PPPNV05    AT LEVEL 076 AS OF 06/12/18                      
*PHASE T41D05A                                                                  
*                                                                               
         TITLE 'T41D05 - LNK/INV COMMUNICATION MODULE'                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* KWAN 04/17/18 PRISMA INVOICE ID                                     *         
*                                                                     *         
* KWAN 08/01/14 PRISMA INVOICES                                       *         
*                                                                     *         
* KWAN 09/14/11 ELECTRONIC INVOICES                                   *         
*                                                                     *         
* BOBY 10/08    ADD DISCREPANCY COMMENT CODE TO INVOICE DETAILS       *         
*                                                                     *         
* BOBY 03/06    ADD INVOICE HISTORY DOWNLOAD                          *         
*                                                                     *         
* BOBY 03/05    SKIP COMMENT PROCESSING IF ERROR FOUND ALREADY        *         
*                                                                     *         
* KWAN 11/05/04 EXTRA LINKIO GET CALL TO ENSURE PROCESS IS IN SYNC    *         
*                                                                     *         
* KWAN 03/15/04 HANDLING DELETED INVOICES                             *         
*                                                                     *         
* KWAN 08/21/03 CONTROL DATA TRAIFFIC BTW LINK AND INVOICE PROGRAMS   *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41D05 - LNK/INV COMMUNICATION MODULE'                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D05 - LNK/INV COMMUNICATION MODULE                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41D00 (PNV CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     COMMUNICATES BTW LINK (AB) AND INVOICE PROGRAM        *         
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
         TITLE 'T41D05 - LNK/INV COMMUNICATION MODULE'                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
T41D05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK05X-WORK05D,T41D05,CLEAR=YES                                 
*                                                                               
         LR    R4,RC                                                            
         USING WORK05D,R4          R4=A(LOCAL STORAGE)                          
*                                                                               
         BASR  R5,0                                                             
         AHI   R5,GLOBALS-*                                                     
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         L     RE,ACOMFACS                                                      
         MVC   ALINKIO,CLINKIO-COMFACSD(RE)                                     
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
*                                                                               
         TITLE 'PPPNV05 - PRINT NEW INVOICE CONTROLLER - VCOMMON'               
*                                                                               
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
         LA    RF,T41D05(RF)                                                    
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1  ,                   RETURN TO CALLER                             
         DROP  RB                                                               
*                                                                               
* COMMON ROUTINE ADDRESSES                                                      
*                                                                               
VBRANCH  DS    0X                  ALIGNMENT                                    
         DC    AL2(VLIOINI-T41D05) LINKIO INITIALIZATION                        
         DC    AL2(VLIOGTH-T41D05) LINKIO HEADER "GET" (REC AND ACT)            
         DC    AL2(VLIOGTD-T41D05) LINKIO DATA   "GET" (DATA)                   
         DC    AL2(VLIOPUT-T41D05) LINKIO PUT                                   
         DC    AL2(VLIOERR-T41D05) LINKIO ERROR                                 
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         TITLE 'T41D05 - LINKIO INITIALIZATION'                                 
*                                                                               
***********************************************************************         
*                                                                     *         
* LINKIO INITIALIZATION, WILL SET LINK CALL SWITCH                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOINI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DDLNKSW,0           INIT LINK CALL SWITCH                        
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',WRKTEMP1,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF                                                    
         BE    VLIOINIX                                                         
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WRKTEMP1(12),=C'PRILINPRIINV'                                    
         BNE   VLIOINIX                                                         
*                                                                               
         L     R3,ATIA                                                          
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD                                       
*                                                                               
         LA    R0,LIOBD+L'LIOB                                                  
         ST    R0,LIOBAREC                                                      
*                                                                               
         AHI   R0,3000             MAX SIZE FOR LINKIO REC USES                 
         ST    R0,LIOBABUF                                                      
*                                                                               
         MVC   LIOBACOM,ACOMFACS                                                
*                                                                               
         LA    RF,MAP                                                           
         STCM  RF,15,LIOBAMAP                                                   
*                                                                               
         MVI   LIOBMSYS,4          PRINT SYSTEM MSGS                            
*                                                                               
*                                  MULTIPLE OUTPUT RECS MODE                    
*                                  USER PROVIDES RECORD CODES                   
         MVI   LIOBINDS,LIOBINRM                                                
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAINI',LIOBD)                                   
         BNE   VLIOINIX                                                         
         MVI   DDLNKSW,C'Y'        YES, IT IS A LINK CALL                       
*                                                                               
VLIOINIX DS    0H                                                               
         J     EXIT                                                             
         DROP  RB                                                               
*                                                                               
         TITLE 'T41D05 - RECORD AND ACTION FIELDS'                              
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS LINKIO "GET" WILL SET RECORD AND ACTION FIELDS                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOGTH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         CLI   DDLNKSW,C'Y'        LINK CALL?                                   
         BE    *+6                                                              
         DC    H'0'                IT HAS TO BE A LINK CALL!                    
         CLI   DDLNKEOF,C'Y'       NO MORE REQ RECS TO PROCESS?                 
         BNE   *+6                                                              
         DC    H'0'                SOMETHING FROM HERE...                       
*                                                                               
         CLI   DDLNKEOF,C'N'       NO YET EOF?                                  
         BE    VLGTH20             ALREADY DID A LINKIO "GET"                   
*                                                                               
         OI    LIOBINDS,LIOBIRET   RETURN TO CALLER (BREAK)                     
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         JH    ALLDONE                                                          
*                                                                               
VLGTH20  MVI   LIOSCRSW,0          RESET SCREEN SWITCH                          
*                                                                               
         MVI   DDLNKEOF,0          CLEAR EOF SWITCH FOR NEXT TIME               
*                                                                               
         LA    RE,RECACTAB                                                      
*                                                                               
VLGTH30  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    VLGTH40                                                          
*                                                                               
         CLC   LIOBMAP#,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,RECACT1Q(RE)     NEXT ENTRY IN TABLE                          
         B     VLGTH30                                                          
*                                                                               
         MVC   DDLNKACT,2+0(RE)                                                 
         MVC   DDLNKREC,2+1(RE)                                                 
*                                                                               
VLGTH40  CLI   DDLNKACT,0          SUCCESSFULLY SET?                            
         BNE   *+6                                                              
         DC    H'0'                INVALID REQ RECORD CODE                      
*                                                                               
         MVC   CONREC,SPACES                                                    
         MVC   CONACT,SPACES                                                    
*                                                                               
         CLI   DDLNKREC,DDLK_HDR   RECORD IS HEADER?                            
         BNE   *+14                                                             
         MVC   CONREC(07),=C'INVOICE'                                           
         MVI   CONRECH+5,7                                                      
*                                                                               
         CLI   DDLNKREC,DDLK_DET   RECORD IS DETAIL?                            
         BNE   *+14                                                             
         MVC   CONREC(06),=C'DETAIL'                                            
         MVI   CONRECH+5,6                                                      
*                                                                               
         CLI   DDLNKREC,DDLK_ACT   RECORD IS ACTIVITY?                          
         BNE   *+14                                                             
         MVC   CONREC(08),=C'ACTIVITY'                                          
         MVI   CONRECH+5,8                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_ADD   ACTION IS ADD?                               
         BNE   *+14                                                             
         MVC   CONACT(03),=C'ADD'                                               
         MVI   CONACTH+5,3                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_CHG   ACTION IS CHANGE?                            
         BNE   *+14                                                             
         MVC   CONACT(05),=C'MAINT'                                             
         MVI   CONACTH+5,5                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_DEL   ACTION IS DELETE?                            
         BNE   *+14                                                             
         MVC   CONACT(06),=C'ABDEL'                                             
         MVI   CONACTH+5,5                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_ULK   ACTION IS UNLINK?                            
         BNE   *+14                                                             
         MVC   CONACT(06),=C'UNLINK'                                            
         MVI   CONACTH+5,6                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_DMT   ACTION IS DELETE MATCHED                     
         BNE   *+14                                                             
         MVC   CONACT(06),=C'DMATCH'                                            
         MVI   CONACTH+5,6                                                      
*                                                                               
         CLI   DDLNKACT,DDLK_HST   ACTION IS HISTORY                            
         BNE   *+14                                                             
         MVC   CONACT(07),=C'HISTORY'                                           
         MVI   CONACTH+5,7                                                      
*                                                                               
         MVI   LIOSCRSW,LIOS_TOP   TOP SCREEN IS SET                            
         B     VLIOGTHX                                                         
*                                                                               
ALLDONE  MVI   DDLNKEOF,C'Y'       NO MORE REQ RECS                             
*                                                                               
VLIOGTHX DS    0H                                                               
         J     EXIT                                                             
         DROP  RB                                                               
*                                                                               
         TITLE 'T41D05 - DATA FIELDS'                                           
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS LINKIO "GET" WILL SET DATA FIELDS                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOGTD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         CLI   DDLNKSW,C'Y'        LINK CALL?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DDLNKEOF,C'Y'       NO MORE REQ RECS TO PROCESS?                 
         BNE   *+6                                                              
         DC    H'0'                SOMETHING FROM HERE...                       
         CLI   DDLNKACT,0          ACTION SWITCH IS SET?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LIOSCRSW,0          RESET SCREEN SWITCH                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         JH    ALLDONE                                                          
         BE    VLGTD10                                                          
*                                                                               
* EXTRA LINKIO GET TO ENSURE PROCESS IS STILL IN SYNC                           
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         JH    ALLDONE                                                          
         BE    *+6                                                              
         DC    H'0'                ALL DATA FIELDS SHOULD BE PROC'D             
*                                                                               
VLGTD10  MVI   LIOSCRSW,LIOS_BOT   BOTTOM SCREEN IS SET                         
*                                                                               
         OC    WRKACOMM,WRKACOMM   COMMENT DATA MAP PRESENT?                    
         BZ    *+14                                                             
         MVI   DDCOMSW,C'Y'        NEED TO ADD COMMENTS                         
         MVC   ALNKCOMM,WRKACOMM   PASS ADDRESS OF COMMENT                      
*                                                                               
         CLI   WRKIVSRC,C' '       ANYTHING IN INVOICE SOURCE?                  
         JNH   *+10                                                             
         MVC   SVINVSRC,WRKIVSRC   SAVE IT FOR LATER PROCESSING                 
*                                                                               
         MVC   SVIVPSID,WRKIPSID   SAVE PRISMA INVOICE ID                       
*                                                                               
*        FILL IN DETAIL SCREEN                                                  
*                                                                               
         CLI   DDLNKREC,DDLK_DET   DETAIL RECORD?                               
         BNE   VLGTD50                                                          
*                                                                               
         BRAS  RE,VGETMCP          GET MED/CLT/PUB FIELDS                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         USING PNVHKEY,R6                                                       
*                                                                               
         MVC   DTLMED,SPACES                                                    
         MVC   DTLMED(L'QMED),WRKINVKY                                          
         MVI   DTLMEDH+5,L'QMED                                                 
*                                                                               
         MVC   DTLCLT,SPACES                                                    
         MVC   DTLCLT(L'PNVHCLT),PNVHCLT                                        
         MVI   DTLCLTH+5,L'PNVHCLT                                              
*                                                                               
         MVC   DTLPUB,SPACES                                                    
         GOTO1 VPUBEDIT,DMCB,(C'0',PNVHPUB),DTLPUB                              
         LA    RF,L'DTLPUB                                                      
         LA    RE,DTLPUB+L'DTLPUB-1                                             
         CLI   0(RE),C' '                                                       
         BNE   *+16                                                             
         MVI   0(RE),0             CLEAR IT                                     
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-16                                                             
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD INPUT LENGTH                             
         STC   RF,DTLPUBH+5                                                     
*                                                                               
         MVC   DTLINV,SPACES                                                    
         MVC   DTLINV(L'PNVHINV#),PNVHINV#                                      
         MVI   DTLINVH+5,PNVHI#MX                                               
*                                                                               
         CLI   DDLNKACT,DDLK_DEL   ACTION IS DELETE (ABDEL)?                    
         BE    *+8                                                              
         CLI   DDLNKACT,DDLK_ULK   ACTION IS UNLINK?                            
         BE    *+8                                                              
         CLI   DDLNKACT,DDLK_DMT   ACTION IS DELETE MATCHED?                    
         BNE   VLGTD20                                                          
*                                                                               
         LA    RE,MINMKEY          INVOICE MASTER KEY                           
         USING PNVKEY,RE                                                        
*                                                                               
         MVC   QMED,PNVKMED        SET MEDIA                                    
         MVC   QSER#,PNVKSER#      SET INVOICE SERIAL NUMBER                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         MVC   WORK(L'DTLDTL#),DTLDTL#                                          
         LA    RF,L'DTLDTL#                                                     
         LA    RE,WORK+L'DTLDTL#-1                                              
*                                                                               
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-12                                                             
*                                                                               
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)         PACK DETAIL SEQUENCE NUMBER                  
*                                                                               
         CVB   RF,DUB              CVB                                          
         STCM  RF,3,QDSQN          SET DETAIL SEQUENCE NUMBER                   
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTD20  CLI   DDLNKACT,DDLK_ADD   ACTION IS ADD?                               
         BE    *+12                                                             
         CLI   DDLNKACT,DDLK_CHG   ACTION IS CHG (MAINT)?                       
         BNE   VLIOGTDX                                                         
*                                                                               
         OC    WRKADCAP,SPACES                                                  
         CLC   WRKADCAP,SPACES     ANYTHING IN AD CAPTION?                      
         BE    VLGTD22                                                          
         MVC   DTLCAP1(L'DTLCAP1),WRKADCAP                                      
         MVI   DTLCAP1H+5,L'DTLCAP1                                             
         CLC   WRKADCAP+L'DTLCAP1(L'DTLCAP2),SPACES                             
         BE    VLGTD22                                                          
         MVC   DTLCAP2(L'DTLCAP2),WRKADCAP+L'DTLCAP1                            
         MVI   DTLCAP2H+5,L'DTLCAP2                                             
*                                                                               
VLGTD22  CLI   DTLRTEH+5,1         * AND SOMETHING ELSE?                        
         BL    VLGTD24                                                          
*                                                                               
         CLI   DTLRTE,C'*'         FROZEN RATE?                                 
         BNE   VLGTD24                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DTLRTEH+5                                                     
         MVC   WRKTEMP1(L'DTLRTE),DTLRTE                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    DTLRTE(0),DTLRTE                                                 
*                                                                               
         BCTR  RE,0                BY PASS * CHAR                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTLRTE(0),WRKTEMP1+1                                             
*                                                                               
         AHI   RE,1                                                             
         STC   RE,DTLRTEH+5        LENGTH W/O * CHAR                            
         OI    DTLRTEH+6,X'80'                                                  
*                                                                               
VLGTD24  OC    WRKINSKY,SPACES                                                  
*                                                                               
         CLC   WRKINSKY,SPACES     INSERTION KEY IS SENT?                       
         BE    VLIOGTDX                                                         
*                                                                               
         MVC   DTLBS#,SPACES                                                    
         MVC   DTLBS#(L'WRKINSKY-L'QMED-L'QCLT),WRKINSKY+L'QMED+L'QCLT          
         OI    DTLBS#H+4,FINPNUM                                                
         MVI   DTLBS#H+5,L'WRKINSKY-L'QMED-L'QCLT                               
*                                                                               
         MVC   DTLDCL(L'QCLT),WRKINSKY+L'QMED                                   
         MVI   DTLDCLH+5,L'QCLT                                                 
         XC    DTLDPR,DTLDPR       NOT NEEDED IF INS KEY IS PRESENT             
         MVI   DTLDPRH+5,0                                                      
         XC    DTLDPB,DTLDPB       NOT NEEDED IF INS KEY IS PRESENT             
         MVI   DTLDPBH+5,0                                                      
         B     VLIOGTDX                                                         
*                                                                               
VLGTD50  CLI   DDLNKREC,DDLK_HDR   HEADER RECORD?                               
         BNE   VLGTD70                                                          
*                                                                               
         CLI   DDLNKACT,DDLK_CHG   ACTION IS CHG (MAINT)?                       
         BE    *+12                                                             
         CLI   DDLNKACT,DDLK_DEL   ACTION IS DEL (ABDEL)?                       
         BNE   VLIOGTDX                                                         
*                                                                               
         BRAS  RE,VGETMCP          GET MED/CLT/PUB FIELDS                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         USING PNVHKEY,R6                                                       
*                                                                               
         MVC   HDRMED,SPACES                                                    
         MVC   HDRMED(L'QMED),WRKINVKY                                          
         MVI   HDRMEDH+5,L'QMED                                                 
*                                                                               
         MVC   HDRCLT,SPACES                                                    
         MVC   HDRCLT(L'PNVHCLT),PNVHCLT                                        
         MVI   HDRCLTH+5,L'PNVHCLT                                              
*                                                                               
         MVC   HDRPUB,SPACES                                                    
         GOTO1 VPUBEDIT,DMCB,(C'0',PNVHPUB),HDRPUB                              
         LA    RF,L'HDRPUB                                                      
         LA    RE,HDRPUB+L'HDRPUB-1                                             
         CLI   0(RE),C' '                                                       
         BNE   *+16                                                             
         MVI   0(RE),0             CLEAR IT                                     
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-16                                                             
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD INPUT LENGTH                             
         STC   RF,HDRPUBH+5                                                     
*                                                                               
         MVC   HDRINV,SPACES                                                    
         MVC   HDRINV(L'PNVHINV#),PNVHINV#                                      
         MVI   HDRINVH+5,PNVHI#MX                                               
*                                                                               
         CLI   DDLNKACT,DDLK_DEL   ACTION IS DEL (ABDEL)?                       
         BNE   VLIOGTDX                                                         
         LA    RE,MINMKEY          INVOICE MASTER KEY                           
         USING PNVKEY,RE                                                        
         MVC   QMED,PNVKMED        SET MEDIA                                    
         MVC   QSER#,PNVKSER#      SET INVOICE SERIAL NUMBER                    
         DROP  RE                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTD70  DS    0H                  FUTURE INVOICE REC TYPE(S)                   
*                                                                               
*        ACTIVITY RECORD                                                        
*                                                                               
         CLI   DDLNKREC,DDLK_ACT   ACTIVITY RECORD?                             
         BNE   VLGTD90                                                          
*                                                                               
         CLI   DDLNKACT,DDLK_HST   ACTION IS HISTORY?                           
         BNE   VLIOGTDX                                                         
*                                                                               
         MVC   LNKAIO,WRKAINVK     SAVE A(DATA)                                 
*                                                                               
         L     R2,LNKAIO           POINT TO RETURNED INVOICE KEY                
         USING WKRDATD,R2          ESTABLISH WORKER DATA FIELD                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WKDTRLEN       GET DATA LENGTH                              
         LR    R0,RF               SAVE ELEMENT LENGTH                          
         SHI   RF,WKDTHDLQ         SUBTRACT HEADER LENGTH                       
         BP    *+6                                                              
         DC    H'0'                   NO DATA                                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKINVKY(0),WKDTDATA  SAVE INVOICE KEY                           
*                                                                               
         MVC   LNKINVKY,WRKINVKY   SAVE INVOICE KEY                             
                                                                                
*                                                                               
         BRAS  RE,VGETMCP          GET MED/CLT/PUB FIELDS                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         USING PNVHKEY,R6                                                       
*                                                                               
         MVC   HDAMED,SPACES                                                    
         MVC   HDAMED(L'QMED),WRKINVKY                                          
         MVI   HDAMEDH+5,L'QMED                                                 
*                                                                               
         MVC   HDACLT,SPACES                                                    
         MVC   HDACLT(L'PNVHCLT),PNVHCLT                                        
         MVI   HDACLTH+5,L'PNVHCLT                                              
*                                                                               
         MVC   HDAPUB,SPACES                                                    
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(C'0',PNVHPUB),HDAPUB                              
*                                                                               
         LA    RF,L'HDAPUB                                                      
         LA    RE,HDAPUB+L'HDAPUB-1                                             
*                                                                               
         CLI   0(RE),C' '                                                       
         BNE   *+16                                                             
         MVI   0(RE),0             CLEAR IT                                     
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-16                                                             
*                                                                               
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD INPUT LENGTH                             
*                                                                               
         STC   RF,HDAPUBH+5                                                     
*                                                                               
         MVC   HDAINV,SPACES                                                    
         MVC   HDAINV(L'PNVHINV#),PNVHINV#                                      
         MVI   HDAINVH+5,PNVHI#MX                                               
*                                                                               
         LA    RE,MINMKEY          INVOICE MASTER KEY                           
         USING PNVKEY,RE                                                        
*                                                                               
         MVC   QMED,PNVKMED        SET MEDIA                                    
         MVC   QSER#,PNVKSER#      SET INVOICE SERIAL NUMBER                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         B     VLIOGTDX                                                         
*                                                                               
VLGTD90  DS    0H                  FUTURE INVOICE REC TYPE(S)                   
*                                                                               
VLIOGTDX DS    0H                                                               
         XIT1                                                                   
         DROP  RB,R2                                                            
*                                                                               
         TITLE 'T41D05 - RETURN DATA TO PC'                                     
*                                                                               
***********************************************************************         
*                                                                     *         
* INTERFACE TO LINKIO TO RETURN DATA TO PC                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOPUT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         OC    WRKACOMM,WRKACOMM   COMMENT PRESENT?                             
         BZ    *+12                                                             
         CLI   DDCOMSW,C'C'        COMMENT IS PROCESSED?                        
         BNE   VLIOPUTX                                                         
*                                                                               
         CLI   SVINVSRC,INVPRM_Q    PRISMA INVOICE?                             
         JE    *+12                                                             
         CLI   SVINVSRC,INVRAD_Q    RADIA INVOICE?                              
         JNE   VLPUT20                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IVPSID),    +        
               ('LD_CHARQ',SVIVPSID),(L'SVIVPSID,0)                             
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'QMED),QMED                                                
         UNPK  WORK+L'QMED(2*L'QSER#+1),QSER#(L'QSER#+1)                        
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    +        
               ('LD_CHARQ',WORK),(L'WRKINVKY,0)                                 
*                                                                               
         CLI   DDLNKREC,DDLK_DET   DETAIL RECORD?                               
         JNE   VLPUT50                                                          
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'QMED),QMED                                                
         MVC   WORK+L'QMED(L'QCLT),QCLT                                         
         MVC   WORK+L'QMED+L'QCLT(L'DTLBS#),DTLBS#                              
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSKEY),    +        
               ('LD_CHARQ',WORK),(L'WRKINSKY,0)                                 
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ITMSQN),    +        
               ('LD_UBINQ',QDSQN),(L'QDSQN,0)                                   
*                                                                               
         J     VLPUT50                                                          
*                                                                               
VLPUT20  CLI   DDLNKREC,DDLK_HDR   HEADER RECORD?                               
         BNE   VLPUT30                                                          
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'QMED),QMED                                                
         UNPK  WORK+L'QMED(2*L'QSER#+1),QSER#(L'QSER#+1)                        
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    +        
               ('LD_CHARQ',WORK),(L'WRKINVKY,0)                                 
*                                                                               
         B     VLPUT90             DONE WITH HDR UPLOAD, NEXT REQ REC           
*                                                                               
VLPUT30  CLI   DDLNKREC,DDLK_DET   DETAIL RECORD?                               
         BNE   VLPUT50                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ITMSQN),    +        
               ('LD_UBINQ',QDSQN),(L'QDSQN,0)                                   
*                                                                               
         B     VLPUT90             DONE WITH DET UPLOAD, NEXT REQ REC           
*                                                                               
VLPUT50  CLI   DDLNKREC,DDLK_ACT   ACTIVITY RECORD?                             
         BNE   VLPUT70                                                          
*                                                                               
         B     VLPUT90             DONE WITH ACTIVITY  NEXT REQ REC             
*                                                                               
VLPUT70  DS    0H                  FOR FUTURE REPLIES                           
*                                                                               
VLPUT90  DS    0H                  ANY OTHER LAST MINUTE REPLIES                
*                                                                               
VLIOPUTX DS    0H                                                               
         J     EXIT                                                             
         DROP  RB                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
* INTERFACE TO LINKIO TO RETURN ERROR, R2 POINTS TO ERR FIELD         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VLIOERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR INILIOB                                                          
*                                                                               
         XC    HALF,HALF           FOR UNKNOW ERROR FIELD                       
*                                                                               
         CLI   SVINVSRC,INVPRM_Q    PRISMA INVOICE?                             
         JE    *+12                                                             
         CLI   SVINVSRC,INVRAD_Q    RADIA INVOICE?                              
         JNE   VLERR10                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IVPSID),    +        
               ('LD_CHARQ',SVIVPSID),(L'SVIVPSID,0)                             
*                                                                               
VLERR10  CLI   DDLNKREC,DDLK_HDR   INVOICE HEADER RECORD?                       
         BNE   *+12                                                             
         LA    R6,ADDINVH          POINT TO INV HEADER FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_DET   INVOICE DETAIL RECORD?                       
         BNE   *+12                                                             
         LA    R6,ADDINVD          POINT TO INV DETAIL FIELDS                   
         B     VLERR20                                                          
*                                                                               
         CLI   DDLNKREC,DDLK_ACT   ACTIVITY DETAIL RECORD?                      
         BNE   *+12                                                             
         LA    R6,ACTINVD          POINT TO INV DETAIL FIELDS                   
         B     VLERR20                                                          
*                                                                               
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
VLERR20  LR    RF,R2                                                            
         LA    RE,T41DFFD                                                       
         SR    RF,RE                                                            
         ST    RF,FULL             DISPLACEMENT TO ERROR FIELD                  
*                                                                               
         USING LIODD,R6                                                         
*                                                                               
VLERR26  OC    0(2,R6),0(R6)       END OF MAP TABLE?                            
         BZ    VLERR40                                                          
*                                                                               
         TM    LIODIND1,LIODITFH   TWA FIELD HEADER?                            
         BZ    VLERR28                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LIODDISP                                                    
*                                                                               
         C     RE,FULL             SAME DISPLACEMENT AS ERROR FIELD?            
         BNE   VLERR28                                                          
*                                                                               
         MVC   HALF,LIODDMAP       MAP CODE OF ERROR FLD TO BE REPLIED          
*                                                                               
         B     VLERR50                                                          
*                                                                               
VLERR28  LA    R6,LIODL(R6)        POINT TO NEXT ENTRY IN MAP TABLE             
         B     VLERR26                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
VLERR40  CLI   DDLNKREC,DDLK_DET   RECORD IS DETAIL?                            
         BNE   VLERR50                                                          
*                                                                               
         LA    RE,DTLMEDH                                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#MEDCOD)                                              
         B     VLERR50                                                          
*                                                                               
         LA    RE,DTLCLTH                                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#CLTCOD)                                              
         B     VLERR50                                                          
*                                                                               
         LA    RE,DTLPUBH                                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#PUBCOD)                                              
         B     VLERR50                                                          
*                                                                               
         LA    RE,DTLINVH                                                       
         CR    R2,RE                                                            
         BNE   *+14                                                             
         MVC   HALF,=AL2(D#VINVNO)                                              
         B     VLERR50                                                          
*                                                                               
VLERR50  DS    0H                                                               
*                                                                               
         CLI   DDLNKACT,DDLK_HST   IF ACTION IS HISTORY                         
         BE    VLERR60                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    +        
               ('LD_UBINQ',HALF),(L'HALF,0)                                     
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',CONHEAD),(L'CONHEAD,0)                               
*                                                                               
* CLEAR COMMENT SWITCH IN CASE INVOICE DETAILS BEING HANDLED                    
*                                                                               
         MVI   DDCOMSW,0           INIT COMMENT SWITCH                          
*                                                                               
* ANOTHER LINKIO "GET" IS NEEDED TO CK EOF IS REACHED                           
*                                                                               
         MVI   DDLNKEOF,C'N'       NOT YET EOF - INDICATES WE DID               
*                                     FIRST GET FOR RECORD/ACTION DATA          
         MVI   LIOBINDS,LIOBIRET   RETURN TO CALLER (BREAK)                     
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         JH    ALLDONE                                                          
         B     VLIOERRX                                                         
*                                                                               
*        INVOICE HISTORY ERROR                                                  
*                                                                               
VLERR60  DS    0H                                                               
*                                                                               
*        MAKE SURE WE HAVE READ TO THE END OF THE FILE                          
*                                                                               
         MVI   LIOBINDS,LIOBIRET   RETURN TO CALLER (BREAK)                     
*                                                                               
         OI    LIOBINDS,LIOBINRM+LIOBIMLT NO REPLY MAPCODE GENERATED            
*                                     MULTIPLE RECORDS REPLIED                  
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
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INVHSE)              
*                                                                               
*        INVOICE KEY                                                            
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INVKEY),    X        
               ('LD_CHARQ',LNKINVKY),(L'LNKINVKY,0)                             
*                                                                               
*        LINE ITEM SEQUENCE NUMBER                                              
*                                                                               
         OC    QDSQN,QDSQN         IF THERE IS A DETAIL SEQUENCE NUMBER         
         BE    VLIOESQX                                                         
*                                                                               
         EDIT  (2,QDSQN),(3,WORK),ALIGN=LEFT                                    
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ITMSQN),    X        
               ('LD_CHARQ',WORK),((R0),0)                                       
*                                                                               
VLIOESQX DS    0H                                                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',CONHEAD),(L'CONHEAD,0)                               
*                                                                               
*        CLOSE THE WORKER FILE                                                  
*                                                                               
         GOTOR VLINKIO,DMCB,('LIOACLO',LIOBD)                                   
*                                                                               
* CLEAR COMMENT SWITCH IN CASE INVOICE DETAILS BEING HANDLED                    
*                                                                               
         MVI   DDCOMSW,0           INIT COMMENT SWITCH                          
         MVI   DDLNKEOF,C'Y'       END OF FILE                                  
*                                                                               
VLIOERRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41D05 - FROM INVOICE KEY, GET MED/CLT/PUB FIELDS'              
*                                                                               
***********************************************************************         
*                                                                     *         
* WRKINVKY  -  MEDIA AND INVOICE SERIAL NUMBER (CHAR FORMAT)          *         
* ELEMENT   -  WILL RETURN LOOKED UP INVOICE HEADER ELEMENT           *         
*                                                                     *         
***********************************************************************         
*                                                                               
VGETMCP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    WRKINVKY,SPACES                                                  
         CLC   WRKINVKY,SPACES                                                  
         BE    VGMCNOTF            INVALID INVOICE KEY                          
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
         CLI   MINERR,0            MUST FIND INVOICE                            
         BNE   VGMCNOTF                                                         
*                                                                               
         CLI   MINOPEN,C'Y'        MUST FIND INVOICE                            
         BNE   VGMCNOTF                                                         
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORKAREA                       
         LA    R6,ELEMENT          BUILD HEADER ELEMENT KEY                     
         USING PNVHKEY,R6                                                       
         MVI   PNVHKCDE,PNVHKIDQ   SET HEADER ELM CODE                          
         GOTOR GETELM,DMCB,ELEMENT READ FOR ELEMENT                             
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
VGMCPX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
VGMCNOTF DS    0H                                                               
         LHI   RF,PPEINVNF         INVOICE NOT FOUND                            
*                                                                               
VGMCERR  DS    0H                                                               
*                                                                               
         MVI   ERROR,0             CLEAR OUT RESIDUAL ERROR                     
         STCM  RF,3,PERROR         SET ERROR CODE                               
         GOTOR ERREXIT                                                          
*                                                                               
         DROP  RB,RE,R7,R6                                                      
         EJECT                                                                  
*                                                                               
* INITIALIZE LIOB ADDRESSES FOR GETS/PUTS                                       
*                                                                               
INILIOB  L     R3,ATIA                                                          
         LA    R0,WORK05D                                                       
         ST    R0,LIOBASB1                                                      
         LA    R0,T41DFFD                                                       
         ST    R0,LIOBASB2                                                      
INILIOBX BR    RE                                                               
*                                                                               
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACTAB DS    0H                                                               
         DC    AL2(M#ULAIH),AL1(DDLK_ADD),AL1(DDLK_HDR)                         
RECACT1Q EQU   *-RECACTAB                                                       
         DC    AL2(M#ULAII),AL1(DDLK_ADD),AL1(DDLK_DET)                         
         DC    AL2(M#ULCIH),AL1(DDLK_CHG),AL1(DDLK_HDR)                         
         DC    AL2(M#ULCII),AL1(DDLK_CHG),AL1(DDLK_DET)                         
         DC    AL2(M#ULDIH),AL1(DDLK_DEL),AL1(DDLK_HDR)                         
         DC    AL2(M#ULDII),AL1(DDLK_DEL),AL1(DDLK_DET)                         
         DC    AL2(M#ULULK),AL1(DDLK_ULK),AL1(DDLK_DET)                         
         DC    AL2(M#ULDMT),AL1(DDLK_DMT),AL1(DDLK_DET)                         
         DC    AL2(M#ULNVHS),AL1(DDLK_HST),AL1(DDLK_ACT)                        
         DC    X'FF'                                                            
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAP      DS    0XL(LIORL)                                                       
         DC    AL2(M#ULAIH,E#INVAH,ADDINVH-MAP)                                 
         DC    AL2(M#ULAII,E#INVAI,ADDINVD-MAP)                                 
         DC    AL2(M#ULCIH,E#INVCH,CHGINVH-MAP)                                 
         DC    AL2(M#ULCII,E#INVCI,CHGINVD-MAP)                                 
         DC    AL2(M#ULDIH,E#INVDH,DELINVH-MAP)                                 
         DC    AL2(M#ULDII,E#INVDI,DELINVD-MAP)                                 
         DC    AL2(M#ULULK,E#IUNLK,ULKINVD-MAP)                                 
         DC    AL2(M#ULDMT,E#IDLMT,DMTINVD-MAP)                                 
         DC    AL2(M#ULNVHS,E#INVHSH,ACTINVD-MAP)                               
MAPX     DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDINVH  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#MEDCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRMEDH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRCLTH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRPUBH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#VINVNO),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRINVH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVSTA),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRSTAH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB2Q)                                       
         DC    AL2(HDRPERH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVDAT),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRDTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVTOT),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRTOTH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ITOTYP),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRGRSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVTAX),AL1(LIOBSB2Q)                                      
         DC    AL2(HDRTAXH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SPREP),AL1(LIOBSB2Q)                                       
         DC    AL2(HDRREPH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#COMMNT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKACOMM-WORK05D),AL1(L'WRKACOMM)                            
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
ADDINVHX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INSKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINSKY-WORK05D),AL1(L'WRKINSKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#UNTRAT),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLRTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PREMUM),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLPREMH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#NETORD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLNETH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#GRSORD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLGRSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#NUMLIN),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLLNSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INSDAT),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SPCDSC),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLSPCH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ADCAP),AL1(LIOBSB1Q)                                       
         DC    AL2(WRKADCAP-WORK05D),AL1(L'WRKADCAP)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDCLH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDPRH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDPBH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ACTIMP),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLIMPSH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ACPM),AL1(LIOBSB2Q)                                        
         DC    AL2(DTLCPMSH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#DCOMCD),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDCMH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#COMMNT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKACOMM-WORK05D),AL1(L'WRKACOMM)                            
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
ADDINVDX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHGINVH  DS    0XL(LIODL)                                                       
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVSTA),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKISTAT-WORK05D),AL1(L'WRKISTAT)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVSTA+1),AL1(LIOBSB2Q)                                    
         DC    AL2(HDRSTAH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB1Q)                                       
         DC    AL2(WRKPRIOD-WORK05D),AL1(L'WRKPRIOD)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#STEND+1),AL1(LIOBSB2Q)                                     
         DC    AL2(HDRPERH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVDAT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVDT-WORK05D),AL1(L'WRKINVDT)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVDAT+1),AL1(LIOBSB2Q)                                    
         DC    AL2(HDRDTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVTOT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKTOTAL-WORK05D),AL1(L'WRKTOTAL)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVTOT+1),AL1(LIOBSB2Q)                                    
         DC    AL2(HDRTOTH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ITOTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKTOTTY-WORK05D),AL1(L'WRKTOTTY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITOTYP+1),AL1(LIOBSB2Q)                                    
         DC    AL2(HDRGRSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVTAX),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVTAX-WORK05D),AL1(L'WRKIVTAX)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVTAX+1),AL1(LIOBSB2Q)                                    
         DC    AL2(HDRTAXH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SPREP),AL1(LIOBSB1Q)                                       
         DC    AL2(WRKSPREP-WORK05D),AL1(L'WRKSPREP)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#SPREP+1),AL1(LIOBSB2Q)                                     
         DC    AL2(HDRREPH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#COMMNT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKACOMM-WORK05D),AL1(L'WRKACOMM)                            
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
CHGINVHX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHGINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITMSQN),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDTL#H-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INSKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINSKY-WORK05D),AL1(L'WRKINSKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INSKEY+1),AL1(LIOBSB1Q)                                    
         DC    AL2(WRKOINSK-WORK05D),AL1(L'WRKOINSK)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#UNTRAT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKRATE-WORK05D),AL1(L'WRKRATE)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#UNTRAT+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLRTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PREMUM),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKPREM-WORK05D),AL1(L'WRKPREM)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PREMUM+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLPREMH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#NETORD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKNET-WORK05D),AL1(L'WRKNET)                                
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#NETORD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLNETH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#GRSORD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKGROSS-WORK05D),AL1(L'WRKGROSS)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#GRSORD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLGRSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#NUMLIN),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKNLINE-WORK05D),AL1(L'WRKNLINE)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#NUMLIN+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLLNSH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INSDAT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINSDT-WORK05D),AL1(L'WRKINSDT)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INSDAT+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLDTEH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#SPCDSC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKSPDES-WORK05D),AL1(L'WRKSPDES)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#SPCDSC+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLSPCH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ADCAP),AL1(LIOBSB1Q)                                       
         DC    AL2(WRKTEMP1-WORK05D),AL1(L'WRKTEMP1)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ADCAP+1),AL1(LIOBSB1Q)                                     
         DC    AL2(WRKADCAP-WORK05D),AL1(L'WRKADCAP)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKDCLT-WORK05D),AL1(L'WRKDCLT)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLDCLH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKDPRD-WORK05D),AL1(L'WRKDPRD)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLDPRH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKDPUB-WORK05D),AL1(L'WRKDPUB)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLDPBH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ACTIMP),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKAIMPS-WORK05D),AL1(L'WRKAIMPS)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ACTIMP+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLIMPSH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#ACPM),AL1(LIOBSB1Q)                                        
         DC    AL2(WRKACPMS-WORK05D),AL1(L'WRKACPMS)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ACPM+1),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLCPMSH-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#DCOMCD),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKDDCM-WORK05D),AL1(L'WRKDDCM)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#DCOMCD+1),AL1(LIOBSB2Q)                                    
         DC    AL2(DTLDCMH-T41DFFD),AL1(0)                                      
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#COMMNT),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKACOMM-WORK05D),AL1(L'WRKACOMM)                            
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
CHGINVDX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELINVH  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
DELINVHX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITMSQN),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDTL#H-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIVSRC-WORK05D),AL1(L'WRKIVSRC)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#IVPSID),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKIPSID-WORK05D),AL1(L'WRKIPSID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
DELINVDX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ULKINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITMSQN),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDTL#H-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
ULKINVDX DC    AL2(0)                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMTINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKINVKY-WORK05D),AL1(L'WRKINVKY)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITMSQN),AL1(LIOBSB2Q)                                      
         DC    AL2(DTLDTL#H-T41DFFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
DMTINVDX DC    AL2(0)                                                           
*                                                                               
ACTINVD  DS    0XL(LIODL)                                                       
*                                                                               
         DC    AL2(D#INVKEY),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKAINVK-WORK05D),AL1(L'WRKAINVK)                            
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#HISTYP),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKHSTID-WORK05D),AL1(L'WRKHSTID)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#ITMSQN),AL1(LIOBSB1Q)                                      
         DC    AL2(WRKDSQN-WORK05D),AL1(L'WRKDSQN)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
ACTINVDX DC    AL2(0)                                                           
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK05D  DSECT                                                                  
*                                                                               
ALINKIO  DS    A                                                                
*                                                                               
WRKINVKY DS    CL(L'QMED+2*L'PNVKSER#)                                          
WRKINSKY DS    CL(L'QMED+L'QCLT+(2*L'PNVDSER#)-1)                               
WRKOINSK DS    CL(L'WRKINSKY)                                                   
*                                                                               
WRKISTAT DS    CL(L'HDRSTA)        OLD INVOICE HEADER VALUES                    
WRKPRIOD DS    CL(L'HDRPER)                                                     
WRKINVDT DS    CL(L'HDRDTE)                                                     
WRKTOTAL DS    CL(L'HDRTOT)                                                     
WRKTOTTY DS    CL(L'HDRGRS)                                                     
WRKIVTAX DS    CL(L'HDRTAX)                                                     
WRKSPREP DS    CL(L'HDRREP)                                                     
*                                                                               
WRKRATE  DS    CL(L'DTLRTE)                                                     
WRKPREM  DS    CL(L'DTLPREM)                                                    
WRKNET   DS    CL(L'DTLNET)                                                     
WRKGROSS DS    CL(L'DTLGRS)                                                     
WRKNLINE DS    CL(L'DTLLIN)                                                     
WRKINSDT DS    CL(L'DTLDTE)                                                     
WRKSPDES DS    CL(L'DTLSPC)                                                     
WRKADCAP DS    CL(L'DTLCAP1+L'DTLCAP2+1)                                        
WRKDCLT  DS    CL(L'DTLDCL)                                                     
WRKDPRD  DS    CL(L'DTLDPR)                                                     
WRKDPUB  DS    CL(L'DTLDPB)                                                     
WRKAIMPS DS    CL(L'DTLIMPS)                                                    
WRKACPMS DS    CL(L'DTLCPMS)                                                    
WRKDDCM  DS    CL(L'DTLDCM)                                                     
WRKDSQN  DS    XL4                 DETAIL SEQUENCE NUMBER                       
WRKHSTID DS    CL1                 HISTORY TYPE                                 
*                                                                               
WRKIVSRC DS    CL1                 INVOICE SOURCE                               
WRKIPSID DS    CL(L'PNVHPSID)      PRISMA INVOICE ID                            
*                                                                               
WRKTEMP1 DS    XL255                                                            
WRKTEMP2 DS    XL255                                                            
*                                                                               
WRKSVKEY DS    XL(L'KEY)                                                        
*                                                                               
WRKACOMM DS    A                   ADDRESS OF COMMENT DATA MAPS                 
WRKAINVK DS    A                   ADDRESS OF INVOICE KEY DATA MAPS             
*                                                                               
WORK05X  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
       ++INCLUDE PPPNVFDD          INVOICE HEADER MAINT SCREEN                  
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVFCD          INVOICE DETAIL MAINT SCREEN                  
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPPNVF8D          ACTIVITY             SCREEN                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPNVWRKD                                                      
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
**PAN#1  DC    CL21'076PPPNV05   06/12/18'                                      
         END                                                                    
