*          DATA SET SPTAM00    AT LEVEL 006 AS OF 11/08/06                      
*PHASE T22900A                                                                  
*INCLUDE TWABLD                                                                 
*                                                                               
*===============================================================*               
*                                                               *               
* HISTORY                                                       *               
* -------                                                       *               
*                                                               *               
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 08NOV06 006 SEND WEEKLY/DAILY INDICATOR                       *               
* 20JUN02 005 CLIENT STRING SECURITY                            *               
* 05SEP01 004 H06_01Q ONLY 6 CHARS                              *               
* 20AUG01 003 BE CAREFUL OF ZERO LENGTH INPUT PASSWORDS         *               
* 14AUG01 002 TAKE COMMENTS OUT OF H08_02 - MAX IS 255 CHARS    *               
* 05JUN01 001 INITIAL DEVELOPMENT                               *               
*                                                               *               
*===============================================================*               
*                                                                               
T22900   TITLE 'SPTAM00 - SPOT TV AVAIL MANAGER - BASE'                         
T22900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T22900,RR=R2,CLEAR=YES                               
         USING WORKD,RC                                                         
         SPACE 1                                                                
*=================================================================*             
* INITIALIZATION CODE *                                                         
*=================================================================*             
         SPACE 1                                                                
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
*                                                                               
         LR    RE,RC                                                            
         AHI   RE,IOAREA1-WORKD                                                 
         ST    RE,AIO                                                           
         ST    RE,AIO1                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO2                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO3                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
*                                                                               
         L     RE,ASYSPARM                                                      
         L     RE,8(RE)                                                         
         USING SPSYSFAC,RE                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RE                                                               
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VGETFACT,CGETFACT                                                
         DROP  R1                                                               
         SPACE 1                                                                
*===================================================================*           
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
*===================================================================*           
         SPACE 1                                                                
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
*                                                                               
INIT02   ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT04              NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT04   LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT02                                                        
*                                                                               
         LR    RE,RB                                                            
         AHI   RE,VCOMMON-T22900                                                
         LA    R0,VCOMBASN                                                      
         SR    RF,RF                                                            
         LA    R1,READ                                                          
INIT10   DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
*                                                                               
         XC    SVRCVEL,SVRCVEL     CLEAR LAST RECEIVE ELEMENT                   
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INIT30               YES - DON'T RESET THESE!                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'  MAKE SURE NOTHING IN GLOBBER            
*                                                                               
         XC    FLAGS,FLAGS                                                      
         XC    BUYPASS,BUYPASS                                                  
*                                                                               
         L     R0,ATIA             CLEAR TIA DATA BUFFER                        
         ST    R0,DBUFFP                                                        
         LHI   R1,DBUFFL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RF,DBUFFP                                                        
         MVI   0(RF),X'FF'         SET EOT                                      
         AHI   RF,DBUFFL                                                        
         ST    RF,DBUFFX                                                        
*                                                                               
INIT30   DS    0H                                                               
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         GOTO1 VFALINK,DMCB,FABLK  GIVE FALINK CONTROL                          
         B     EXIT                                                             
         EJECT                                                                  
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
* INITIALIZE FALINK                                                             
*====================================================================*          
         SPACE 1                                                                
INIFALNK NTR1                                                                   
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INI2                                                             
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVOLDRCV,SVOLDRCV                                                
*                                                                               
INI2     MVC   SVRESUME,SVXFROV    SAVE THE GLOBBING OVERLAY NUMBER             
         MVI   SVXFROV,0           AND CLEAR THIS FLAG NOW !                    
*                                                                               
         OI    TAMSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    TAMSERVH+6,X'80'                                                 
*                                                                               
                                                                                
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,TAMINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
*                                                                               
         L     R1,ACOMFACS         A(SWITCH)                                    
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
*                                                                               
         L     R0,=A(RECEIVE)                                                   
         A     R0,BASERELO                                                      
         ST    R0,FALARCV                                                       
*                                                                               
         L     R0,=A(SEND)                                                      
         A     R0,BASERELO                                                      
         ST    R0,FALASND                                                       
*                                                                               
         L     R0,=A(BREAK)                                                     
         A     R0,BASERELO                                                      
         ST    R0,FALASTP                                                       
*                                                                               
         L     R0,=A(RESUME)                                                    
         A     R0,BASERELO                                                      
         ST    R0,FALARSM                                                       
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R1,FALAMSG                                                       
*                                                                               
         LA    R1,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-TWAD) A(FALINK SAVED STORAGE)                     
         ST    R1,FALASVE                                                       
*                                                                               
         L     R0,=A(FAMAP)        A(MAP TABLE)                                 
         A     R0,BASERELO                                                      
         ST    R0,FALAMAP                                                       
         ST    R0,AMAPTAB          FOR OTHER OVERLAYS                           
*                                                                               
         MVC   FALAPGS,TWAPGS                                                   
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         EJECT                                                                  
*====================================================================           
* LITERALS AND CONSTANTS                                                        
*====================================================================           
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
BREAK    NTR1  BASE=*,LABEL=*                                                   
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* ON RETURN FROM AN OVERLAY THAT WANTS TO EXIT VIA GLOBBER,         *           
* WE RETURNED TO FALINK WITH AN FAGLB, FAGLB CALL.                  *           
* WHEN CALLED PROGRAM RETURNS TO US, THIS EXIT IS CALLED.           *           
* ON EXIT FROM HERE, AND SVRESUME IS THE OVERLAY TO BE CALLED.      *           
*====================================================================           
         SPACE 1                                                                
RESUME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'                                                     
         BZ    RSM2                NO CONTROL ELEM                              
         CLI   *,FF                SET CC LOW                                   
         BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN HERE                               
*                                                                               
RSM2     GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
*                                                                               
G        USING GLVXFRSY,BLOCK                                                   
         CLC   =C'SPONWS',G.GLVXFRSY  FROM SPOT NWS ?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  G                                                                
*                                                                               
         CR    RB,RB               EXIT WITH CC =                               
*                                                                               
RSMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* CONTROL RECEIVED HERE WHEN TO SEND FALINK DATA                    *           
*====================================================================           
         SPACE 1                                                                
SEND     NTR1  BASE=*,LABEL=*                                                   
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         CLI   SVRESUME,0          TEST XFRCTL RETURN                           
         BE    SEND10                                                           
         LA    R4,SVRESUME-2       POINT 2 BYTES BEFORE OVERLAY NUM             
         B     SEND20                                                           
*                                                                               
SEND10   LA    R4,SOVTAB                                                        
         LA    R5,(SOVTABX-SOVTAB)/L'SOVTAB                                     
SEND12   CLC   SVRCVEL,0(R4)       MATCH FIRST RCV ELCODE3                      
         BE    SEND20                                                           
         LA    R4,L'SOVTAB(R4)                                                  
         BCT   R5,SEND12                                                        
         DC    H'0'                                                             
*                                                                               
SEND20   CLI   2(R4),X'FF'         TEST NOT TO CALL ANYTHING                    
         BE    SEND40              ALAN SAYS A ZED WILL GO OUT !                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),2(R4)       MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
         MVC   SVRESUME,SVXFROV    SAVE THIS OVERLAY NUMBER                     
         CLI   SVXFROV,0           TEST OVLY REQUESTED GLOBBER CALL             
         BE    SEND40                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB                             
         B     SENDX                                                            
*                                                                               
SEND40   GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
*                                                                               
SENDX    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
*==================================================================*            
* SEND OVERLAY LOOKUP TABLE                                        *            
* ENTRIES ARE                                                      *            
*        DS    AL2(FIRST RCV EL CODE)                              *            
*        DS    XL1(SEND OVERLAY NUMBER)                            *            
*        DS    XL1(SPARE)                                          *            
*==================================================================*            
         SPACE 1                                                                
SOVTAB   DS    0AL4                                                             
         DC    AL2(H02Q),X'01',X'00' CAMPAIGN LIST                              
         DC    AL2(H04Q),X'01',X'00' CAMPAIGN RECORD                            
         DC    AL2(H08Q),X'01',X'00' WORK ADD                                   
         DC    AL2(HFDQ),X'FF',X'00' VERSION CODES                              
         DC    AL2(HFEQ),X'FF',X'00' VERSION CODES                              
SOVTABX  EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* CONTROL RECEIVED HERE WHEN FALINK HAS RECEIVED DATA               *           
*===================================================================*           
         SPACE 1                                                                
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESS                  
*                                                                               
RCV10    GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVERR              FALINK ERROR                                 
         BH    RCVX                END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV20                                                            
         SPACE 1                                                                
*====================================================================           
* PRCHDR - PROCESS HEADER ELEMENT                                               
*====================================================================           
         SPACE 1                                                                
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
*                                                                               
         OC    SVRCVEL,SVRCVEL     TEST FIRST ELEMENT                           
         BNZ   RCV10                                                            
         CLC   MHCODE,=X'00FE'     IGNORE FE/FD ELEMS                           
         BE    RCV10                                                            
         CLC   MHCODE,=X'00FD'                                                  
         BE    RCV10                                                            
         MVC   SVRCVEL,MHCODE      SAVE FIRST RECEIVE ELEMENT                   
*                                                                               
         B     RCV10                                                            
         EJECT                                                                  
*====================================================================           
* PROCESS DATA FIELD                                                            
*====================================================================           
         SPACE 1                                                                
RCV20    L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(DATA ENTRY)                             
*                                                                               
         L     R4,FPARMS+4         GET DATA ADDRESS                             
         L     R5,FPARMS+8         GET DATA LENGTH                              
         AHI   R5,-1               SET FOR EX                                   
*                                                                               
         ICM   RF,15,MDUSER        GET PROCESS DATA ROUTINE ADDRESS             
         A     RF,BASERELO                                                      
         BASR  RE,RF               NO RETURN EXPECTED - TRACE USE ONLY          
         DC    H'0'                                                             
*                                                                               
RCVX     CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
RCVERR   GOTO1 SENDMSG             RETURN FALINK ERROR                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* DATA RECEIVE ROUTINES                                           *             
*=================================================================*             
         SPACE 1                                                                
DUMMY    B     RCV10               DUMMY ROUTINE - DO NOTHING                   
*                                                                               
INVERFD  MVC   VERSION,0(R4)       VERSION DATA (HFDQ)                          
         OI    FLAGS,FLSKVRSN                                                   
         B     RCV10                                                            
*                                                                               
INVERSN  MVC   VERSION,0(R4)       VERSION DATA                                 
         B     RCV10                                                            
*                                                                               
INBYR    XC    KEY,KEY             GET NWS BUYER RECORD                         
         LA    R2,KEY                                                           
         USING BYRRECD,R2                                                       
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAGMD,BAGYMD                                                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BYRKBYR(0),0(R4)                                                 
         OC    BYRKBYR,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    INBYR10                                                          
         MVC   ERROR,=Y(BADBYR)                                                 
         BRAS  RE,VCOMERR                                                       
*                                                                               
INBYR10  L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   BUYERCD,BYRKBYR                                                  
         MVC   BBUYER,BYRCODE                                                   
         MVC   BBYRMASK,BYRMASK                                                 
         MVC   BUYERNM,BYRNAME                                                  
         MVC   BUYPASS,BYRPASS                                                  
         B     RCV10                                                            
*                                                                               
INCAMP   MVC   BCAMP,0(R4)                                                      
         XC    BCAMP,=X'FFFF'      BCAMP IS XFF COMPLEMENT                      
         B     RCV10                                                            
*                                                                               
*================================================================*              
* TABLE IS:                                                      *              
* 2 BYTES LENGTH, 2 BYTES KEY, 4 BYTES NULLS (ERROR CD), DATA    *              
*================================================================*              
INWORK   L     RF,DBUFFP                                                        
         ST    RF,DBUFFS           SAVE A(START OF ENTRY)                       
*                                                                               
* RE=4(R5): +1 BCTR R5, +2 L'ENTRY, -3 PACKED KEY & DELIM, +4 ERRCD             
         LA    RE,4(R5)                                                         
         STCM  RE,3,0(RF)          LENGTH IS FIRST 2 BYTES OF TABLE             
*                                                                               
         PACK  DUB,0(4,R4)         KEY IS 4 CHAR NUMERIC                        
         CVB   R0,DUB                                                           
         STCM  R0,3,2(RF)          IN TABLE AS 2 BYTE BINARY                    
*                                                                               
         AHI   R5,-5               DEC LENGTH FOR KEY & DELIMETER               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),5(R4)                                                    
*                                                                               
         AR    RF,RE               RF=A(NXT ENTRY)                              
         C     RF,DBUFFX           MAKE SURE NOT PAST EOT                       
         BL    *+6                                                              
         DCHO                                                                   
         MVI   0(RF),X'FF'         SET EOT MARKER FOR WALT                      
         ST    RF,DBUFFP                                                        
         B     RCV10                                                            
*                                                                               
INCMT    L     RF,DBUFFP                                                        
         MVI   0(RF),C'|'          SET BUFFER DELIMETER                         
         LTR   R5,R5               DON'T MOVE IF NOTHING SENT!                  
         BM    INCMT10              BUT STILL UPDATE LENGTH & DBUFFP            
         EX    R5,*+8              MOVE IN COMMENT                              
         B     *+10                                                             
         MVC   1(0,RF),0(R4)                                                    
*                                                                               
INCMT10  LA    RF,2(RF,R5)         SET A(NEXT ENTRY) (R5 BCTR +1 DELIM)         
         C     RF,DBUFFX           MAKE SURE NOT PAST EOT                       
         BL    *+6                                                              
         DCHO                                                                   
         MVI   0(RF),X'FF'         SET EOT MARKER FOR WALT                      
         ST    RF,DBUFFP                                                        
*                                                                               
         L     RF,DBUFFS           UPDATE TABLE ENTRY LENGTH                    
         ICM   RE,3,0(RF)                                                       
         LA    RE,2(RE,R5)         +1 FOR BCTR, +1 DELIM                        
         STCM  RE,3,0(RF)                                                       
         B     RCV10                                                            
*                                                                               
* CMPEDDT IS THE START OF THE B'CAST MON THAT WAS PASSED AS THE                 
* CAMPAIGN ENDING DATE BY TAM/PC.  BROWSE FUNCTION WILL NOT SEND                
* ANY CAMPAIGN WHOOSE END DATE IS PRIOR TO CMPEDDT.                             
INCMPDT  MVC   FULL(3),0(R4)                                                    
         GOTO1 VDATCON,DMCB,(3,FULL),(0,DUB)                                    
         GOTO1 VGETBROD,DMCB,(1,DUB),WORK,VGETDAY,VADDAY                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         GOTO1 VDATCON,DMCB,(0,WORK),(3,CMPEDDT)                                
         B     RCV10                                                            
*                                                                               
INPASS   XC    BUYPASS,BUYPASS                                                  
         LTR   R5,R5                                                            
         BM    RCV10                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUYPASS(0),0(R4)                                                 
         B     RCV10                                                            
*                                                                               
INMED    MVC   QMED,0(R4)                                                       
         GOTO1 VALIMED                                                          
         B     RCV10                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                 *              
* ON ENTRY, CALLER MUST HAVE RC = A(WORK)                        *              
*================================================================*              
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         SRL   RF,24                                                            
         L     RF,VBRANCH(RF)                                                   
         AR    RF,RB                                                            
         BASR  RE,RF               *** NO RETURN EXPECTED HERE ***              
         DC    H'0'                                                             
VCOMMONX XIT1                                                                   
*                                                                               
VCOMERR  DS    0H                                                               
*                                                                               
VMSG     LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         OC    ERROR,ERROR         FAMSGNO CAN BE SET BY FALINK                 
         BZ    *+10                                                             
         MVC   FAMSGNO,ERROR                                                    
         CLI   *,FF                SET CC LOW                                   
         L     RD,BASERD           MONITOR TO SPMAK00                           
         L     RD,8(RD)            SPMAK00 TO FALINK                            
         L     RD,8(RD)            FALINK TO SPMAK00                            
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 1                                                                
VBRANCH  DS    0A                                                               
         DC    A(DMREAD-VCOMMON)                                                
         DC    A(DMSEQ-VCOMMON)                                                 
         DC    A(DMHIGH-VCOMMON)                                                
         DC    A(DMADD-VCOMMON)                                                 
         DC    A(DMWRITE-VCOMMON)                                               
         DC    A(DMGETREC-VCOMMON)                                              
         DC    A(DMPUTREC-VCOMMON)                                              
         DC    A(DMADDREC-VCOMMON)                                              
         DC    A(DMRDSTA-VCOMMON)                                               
         DC    A(DMHISTA-VCOMMON)                                               
         DC    A(VMED-VCOMMON)                                                  
         DC    A(VCLT-VCOMMON)                                                  
         DC    A(VGETHDR-VCOMMON)                                               
         DC    A(VGETDATA-VCOMMON)                                              
         DC    A(VMSG-VCOMMON)     ERROR MESSAGES USE THIS                      
         DC    22A(0)              SPARE                                        
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* DIRECTORY AND FILE ROUTINES                                       *           
*===================================================================*           
         SPACE 1                                                                
DMREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
*                                                                               
DMSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
DMHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
*                                                                               
DMADD    MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
*                                                                               
DMWRITE  MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   DIRECTRY,=C'SPTDIR'                                              
         CLI   XSP,C'Y'                                                         
         BNE   *+10                                                             
         MVC   DIRECTRY,=C'XSPDIR'                                              
         CLI   XSP,C'T'                                                         
         BNE   *+10                                                             
         MVC   DIRECTRY,=C'TRFDIR'                                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),DIRECTRY,KEYSAVE,       X        
               KEY,0                                                            
         B     DMCHECK                                                          
*                                                                               
DMGETREC MVC   COMMAND,=C'GETREC'                                               
         B     DMFILE                                                           
*                                                                               
DMPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     DMFILE                                                           
*                                                                               
DMADDREC MVC   COMMAND,=C'ADDREC'                                               
         B     DMFILE                                                           
*                                                                               
DMFILE   CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         LA    R0,KEY+14                                                        
         MVC   FILE(8),=CL8'SPTFILE'                                            
         CLI   XSP,C'T'                                                         
         BNE   *+10                                                             
         MVC   FILE(8),=CL8'TRFFILE'                                            
         CLI   XSP,C'Y'                                                         
         BNE   *+14                                                             
         LA    R0,KEY+36                                                        
         MVC   FILE(8),=CL8'XSPFILE'                                            
*                                                                               
DMFILEGO GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),FILE,(R0),              X        
               AIO,(0,DMWORK)                                                   
         B     VCOMMONX                                                         
*                                                                               
DMRDSTA  MVC   COMMAND,=C'DMREAD'                                               
         B     DMSTA                                                            
*                                                                               
DMHISTA  MVC   COMMAND,=C'DMRDHI'                                               
         B     DMSTA                                                            
*                                                                               
DMSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AIO              
         SPACE 1                                                                
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   DMBYTE,DMCB+8                                                    
         NC    DMBYTE,DMOUTBTS                                                  
         B     VCOMMONX                                                         
         EJECT                                                                  
*================================================================*              
* RETURN ADDRESS OF MAP HEADER ELEMENT IN R1                     *              
*================================================================*              
         SPACE 1                                                                
VGETHDR  LA    RE,FAMAP                                                         
         USING MHELD,RE                                                         
         SR    RF,RF                                                            
*                                                                               
GHDR2    CLM   R1,3,MHCODE         MATCH EL                                     
         BE    GHDRX                                                            
         ICM   RF,3,MHDISP                                                      
         AR    RE,RF                                                            
         CLI   MHLEN,0                                                          
         BNE   GHDR2                                                            
         DC    H'0'                                                             
*                                                                               
GHDRX    ST    RE,HDRADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* RETURN DATA ITEM ADDRESS FOR HEADER  IN HDRADDR                *              
* R1 CONTAINS DATA ITEM NUMBER                                   *              
*================================================================*              
         SPACE 1                                                                
VGETDATA ICM   RE,15,HDRADDR                                                    
         BNZ   *+6                                                              
         DC    H'0'                TAKING NO PRISONERS                          
         USING MDELD,RE                                                         
         SR    RF,RF                                                            
         IC    RF,MHLEN-MHELD(RE)  DSPL TO FIRST DATA ITEM                      
         AR    RE,RF                                                            
*                                                                               
GDAT2    CLM   R1,3,MDCODE         MATCH EL                                     
         BE    GDATX                                                            
         ICM   RF,1,MDLEN                                                       
         AR    RE,RF                                                            
         CLI   MDLEN,0                                                          
         BNE   GDAT2                                                            
         DC    H'0'                                                             
*                                                                               
GDATX    ST    RE,DATADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* VALIDATE MEDIA CODE                                            *              
*================================================================*              
         SPACE 1                                                                
VMED     XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,QAGY                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
*                                                                               
         CLI   QMED,C'N'           NETWORK TV?                                  
         BNE   VMED1                NO                                          
         CLI   SVAPROF+7,C'C'      BETTER BE CANADA...                          
         BE    VMED1                                                            
         MVC   ERROR,=Y(BADMED)                                                 
         BRAS  RE,VCOMERR                                                       
*                                                                               
VMED1    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VMED4                                                            
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                                                                   
VMED4    CLC   2(1,R6),QMED        MATCH MEDIA CODE                             
         BNE   VMED2                                                            
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   SVMEDNM,4(R6)       MEDIA NAME                                   
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*================================================================*              
* VALIDATE CLIENT                                                *              
*   PASS QCLT                                                    *              
*   RETURNS CLT REC IN AIO2, SVCPROF AND SVCXTRA                 *              
*================================================================*              
         SPACE 1                                                                
VCLT     MVC   ERROR,=Y(BADCLT)                                                 
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         B     VCOMMONX                                                         
*                                                                               
*&&DO                                                                           
         MVC   ERROR,=Y(SECLOCK)                                                
         OC    TWAACCS(2),TWAACCS  TEST ANY SECURITY LIMIT                      
         BZ    VCOMMONX                                                         
         CLI   TWAACCS,C'*'        TEST OFFICE LOCKOUT                          
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'+'        TEST MKT LOCKOUT                             
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BE    CLT20               YES                                          
*                                                                               
CLT10    CLC   TWAACCS(2),BCLT                                                  
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
*                                                                               
CLT20    CLI   TWAACCS,C'$'                                                     
         BE    CLT30                                                            
         CLI   TWAACCS,C'*'                                                     
         BNE   VCOMMONX                                                         
         LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
CLT25    CLC   TWAACCS+1(1),0(R1)                                               
         BE    VCOMMONX                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CLT25                                                         
         BRAS  RE,VCOMERR                                                       
*                                                                               
CLT30    CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BNE   VCOMMONX                                                         
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0                                                          
         BE    VCOMMONX                                                         
         BRAS  RE,VCOMERR                                                       
*&&                                                                             
         DROP  R6                                                               
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* LOADED PHASE LIST                                              *              
*================================================================*              
         SPACE 1                                                                
PHASES   DS    0X                                                               
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QCLUNPK)                                                     
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
*====================================================================*          
* FALINK MAP TABLE                                                              
*====================================================================*          
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**FAMAP*'         EYE CATCHER                                  
FAMAP    DS    0D                                                               
         SPACE 1                                                                
*====================================================================*          
* 02 - BROWSE FOR CAMPAIGN REQUEST                                   *          
*====================================================================*          
         SPACE 1                                                                
H02      DC    AL1(H02X-H02)       HEADER LENGTH                                
         DC    AL2(H02Q)           HEADER CODE                                  
         DC    AL2(H02XX-H02)      DISP TO NEXT HEADER                          
H02X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(H02_01Q),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(H02_02Q),CL5'BUYER',AL1(MDTCHQ),AL1(3)               
         DC    AL4(INBYR)                                                       
         DC    AL1(10),AL2(H02_03Q),CL5'CMPNO',AL1(MDTBIQ),AL1(2)               
         DC    AL1(10),AL2(H02_04Q),CL5'CLT  ',AL1(MDTCHQ),AL1(3)               
         DC    AL1(10),AL2(H02_05Q),CL5'CMPNM',AL1(MDTCHQ),AL1(20)              
         DC    AL1(10),AL2(H02_06Q),CL5'PRD  ',AL1(MDTCHQ),AL1(3)               
         DC    AL1(10),AL2(H02_07Q),CL5'EST  ',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H02_08Q),CL5'STDT ',AL1(MDTBDQ),AL1(3)               
         DC    AL1(14),AL2(H02_09Q),CL5'EDDT ',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INCMPDT)                                                     
         DC    AL1(10),AL2(H02_0AQ),CL5'LNGTH',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H02_0BQ),CL5'BYRNM',AL1(MDTCHQ),AL1(20)              
         DC    AL1(10),AL2(H02_0CQ),CL5'WKST ',AL1(MDTCDQ),AL1(2)               
         DC    AL1(10),AL2(H02_0DQ),CL5'WKED ',AL1(MDTCDQ),AL1(2)               
         DC    AL1(10),AL2(H02_0EQ),CL5'UPGDF',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H02_0FQ),CL5'PASS ',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H02_10Q),CL5'CANAD',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H02_11Q),CL5'BUFSZ',AL1(MDTBIQ),AL1(4)               
         DC    AL1(10),AL2(H02_12Q),CL5'DAILY',AL1(MDTCHQ),AL1(1)               
         DC    X'00'                                                            
H02XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 04 - CAMPAIGN RECORD                                               *          
*====================================================================*          
         SPACE 1                                                                
H04      DC    AL1(H04X-H04)       HEADER LENGTH                                
         DC    AL2(H04Q)           HEADER CODE                                  
         DC    AL2(H04XX-H04)      DISP TO NEXT HEADER                          
H04X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(H04_01Q),CL5'MEDIA',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(H04_02Q),CL5'BUYER',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INBYR)                                                       
         DC    AL1(14),AL2(H04_03Q),CL5'CMPNO',AL1(MDTBIQ),AL1(2)               
         DC    AL4(INCAMP)                                                      
         DC    X'00'                                                            
H04XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 06 - ESTIMATE DATA                                                 *          
*====================================================================*          
         SPACE 1                                                                
H06      DC    AL1(H06X-H06)       HEADER LENGTH                                
         DC    AL2(H06Q)           HEADER CODE                                  
         DC    AL2(H06XX-H06)      DISP TO NEXT HEADER                          
H06X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(H06_01Q),CL5'DCAT ',AL1(MDTCHQ),AL1(6)               
         DC    AL1(10),AL2(H06_02Q),CL5'MENUN',AL1(MDTCHQ),AL1(1)               
         DC    AL1(10),AL2(H06_03Q),CL5'MENU ',AL1(MDTCHQ),AL1(6)               
         DC    AL1(10),AL2(H06_04Q),CL5'ESTST',AL1(MDTCDQ),AL1(2)               
         DC    AL1(10),AL2(H06_05Q),CL5'ESTED',AL1(MDTCDQ),AL1(2)               
         DC    AL1(10),AL2(H06_06Q),CL5'OOWST',AL1(MDTBIQ),AL1(1)               
         DC    X'00'                                                            
H06XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 08 - WORK RECORD                                                   *          
*====================================================================*          
         SPACE 1                                                                
H08      DC    AL1(H08X-H08)       HEADER LENGTH                                
         DC    AL2(H08Q)           HEADER CODE                                  
         DC    AL2(H08XX-H08)      DISP TO NEXT HEADER                          
H08X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(H08_01Q),CL5'PASS ',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INPASS)                                                      
         DC    AL1(14),AL2(H08_02Q),CL5'WORK ',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INWORK)                                                      
         DC    AL1(14),AL2(H08_03Q),CL5'CMT  ',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INCMT)                                                       
         DC    X'00'                                                            
H08XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 09 - STATUS RECORD                                                 *          
*====================================================================*          
         SPACE 1                                                                
H09      DC    AL1(H09X-H09)       HEADER LENGTH                                
         DC    AL2(H09Q)           HEADER CODE                                  
         DC    AL2(H09XX-H09)      DISP TO NEXT HEADER                          
H09X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(H09_01Q),CL5'KEY  ',AL1(MDTBIQ),AL1(2)               
         DC    AL1(10),AL2(H09_02Q),CL5'ERRCD',AL1(MDTBIQ),AL1(2)               
         DC    AL1(10),AL2(H09_03Q),CL5'ERFLD',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(H09_04Q),CL5'ERRTX',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H09XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FD - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFD      DC    AL1(HFDX-HFD)       HEADER LENGTH                                
         DC    AL2(HFDQ)           HEADER CODE                                  
         DC    AL2(HFDXX-HFD)      DISP TO NEXT HEADER                          
HFDX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(HFD_01Q),CL5'VERSN',AL1(MDTHXQ),AL1(4)               
         DC    AL4(INVERFD)                                                     
         DC    X'00'                                                            
HFDXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FE - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFE      DC    AL1(HFEX-HFE)       HEADER LENGTH                                
         DC    AL2(HFEQ)           HEADER CODE                                  
         DC    AL2(HFEXX-HFE)      DISP TO NEXT HEADER                          
HFEX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(HFE_01Q),CL5'VERSN',AL1(MDTHXQ),AL1(4)               
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFEXX    EQU   *                                                                
*                                                                               
         DC    X'00'               EOFT                                         
         EJECT                                                                  
       ++INCLUDE SPTAMWRK                                                       
       ++INCLUDE SPSYSFAC                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPNWSBYR                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPTAM00   11/08/06'                                      
         END                                                                    
