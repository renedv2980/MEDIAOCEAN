*          DATA SET SPSDE00    AT LEVEL 023 AS OF 08/26/13                      
*PHASE T23100B                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE BINSRCH2                                                               
T23100   TITLE 'SPSDE00 - SPOT SUPERDESK - BASE'                                
T23100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T23100,RR=R2,CLEAR=YES                               
         USING WORKD,RC                                                         
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INITIALIZATION CODE                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
         AHI   RE,LENIO                                                         
         ST    RE,AIO5                                                          
         ST    RE,ACLTREC                                                       
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
*                                                                               
         LA    RE,TSARBLK                                                       
         USING TSARD,RE                                                         
         MVC   TSACOM,ACOMFACS                                                  
         DROP  RE                                                               
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
*                                                                               
         L     RE,=V(BINSRCH)                                                   
         A     RE,BASERELO                                                      
         ST    RE,VBINSRCH                                                      
*                                                                               
         LH    RF,=Y(SECBLK-TWAD)       SAVE A(SECRET BLOCK)                    
         AR    RF,RA                                                            
         ST    RF,ASECBLK                                                       
*                                                                               
         OC    TWAD+4(2),TWAD+4         TEST ON NEW SECURITY                    
         BNZ   *+14                                                             
         OC    TWAD+6(2),TWAD+6         OR HAVE LIMIT ACCESS                    
         BZ    INIT00                                                           
*                                                                               
         L     RF,CSECRET                                                       
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R1                                                               
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
INIT00   L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
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
         AHI   RE,VCOMMON-T23100                                                
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
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAY)    GET TODAY'S DATE                 
*                                                                               
         LA    R4,=C'S0SD'         AGENCY  SUPERDESK  PROFILE                   
         O     R4,=X'80000000'     SET FLAG1 FOR GETIT                          
         LA    R5,SVSDPRF                                                       
         BRAS  RE,GETIT                                                         
         EJECT                                                                  
INIT20   DS    0H                                                               
         CLI   SVRCVEL+1,X'06'           DON'T CLEAR IF 06                      
         BE    *+10                      BECAUSE IT COULD BE A RESUME           
         XC    SVRCVEL,SVRCVEL           FROM OVERFLOW BREAK                    
*                                                                               
         BAS   RE,INIFALNK               INITIALIZE FALINK BLOCK                
         GOTO1 VFALINK,DMCB,FABLK        GIVE FALINK CONTROL                    
         CLI   ERRORFLG,C'A'             NEED TO UNDO DMGR WRITES?              
         BNE   EXIT                                                             
         MVI   ERRORFLG,0                                                       
         DC    H'0',C'$ABEND'                                                   
*                                                                               
EXIT     DS    0H                        JUST EXIT                              
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INITIALIZE FALINK                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
         XC    SVREASON,SVREASON                                                
*                                                                               
INI2     MVC   SVRESUME,SVXFROV    SAVE THE GLOBBING OVERLAY NUMBER             
         MVI   SVXFROV,0           AND CLEAR THIS FLAG1 NOW !                   
*                                                                               
         OI    SDESERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    SDESERVH+6,X'80'                                                 
*                                                                               
                                                                                
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,SDEINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
*                                                                               
         L     R1,ACOMFACS             A(SWITCH)                                
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
         L     R0,=A(OVRFLO)           A(FALINK BUFFR OVERFLOW ROUTINE)         
         A     R0,BASERELO                                                      
         ST    R0,FALAFUL                                                       
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
*                                                                               
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-TWAD) A(FALINK SAVED STORAGE)                     
         ST    R1,FALASVE                                                       
*                                                                               
         L     R0,=A(FAMAP)            A(MAP TABLE)                             
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
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* LITERALS AND CONSTANTS                                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BREAK    NTR1  BASE=*,LABEL=*                                                   
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON RETURN FROM AN OVERLAY THAT WANTS TO EXIT VIA GLOBBER,                     
* WE RETURNED TO FALINK WITH AN FAGLB, FAGLB CALL.                              
* WHEN CALLED PROGRAM RETURNS TO US, THIS EXIT IS CALLED.                       
* ON EXIT FROM HERE, AND SVRESUME IS THE OVERLAY TO BE CALLED.                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
         CLC   =C'SPOMIS',G.GLVXFRSY  FROM SPOT MIS ?                           
         BE    RSM10                                                            
         CLC   =C'SPOFIS',G.GLVXFRSY  FROM SPOT FIS ?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  G                                                                
*                                                                               
         GOTO1 (RF),(R1),=C'GETD',SVPCNPD,6,GLVBUY1    %PAID                    
         GOTO1 (RF),(R1),=C'CLEAR'   MAKE SURE NOTHING IN GLOBBER               
*&&DO                                                                           
*****    GOTO1 (RF),DMCB,=C'PUTD',MISMSG,60,GLVSMSG                             
*                                                                               
* MAKE SURE DELETE ALL GLOBBER ELEMS ON ERROR                                   
         GOTO1 (RF),(R1),=C'CLEAR'   MAKE SURE NOTHING IN GLOBBER               
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGNO,BLOCK       MESSAGE NUMBER                               
         MVC   FAMSGTYP,BLOCK+2    TYPE                                         
         MVC   FAMSGSYS,BLOCK+3    SYSTEM                                       
         CLI   *,FF                SET CC LOW                                   
         B     RSMX                                                             
*&&                                                                             
*                                                                               
RSM10    CR    RB,RB               EXIT WITH CC =                               
*                                                                               
RSMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
OVRFLO   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,FABLK                                                         
         USING FALINKD,R2                                                       
*                                                                               
         CLI   MAHMOUNS,X'00'      DOES FALINK WANT TO BREAK?                   
         BNE   OVRFLO10                                                         
         OI    BRKFLAG,BRKFULL                                                  
         B     OVRFLOX                                                          
*                                                                               
OVRFLO10 CLI   MAHMOUNS,X'FF'      DOES FALINK WANT TO RESUME?                  
         BNE   OVRFLOX                                                          
         NI    BRKFLAG,X'FF'-BRKFULL                                            
         OI    BRKFLAG,BRKRESM                                                  
*                                                                               
OVRFLOX  CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SEND     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
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
         TM    BRKFLAG,BRKFULL                                                  
         BO    SENDX                                                            
*                                                                               
         MVC   SVRESUME,SVXFROV    SAVE THIS OVERLAY NUMBER                     
         CLI   SVXFROV,0           TEST OVLY REQUESTED GLOBBER CALL             
         BE    SEND30                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB                             
         B     SENDX                                                            
*                                                                               
SEND30   CLI   ANYDATA,C'Y'        ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
SEND40   GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
*                                                                               
SENDX    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND OVERLAY LOOKUP TABLE                                                     
* ENTRIES ARE                                                                   
*        DS    AL2(FIRST RCV EL CODE)                                           
*        DS    XL1(SEND OVERLAY NUMBER)                                         
*        DS    XL1(SPARE)                                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SOVTAB   DS    0AL4                                                             
         DC    XL2'01',X'01',X'00' INITIAL MOMENT                               
         DC    XL2'02',X'02',X'00' AUTHORIZATION TRANSACTION                    
         DC    XL2'03',X'01',X'00' ACTIVE AUTH MARKET DOWNLOAD                  
         DC    XL2'04',X'02',X'00' UPDATE STATUS                                
         DC    XL2'05',X'02',X'00' UPDATE STATION DETAILS                       
         DC    XL2'06',X'02',X'00' AUTHORIZATION DETAIL                         
         DC    XL2'07',X'01',X'00' AUTHORIZATION MARKET DETAILS                 
         DC    XL2'08',X'02',X'00' REVISION HISTORY                             
         DC    XL2'0A',X'01',X'00' APPROVAL HISTORY                             
         DC    XL2'0B',X'02',X'00' AUTH COMMENTS AND ATTACHMENTS                
         DC    XL2'0C',X'FF',X'00' AUTHORIZATION KEY                            
         DC    XL2'0F',X'01',X'00' AGENCY COMPLETE MARKET DOWNLOAD              
         DC    XL2'20',X'10',X'00' MIS SCREENS                                  
         DC    XL2'21',X'10',X'00' INVOICE INFORMATION SCREEN                   
         DC    XL2'22',X'10',X'00' % PAID (GLOBBER TO FIS)                      
         DC    XL2'23',X'01',X'00' CLIENT OFFICE                                
         DC    XL2'24',X'01',X'00' CLIENT SDUDEFS                               
* FD & FE CURRENTLY GET IGNORED                                                 
         DC    XL2'FD',X'FF',X'00' VERSION CODES                                
         DC    XL2'FE',X'FF',X'00' VERSION CODES                                
SOVTABX  EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CONTROL RECEIVED HERE WHEN FALINK HAS RECEIVED DATA                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESS                  
*                                                                               
RCV10    GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVX2               FALINK ERROR                                 
*****    BL    RCVERR              FALINK ERROR                                 
         BH    RCVX                END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV20                                                            
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PRCHDR - PROCESS HEADER ELEMENT                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
*                                                                               
         CLC   MHCODE,=X'00FE'     IGNORE FE/FD ELEMS                           
         BE    RCV10                                                            
         CLC   MHCODE,=X'00FD'                                                  
         BE    RCV10                                                            
         MVC   SVRCVEL,MHCODE      SAVE FIRST RECEIVE ELEMENT                   
*                                                                               
         B     RCV10                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PROCESS DATA FIELD                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
RCV20    L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(DATA ENTRY)                             
*                                                                               
         CLI   ERRORFLG,C'Y'       ERROR, NO NEED TO PROCESS THE DATA           
         BE    RCV10                                                            
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
RCVX2    XIT1                                                                   
*                                                                               
RCVERR   DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* DATA RECEIVE ROUTINES                                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
*                                                                               
INUSRCD  MVC   SVUSRCD,SPACES                                                   
         MVC   SVUSRCD(0),0(R4)                                                 
         EX    R5,*-6                                                           
         B     RCV10                                                            
*                                                                               
INBYGRP  MVC   SVBYGRP(0),0(R4)                                                 
         EX    R5,*-6                                                           
         OC    SVBYGRP,SPACES                                                   
         B     RCV10                                                            
*                                                                               
INUSRTP  MVC   SVUSRTP,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INDUEDT  MVC   SVDUEDT,0(R4)                                                    
         OI    SVFLAG1,F1DUEDT                                                  
         B     RCV10                                                            
*                                                                               
INSTADT  MVC   SVSTADT,0(R4)                                                    
         MVC   ERROR,=Y(BYFLTEST)                                               
         CLC   SVSTADT,SVESSTR           BUY FLIGHT START DATE MUST BE          
         BNL   RCV10                     >= ESTIMATE START DATE                 
         MVI   ERRORFLD,MCSTADT                                                 
         B     VCOMERR                                                          
*                                                                               
INENDDT  MVC   SVENDDT,0(R4)                                                    
         MVC   ERROR,=Y(BYFLTEST)                                               
         CLC   SVENDDT,SVESEND           BUY FLIGHT END DATE MUST BE            
         BH    VCOMERR                   <= ESTIMATE END DATE                   
         B     RCV10                                                            
*                                                                               
INISSDT  MVC   SVISSDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INDNLAY  OI    FLAG2,DNLAYOUT            DONE LAYOUT                            
         B     *+8                                                              
INTRANT  NI    FLAG2,X'FF'-DNLAYOUT                                             
         LA    R0,AUREC                  CLEAR AUTH DETAIL RECORD AREA          
         LA    R1,AURECLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
INDATYP  MVC   SVTRANT,0(R4)             TRANSACTION TYPE                       
         B     RCV10                                                            
*                                                                               
INSTDUE  MVC   SVSTDUE,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INBYBAS  MVC   SVBYBAS,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INCANMK  CLI   0(R4),C'1'                CANCEL MARKETS? 1=Y                    
         BNE   RCV10                                                            
         OI    SVFLAG1,F1CANMK                                                  
         B     RCV10                                                            
*                                                                               
INDELMK  CLI   0(R4),C'1'                DELETE MARKETS? 1=Y                    
         BNE   RCV10                                                            
         OI    SVFLAG1,F1DELMK                                                  
         B     RCV10                                                            
*                                                                               
INPPDDT  MVC   SVPPDDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INTPPDT  MVC   SVTPPDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INAFDDT  MVC   SVAPDDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INATHCM  LA    RE,SVATHCM                                                       
         ZICM  R0,SVCOMLEN,2                                                    
         AR    RE,R0               ADVANCE RE TO END OF STORAGE                 
*                                                                               
         AHI   R0,1                ADD 1 TO LENGTH                              
         AR    R0,R5               ADD LENGTH OF COMMENT TO TOTAL               
         STCM  R0,3,SVCOMLEN       LENGTH OF ALL COMMENTS                       
         CH    R0,=H'2000'                                                      
         BNH   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   0(0,RE),0(R4)       STORE COMMENT IN SVATHCM AFTER               
         EX    R5,*-6              OTHER COMMENTS                               
         B     RCV10                                                            
*                                                                               
INATACH  LA    R2,SVATACH                ADDRESS OF FIRST ATTACHMENT            
         MVC   ERROR,=Y(TABLERR)                                                
*                                                                               
INAT10   CLI   0(R2),0                   FIND AVAILABLE SPACE                   
         BE    INAT20                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     INAT10                                                           
*                                                                               
INAT20   LR    RE,R5                     LENGTH OF FILE NAME - 1                
         AHI   RE,2                      ADD 2 BYTES FOR CORRECT LEN            
         LR    R0,RE                                                            
         AR    R0,R2                     ADD ADDR OF NEXT AVAIL BYTE            
         LA    R3,ATACHEND               END OF ATTACHMENT SPACE                
         CR    R0,R3                     CHECK IF ENOUGH ROOM                   
         BNL   VCOMERR                   NOT ENOUGH ROOM                        
*                                                                               
         STC   RE,0(R2)                  STORE LENGTH OF FILENAME               
         MVC   1(0,R2),0(R4)                                                    
         EX    R5,*-6                    STORE ATTACHMENT FILENAME              
         B     RCV10                                                            
*                                                                               
*                                                                               
INUDEF   LA    R2,SVUDEFS                ADDRESS OF FIRST UDEF                  
         MVC   ERROR,=Y(TABLERR)                                                
*                                                                               
INUD10   CLI   0(R2),0                   FIND AVAILABLE SPACE                   
         BE    INUD20                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     INUD10                                                           
*                                                                               
INUD20   LR    RE,R5                     LENGTH OF UDEF INFO - 1                
         AHI   RE,2                      ADD 2 BYTES FOR CORRECT LEN            
         LR    R0,RE                                                            
         AR    R0,R2                     ADD ADDR OF NEXT AVAIL BYTE            
         LA    R3,UDEFSEND               END OF UDEF SPACE                      
         CR    R0,R3                     CHECK IF ENOUGH ROOM                   
         BNL   VCOMERR                   NOT ENOUGH ROOM                        
*                                                                               
         STC   RE,0(R2)                  STORE LENGTH OF UDEF INFO              
         MVC   1(0,R2),0(R4)                                                    
         EX    R5,*-6                    STORE UDEF INFO                        
         B     RCV10                                                            
*                                                                               
INMGPOL  MVC   SVMGPOL,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INMGWKS  MVC   SVMGWKS,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INAPPRQ  CLI   0(R4),C'1'                APPROVAL REQUIRED? 1=Y                 
         BNE   RCV10                                                            
         OI    SVFLAG1,F1APPRQ                                                  
         B     RCV10                                                            
*                                                                               
INAPSTA  MVC   SVAPSTA,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INAPCOM  MVC   SVATHCM(0),0(R4)                                                 
         EX    R5,*-6                                                           
         STCM  R5,3,SVCOMLEN                                                    
         B     RCV10                                                            
*                                                                               
INORSDT  MVC   SVORSDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INORCNF  MVC   SVORCNF,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INCNCDT  MVC   SVCNCDT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INAPREV  CLI   0(R4),C'1'                APPLY REVISION? 1=Y                    
         BNE   RCV10                                                            
         OI    SVFLAG1,F1APREV                                                  
         B     RCV10                                                            
*                                                                               
INMKTFLT B     RCV10               *** NO LONGER USING THIS ***                 
*                                                                               
INCLTLST B     RCV10               *** NO LONGER USING THIS ***                 
*                                                                               
INMED    MVC   QMED,0(R4)                                                       
         OI    QMED,X'40'                                                       
         GOTO1 VALIMED                                                          
         BRAS  RE,GETPROF                GET AGY LEVEL PROFILES                 
*                                                                               
         NI    FLAG2,X'FF'-ALLMKTS-MKTSNT-MKTDUES-DIFFDUES   INIT               
         L     R3,AIO4                   CLEAR TABLE FOR 02 DOWNLOAD            
         XC    LASTMKT,LASTMKT           DISPLACEMENT INTO TABLE = 0            
         MVC   0(2,R3),=X'FFFF'                                                 
         XC    ADDMKTS,ADDMKTS     TABLE FOR MKTS TO RE-ADD                     
         B     RCV10                                                            
*---------------------------------------------------------------                
INMKT    XC    BSTA,BSTA                 CLEAR STATION                          
         XC    QSTA,QSTA                                                        
         SR    R0,R0                                                            
         ICM   R0,3,0(R4)                                                       
         STCM  R0,3,BMKT                                                        
         BNZ   *+12                      FILTER ON ALL MARKETS?                 
         OI    FLAG2,ALLMKTS             FLAG1 FOR ALL MKTS SELECTED            
         B     RCV10                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
*                                                                               
         CLI   SVRCVEL+1,X'07'           IF RECEIVING 07, CLEAR AUREC           
         BNE   *+10                                                             
         XC    AUREC(AMRECLNQ),AUREC                                            
         CLI   SVRCVEL+1,X'02'           IF RECEIVING 02, DON'T MARK            
         BNE   RCV10                     TABLE                                  
*                                                                               
         L     R3,AIO4                   ADDRESS OF MARKET TABLE                
         L     R2,LASTMKT                DISPLACEMENT OF LAST MARKET            
         AR    R3,R2                     ADD TO ADDRESS OF TABLE                
         AHI   R2,2                      ADD 2 BYTES FOR NEW MARKET             
         ST    R2,LASTMKT                                                       
         MVC   0(2,R3),BMKT              ADD NEW MARKET TO TABLE                
         MVC   2(2,R3),=X'FFFF'          MARK END OF TABLE                      
         OI    FLAG2,MKTSNT                                                     
         B     RCV10                                                            
*---------------------------------------------------------------                
INADDMKT DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,0(R4)                                                       
         BZ    RCV10               NOTHING THERE                                
*                                                                               
         LA    R3,ADDMKTS          TABLE OF 20 MKTS TO READD                    
         LA    R2,20                                                            
INAMKT10 OC    0(2,R3),0(R3)       FIND EMPTY SPOT                              
         BZ    INAMKT20            ADD 2 BYTES FOR NEW MARKET                   
         LA    R3,2(R3)                                                         
         BCT   R2,INAMKT10                                                      
         DC    H'0'                RE-ADDING MORE THAN 20 MKTS                  
INAMKT20 STCM  R0,3,0(R3)          ADD NEW MARKET TO TABLE                      
         B     RCV10                                                            
*---------------------------------------------------------------                
INMKTDD  TM    FLAG2,MKTSNT              MKTS W/DUE DATES SENT                  
         BNO   *+6                                                              
         DC    H'0'                                                             
         XC    BSTA,BSTA                 CLEAR STATION                          
         XC    QSTA,QSTA                                                        
*                                                                               
         SR    R1,R1               MKT|DUE DATE IN CHAR                         
         LA    R2,0(R4)                                                         
IMD05    CLI   0(R2),C'|'          GET LEN OF MKT TO PACK                       
         BE    IMD10                                                            
         AHI   R1,1                                                             
         LA    R2,1(R2)                                                         
         B     IMD05                                                            
*                                                                               
IMD10    BCTR  R1,0                PACK MKT INTO BMKT                           
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     IMD15                                                            
         PACK  DUB,0(0,R4)   * EXECUTED *                                       
IMD15    STCM  R0,3,BMKT                                                        
*                                                                               
         XC    WORK,WORK                                                        
         AHI   R1,2                1 FOR EX AND 1 FOR |                         
         LA    R3,0(R4)            POINT TO MKT                                 
         AR    R3,R1               NOW R3 SHOULD POINT TO DATE                  
         LR    R2,R5               TOTAL LEN - 1                                
         SR    R2,R1               SUBTRACT MKT AND | FOR DATE LEN              
         MVC   WORK(0),0(R3)                                                    
         EX    R2,*-6                                                           
         GOTO1 VDATVAL,DMCB,WORK,WORK+20                                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,DMCB,(0,WORK+20),(3,WORK)                                
*                                                                               
         CLI   SVRCVEL+1,X'02'           IF NOT RECEIVING 02, NO MARK           
         BNE   RCV10                     TABLE                                  
         L     R3,AIO4                   ADDRESS OF MARKET TABLE                
         L     R2,LASTMKT                DISPLACEMENT OF LAST MARKET            
         AR    R3,R2                     ADD TO ADDRESS OF TABLE                
         AHI   R2,5                      ADD 5 BYTES FOR NEW MKT/DATE           
         ST    R2,LASTMKT                                                       
         MVC   0(2,R3),BMKT              ADD NEW MARKET TO TABLE                
         MVC   2(3,R3),WORK              AND DUE DATE                           
         MVC   5(2,R3),=X'FFFF'          MARK END OF TABLE                      
*                                                                               
         OI    FLAG2,MKTDUES             MKTS W/DUE DATES SENT                  
         NI    FLAG2,X'FF'-ALLMKTS       TURN OFF ALL MKTS                      
*                                                                               
         TM    SVFLAG1,F1DUEDT           WAS A AUTH LEVEL DUE DT SENT           
         BO    RCV10                     YES THEN KEEP                          
         OC    SVDUEDT,SVDUEDT                                                  
         BZ    INMD20                                                           
         CLC   SVDUEDT,WORK                                                     
         BE    *+8                                                              
         OI    FLAG2,DIFFDUES            SENT DIFFERENT DUE DATES               
         CLC   SVDUEDT,WORK              SAVE EARLIEST DUE DATE                 
         BL    *+10                                                             
INMD20   MVC   SVDUEDT,WORK                                                     
         B     RCV10                                                            
*---------------------------------------------------------------                
INRDUE   MVC   SVRPDUE,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INRPSNT  MVC   SVRPSNT,0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INCLT    MVC   QCLT,0(R4)                                                       
         OC    QCLT,SPACES                                                      
         XC    BCLT,BCLT                                                        
         GOTO1 VALICLT                                                          
         L     R6,AIO                    FIGURE OUT BUYING TYPE *               
         USING CLTHDRD,R6                                                       
         CLI   CPOLONLY,C'N'                                                    
         BE    *+12                                                             
         CLI   CPOLONLY,X'00'                                                   
         BNE   INCLT10                                                          
*                                        R3 -> BRAND POL T/A                    
         CLI   CPROF,C'0'                BRD?                                   
         BE    *+12                                                             
         MVI   SVPOOL,C'B'                                                      
         B     INCLT30                                                          
*                                                                               
         MVI   SVPOOL,C'N'                                                      
         B     INCLT30                                                          
*                                        R3 -> BRAND POL T/A                    
INCLT10  CLI   CPROF,C'0'                TPOL?                                  
         BNE   INCLT20                   NO - BPOL                              
         MVI   SVPOOL,C'T'                                                      
         B     INCLT30                                                          
*                                                                               
INCLT20  MVI   SVPOOL,C'B'                                                      
*                                                                               
INCLT30  BRAS  RE,GETPROF                                                       
         B     RCV10                                                            
         DROP  R6                                                               
*                                                                               
INPRD    DS    0H                                                               
         MVC   ERROR,=Y(INVPRD)                                                 
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         XC    BLOCK,BLOCK                                                      
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(0),0(R4)                                                    
         EX    R5,*-6                                                           
         GOTO1 VSCANNER,DMCB,(C'C',WORK),(X'82',BLOCK),C',=-='                  
         CLI   DMCB+4,0                                                         
         BE    VCOMERR                                                          
*                                                                               
         LA    R3,BLOCK                                                         
         CLI   0(R3),0                                                          
         BE    VCOMERR                                                          
         CLI   1(R3),0                                                          
         BNE   VCOMERR                                                          
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QPRD(0),12(R3)            PRODUCT 1                              
         OC    QPRD,SPACES                                                      
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         CLI   SVRCVEL+1,X'20'           IF MIS REPORT DON'T CARE               
         BE    INPRD20                                                          
         CLI   BPRD,X'FF'                POL PRODUCT?                           
         BNE   INPRD10                   YES                                    
         MVC   ERROR,=Y(PRDCTPOL)        PRD CAN'T BE POL                       
         CLI   SVPOOL,C'T'               CLIENT MUST BE TRUE POOL               
         BNE   VCOMERR                   NO, BPRD CANNOT =X'FF'                 
         B     INPRD20                                                          
*                                                                               
INPRD10  MVC   ERROR,=Y(PRDPOL)          PRD MUST BE POL                        
         CLI   SVPOOL,C'T'               IF CLIENT IS TRUE POOL                 
         BE    VCOMERR                   PRD MUST BE POL                        
*                                                                               
INPRD20  MVC   ERROR,=Y(INVPRD)                                                 
         LA    R3,32(R3)                 A(PIGGYBACK PRODUCT)                   
         CLI   0(R3),0                                                          
         BE    INPRDX                                                           
*                                                                               
         CLI   1(R3),0                   PRODUCT CAN'T HAVE SUB-FIELDS          
         BNE   VCOMERR                                                          
*                                                                               
         CLI   BPRD,X'FF'                NO PIGGYBACK IF POL ORDER              
         BE    VCOMERR                                                          
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QPRD2(0),12(R3)                                                  
         OC    QPRD2,SPACES                                                     
*                                                                               
         GOTO1 VALIPR2                                                          
*                                                                               
         CLI   BPRD2,X'FF'               POL PRODUCT?                           
         BE    VCOMERR                   YES, SHOULD NOT BE ACCEPTED            
*                                                                               
         CLC   QPRD,QPRD2                PUT PRDS IN ALPHABETICAL ORDER         
         BL    INPRDX                                                           
         MVC   BYTE,BPRD                                                        
         MVC   BPRD,BPRD2                                                       
         MVC   BPRD2,BYTE                                                       
INPRDX   B     RCV10                                                            
*                                                                               
INEST    DS    0H                                                               
         MVC   BEST,0(R4)                1 BYTE BINARY EST                      
*                                                                               
         CLI   SVPOOL,C'B'               BRAND POL CLIENT?                      
         BNE   INEST05                                                          
         MVC   FULL(3),QPRD              YES, MAKE SURE POL EST OPEN            
         MVC   QPRD,=C'POL'                                                     
         OI    FLAG1,PRD1                                                       
         GOTO1 VALIEST                                                          
         MVC   QPRD,FULL                                                        
*                                                                               
INEST05  OI    FLAG1,PRD1                VALIDATE FIRST PRODUCT                 
         GOTO1 VALIEST                                                          
*                                                                               
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
*                                                                               
         OC    QPRD2,QPRD2                                                      
         BZ    INEST10                                                          
         OI    FLAG1,PRD2                VALIDATE PIGGYBACK                     
         GOTO1 VALIEST                                                          
INEST10  B     RCV10                                                            
*                                                                               
INREVNM  MVI   SVREVNM,X'99'             IF ADD OR NEW, SVREV=99                
         CLI   SVTRANT,ADAUTH                                                   
         BE    RCV10                                                            
         CLI   SVTRANT,NEWVERS                                                  
         BE    RCV10                                                            
         ZAP   WORK(2),=P'99'            FINDING 9'S COMPLIMENT                 
         SP    WORK(2),0(2,R4)           99 - (REV #) = 9'S COMP REV #          
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVREVNM,WORK                                                     
         B     RCV10                                                            
*                                                                               
INVERNM  ZAP   WORK(2),=P'99'            FINDING 9'S COMPLIMENT                 
         SP    WORK(2),0(2,R4)           99 - (VER #) = 9'S COMP VER #          
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVVERNM,WORK                                                     
*                                                                               
INVER10  MVI   XSP,C'Y'                  BUILD AUTHORIZATION KEY HERE           
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   AUTKTYP(2),=X'0D39'                                              
         MVC   AUTKAM,BAGYMD                                                    
         MVC   AUTKCLT,BCLT                                                     
         MVC   AUTKPRD,BPRD                                                     
         MVC   AUTKPRD2,BPRD2                                                   
         MVC   AUTKEST,BEST                                                     
         MVC   AUTKAUN,SVVERNM                                                  
         MVC   SVAUKEY,KEY                                                      
         B     RCV10                                                            
         DROP  R6                                                               
*                                                                               
INVRSN   B     RCV10                                                            
*                                                                               
INSTA    MVC   QSTA(0),0(R4)                                                    
         EX    R5,*-6                                                           
         OC    QSTA,SPACES                                                      
         GOTO1 VALISTA                                                          
*                                                                               
         LA    R0,AUREC                  CLEAR FOR STATION COMMENTS             
         LA    R1,AURECLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     RCV10                                                            
*                                                                               
INFMT    MVC   SVFMT,0(R4)         FORMAT FOR MIS SCREENS                       
         B     RCV10                                                            
*                                                                               
INFSTDT  MVC   SVSTADT,0(R4)       FILTER START DATE                            
         MVI   BRKFLAG,0                                                        
         B     RCV10                                                            
*                                                                               
INFENDT  MVC   SVENDDT,0(R4)       FILTER END DATE                              
         B     RCV10                                                            
*                                                                               
INVERSN  MVC   VERSION,0(R4)       VERSION DATA                                 
         B     RCV10                                                            
*                                                                               
INDUMP   MVC   BLOCK,0(R4)         SAVE DEBUGGING INFO                          
         DC    H'0'                AND DUMP!                                    
         B     RCV10                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
* ON ENTRY, CALLER MUST HAVE RC = A(WORK)                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
VCOMMONX SR    RC,RC                                                            
NTEQUAL  LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
VCOMERR  DS    0H                                                               
*                                                                               
VMSG     LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         OC    ERROR,ERROR         FAMSGNO CAN BE SET BY FALINK                 
         BZ    *+10                                                             
         MVC   FAMSGNO,ERROR                                                    
         CLI   *,FF                SET CC LOW                                   
         L     RD,BASERD           MONITOR TO SPSDE00                           
         L     RD,8(RD)            SPSDE00 TO FALINK                            
         L     RD,8(RD)            FALINK TO SPSDE00                            
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
         DC    A(VSPV-VCOMMON)                                                  
         DC    A(VBYR-VCOMMON)                                                  
         DC    A(VMED-VCOMMON)                                                  
         DC    A(VCLT-VCOMMON)                                                  
         DC    A(VPRD-VCOMMON)                                                  
         DC    A(VEST-VCOMMON)                                                  
         DC    A(VSTA-VCOMMON)                                                  
         DC    A(VPR2-VCOMMON)                                                  
         DC    A(VGETHDR-VCOMMON)                                               
         DC    A(VGETDATA-VCOMMON)                                              
         DC    A(VMSG-VCOMMON)     ERROR MESSAGES USE THIS                      
         DC    A(DMSEQSTA-VCOMMON)                                              
         DC    15A(0)              SPARE                                        
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* DIRECTORY AND FILE ROUTINES                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
         LA    R4,KEY+14                                                        
         MVC   FILE(8),=CL8'SPTFILE'                                            
         CLI   XSP,C'Y'                                                         
         BNE   *+14                                                             
         LA    R4,KEY+36                                                        
         MVC   FILE(8),=CL8'XSPFILE'                                            
*                                                                               
DMFILEGO GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),FILE,(R4),              X        
               AIO,(0,DMWORK)                                                   
         MVC   DMDSKADD,0(R4)                                                   
         B     DMCHECK                                                          
*                                                                               
DMSEQSTA MVC   COMMAND,=C'DMRSEQ'                                               
         B     DMSTA                                                            
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
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RETURN ADDRESS OF MAP HEADER ELEMENT IN R1                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RETURN DATA ITEM ADDRESS FOR HEADER  IN HDRADDR                               
* R1 CONTAINS DATA ITEM NUMBER                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
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
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE MEDIA CODE                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VMED     XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,QAGY                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVI   XSP,C'N'                                                         
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
         MVC   ERROR,=Y(INVMED)                                                 
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VMED20                                                           
         SPACE 1                                                                
VMED10   BAS   RE,NEXTEL                                                        
         BNE   VCOMERR                                                          
VMED20   CLC   2(1,R6),QMED        MATCH MEDIA CODE                             
         BNE   VMED10                                                           
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   SVMEDNM,4(R6)       MEDIA NAME                                   
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                     VALIDATE SUPERVISOR                                       
* ON ENTRY: SVUSER(6) = BUYING GROUP(2) + SUPERVISOR CODE(4)                    
*           NAMEONLY BIT TURNED ON IF I ONLY NEED TO GET THE USERNAME           
* ON EXIT:  SVUSRNM CONATINS THE USERNAME (LAST NAME, FIRST INITIAL)            
*           SVSPIDNO CONATINS THE PERSON ID NUMBER                              
*           SUPERVISOR RECORD IN IO3                                            
*           CONDITION CODE SET TO NE IF SUPERVISOR CODE DOESN'T EXIST           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VSPV     DS    0H                                                               
         XC    SVSPIDNO,SVSPIDNO         IN CASE THERE ISN'T ONE                
         XC    SVUSRNM,SVUSRNM           IN CASE THERE ISN'T ONE                
         MVI   XSP,C'N'                                                         
         USING SPVRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,BAGYMD                                                   
         NI    SPVKAGY,X'F0'             DROP MEDIA CODE                        
         MVC   SPVKOFC(6),SVUSER         BUYING GROUP + USER CODE               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SPVKEY(9),KEYSAVE                                                
         BNE   NTEQUAL                                                          
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,SPVEL                                                         
         USING SPVNAMED,R6                                                      
         MVC   SVUSRNM(14),SPVLNAME      GET LAST NAME,FIRST INITIAL            
         LA    R1,SVUSRNM+L'SPVLNAME-1                                          
         LA    R5,L'SPVLNAME                                                    
VSPV10   CLI   0(R1),C' '                                                       
         BH    VSPV20                                                           
         BCTR  R1,0                                                             
         BCT   R5,VSPV10                                                        
VSPV20   MVI   1(R1),C','                                                       
         MVC   3(1,R1),SPVFNAME                                                 
*                                                                               
         USING SPIDELD,R6                                                       
         MVI   ELCODE,SPIDELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   VCOMMONX                                                         
         MVC   SVSPIDNO,SPIDNO                                                  
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                     VALIDATE BUYER                                            
* ON ENTRY: SVUSER(6) = BUYING GROUP(2) + BUYER CODE(4)                         
*           NAMEONLY BIT TURNED ON IF I ONLY NEED TO GET THE USERNAME           
* ON EXIT:  SVUSRNM CONTAINS THE USERNAME (LAST NAME, FIRST INITIAL)            
*   *OPT*   SVBPIDNO CONTAINS THE PERSON ID NUMBER                              
*   *OPT*   SVSPVCD CONTAINS THE BUYER'S SUPERVISOR CODE                        
*   *OPT*   SVBYRFLT CONTAINS THE FILTER FOR CLT CODES IN SPV RECORD            
*           BUYER RECORD IN IO2                                                 
*           CONDITION CODE SET TO NE IF BUYER CODE DOESN'T EXIST                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VBYR     DS    0H                                                               
         XC    SVBPIDNO,SVBPIDNO         IN CASE THERE ISN'T ONE                
         XC    SVUSRNM,SVUSRNM           IN CASE THERE ISN'T ONE                
         MVI   XSP,C'N'                                                         
         USING BYRRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,BAGYMD                                                   
         NI    BYRKAGY,X'F0'             DROP MEDIA CODE                        
         MVC   BYRKOFC(6),SVUSER         BUYING GROUP + USER CODE               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   BYRKEY(9),KEYSAVE                                                
         BNE   NTEQUAL                   SEND ERROR                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         LA    R6,BYREL                                                         
         USING BYRNAMED,R6                                                      
*                                                                               
* SVUSRNAME = "LASTNAME, FIRST INITIAL"                                         
         MVC   SVUSRNM(L'BYRLNAME),BYRLNAME                                     
         LA    R1,SVUSRNM+L'BYRLNAME-1                                          
         LA    R5,L'BYRLNAME                                                    
VBYR10   CLI   0(R1),C' '                                                       
         BH    VBYR20                                                           
         BCTR  R1,0                                                             
         BCT   R5,VBYR10                                                        
VBYR20   MVI   1(R1),C','                                                       
         MVC   3(1,R1),BYRFNAME                                                 
*                                                                               
         TM    FLAG1,NAMEONLY                                                   
         BO    VCOMMONX                                                         
         MVC   SVSPVCD,BYRSPV            SUPERVISOR'S CODE                      
         MVC   SVBYRFLT,BYRFILT          BUYER FILTER (FOR CLIENT CODE)         
         USING BPIDELD,R6                                                       
         MVI   ELCODE,BPIDELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   VCOMMONX                  NONE - CHECK SUPV                      
         MVC   SVBPIDNO,BPIDNO                                                  
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE CLIENT                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VCLT     DS    0H                                                               
         MVC   ERROR,=Y(INVCLT)                                                 
*                                                                               
         OC    BCLT,BCLT                 ALREADY HAVE BCLT?                     
         BNZ   CLT05                     YES                                    
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BNE   VCOMERR                                                          
*                                                                               
CLT05    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVI   XSP,C'N'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOMERR                                                          
*                                                                               
         L     R6,ACLTREC                                                       
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVCNAME,CNAME       AND CLIENT NAME                              
*                                                                               
         MVC   ERROR,=Y(SECLOCK)                                                
         OC    TWAACCS(2),TWAACCS  TEST ANY SECURITY LIMIT                      
         BZ    VCOMMONX                                                         
*                                                                               
CLT30    XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'            SYSTEM ID = SPOT                          
         MVC   OFCAUTH,TWAACCS        TWA+6(2) (BINARY CLIENT)                  
         MVC   OFCAGY,QAGY            ALPHA AGENCY                              
         MVC   OFCOFC,COFFICE         OFFICE CODE TO BE VALIDATED               
*                                                                               
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),OFCCLT                               
*                                                                               
*** CODE HAS BEEN NOOPED SINCE CLIENT GROUP LIMIT ACCESS ONLY WORKS IF          
*** YOU PASS THE 3-CHARACTER CLIENT CODE                                        
*                                                                               
*        OI    OFCINDS,OFCI2CSC       PASSING 2-BYTE CLIENT CODE                
*        MVC   OFCCLT2,BCLT           2-BYTE CLIENT CODE (QCLT NOT SET)         
*                                                                               
*** CODE HAS BEEN NOOPED SINCE CLIENT GROUP LIMIT ACCESS ONLY WORKS IF          
*** YOU PASS THE 3-CHARACTER CLIENT CODE   -HWON 8/22/2013                      
*                                                                               
         MVC   OFCSAGMD,BAGYMD        SPOT AGY/MED BYTE                         
         MVC   OFCLMT(4),TWAACCS      LIMIT ACCESS VALUE TWA+6(4)               
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVI   OFCACCSM,X'FF'         NO ACCESS LIST FROM MKTREC                
         MVC   OFCSECD,ASECBLK        A(SECRET BLOCK)                           
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',(R4)),ACOMFACS                               
         CLI   0(R1),0                                                          
         JE    VCOMMONX                                                         
*                                                                               
SECERR   CLI   SVRCVEL+1,X'0C'     IF THIS IS ADD/EDIT AUTHORIZATION            
         BE    VCOMERR              THEN SECURITY ERROR                         
         B     NTEQUAL             OTHERWISE JUST SEND NE RETURN CODE           
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE PRODUCT CODE                                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                                                                               
VPR2     OI    FLAG1,PRD2          INDICATE PRD2                                
         B     VPRD2                                                            
*                                                                               
VPRD     DS    0H                                                               
         MVC   ERROR,=Y(INVPRD)                                                 
         OI    FLAG1,PRD1          INDICATE PRD1                                
*                                                                               
VPRD2    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         TM    FLAG1,PRD1                                                       
         BO    *+10                                                             
         MVC   KEY+4(3),QPRD2                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVI   XSP,C'N'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOMERR                                                          
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         TM    FLAG1,PRD2                                                       
         BO    VPRD10                                                           
         MVC   BPRD,PCODE+1                                                     
         MVC   SVPNAME,PNAME                                                    
         B     VPRDX                                                            
*                                                                               
VPRD10   MVC   BPRD2,PCODE+1                                                    
         MVC   SVPNAME2,PNAME                                                   
VPRDX    MVI   FLAG1,0                                                          
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE BINARY ESTIMATE NUMBER IN BEST(1)                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VEST     DS    0H                                                               
         MVC   ERROR,=Y(EST1_255)                                               
         ZIC   R1,BEST                                                          
         CHI   R1,255                                                           
         BH    VCOMERR                                                          
*                                                                               
         MVC   ERROR,=Y(INVEST)          INVALID EST                            
         CLC   QPRD,=C'POL'                                                     
         BNE   *+10                                                             
         MVC   ERROR,=Y(NOPOLEST)        NO POL ESTIMATE OPEN                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         TM    FLAG1,PRD1                                                       
         BO    *+10                                                             
         MVC   EKEYPRD,QPRD2                                                    
         MVC   EKEYEST,BEST                                                     
         MVI   XSP,C'N'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VEST05                                                           
         TM    FLAG1,SDEBIT              JUST TURNING ON SDESK FLAG1?           
         BO    VESTX                     YES, THEN IGNORE ERROR                 
         B     VCOMERR                                                          
*                                                                               
VEST05   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         TM    FLAG1,SDEBIT                                                     
         BNO   VEST10                                                           
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
VEST10   GOTO1 GETREC                                                           
         TM    FLAG1,SDEBIT                                                     
         BNO   VEST20                                                           
         OI    EFLAG1,EF1SDE                                                    
         GOTO1 PUTREC                                                           
         B     VESTX                                                            
*                                                                               
VEST20   MVC   WORK(12),ESTART     SAVE ESTART AND EEND DATES                   
         GOTO1 VDATCON,DMCB,(0,WORK),(3,SVESSTR)                                
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,SVESEND)                              
         MVC   SVEDESC,EDESC       SAVE ESTIMATE DESCRIPTION                    
         MVC   SVESODT,ECRDATE     ESTIMATE CREATION DATE                       
         MVC   SVEDEMOS(9),EDEMOS  SAVE FIRST DEMO                              
         MVC   SVEUSRNM,EUSRNMS    SAVE USER DEMO NAME                          
         MVC   SVEWGTNM,EWGTNM     SAVE DEMO WEIGHT NAME                        
         MVC   SVEDAYMN,EDAYMENU   DAYPART MENU NUMBER                          
VESTX    MVI   FLAG1,0                                                          
         B     VCOMMONX                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* LOOK UP MARKET NUMBER AND GET 3 BYTE PACKED STA                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VSTA     DS    0H                                                               
         MVC   ERROR,=Y(INVSTA)                                                 
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         CLI   KEY+6,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),QAGY                                                    
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         USING STARECD,R6                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 READSTA                                                          
*                                                                               
         CLI   8(R1),0                                                          
         BNE   VCOMERR                                                          
*                                                                               
         CLI   SVRCVEL+1,X'05'           CAN COMPLETE STATION THAT              
         BE    VSTA10                           IS NOT IN IT'S HOME MKT         
         MVC   ERROR,=Y(STANTMKT)                                               
         CLC   SMKT,QMKT                 STATION IN MARKET SELECTED?            
         BNE   VCOMERR                                                          
*                                                                               
VSTA10   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET,QSTA+5                                                  
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BNE   VCOMERR                                                          
         MVC   BSTA,STAPSTA                                                     
         EJECT                                                                  
* READ MARKET RECORD TO IO2                                                     
         SPACE 1                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),QAGY                                                    
*                                                                               
         MVC   ERROR,=Y(INVMKT)                                                 
         GOTO1 HIGHSTA                                                          
*                                                                               
         CLC   KEY(8),0(R6)        TEST MARKET FOUND                            
         BE    VSTA30                                                           
         CLI   QMED,C'C'           ERROR EXCEPT FOR MEDIA=C, MKT=0000           
         BNE   VCOMERR                                                          
         CLC   QMKT,=C'0000'                                                    
         BNE   VCOMERR                                                          
         B     VCOMMONX                                                         
*                                                                               
VSTA30   CLI   TWAACCS,C'+'        TEST MARKET LOCKOUT                          
         BE    VSTA35                                                           
         CLI   TWAACCS+2,C'+'      TEST MARKET LOCKOUT                          
         BNE   VCOMMONX                                                         
*                                                                               
*        LA    R0,3                                                             
*        LA    R1,MKTLTACC                                                      
*        CLC   TWAACCS+1(1),0(R1)                                               
*        BE    VCOMMONX                                                         
*        LA    R1,1(R1)                                                         
*        BCT   R0,*-14                                                          
*                                                                               
VSTA35   XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'            SYSTEM ID = SPOT                          
         MVC   OFCAUTH,TWAACCS        TWA+6(2) (BINARY CLIENT)                  
         MVC   OFCLMT(4),TWAACCS      LIMIT ACCESS VALUE TWA+6(4)               
         MVC   OFCAGY,QAGY            ALPHA AGENCY                              
         MVC   OFCSAGMD,BAGYMD        SPOT AGY/MED BYTE                         
         MVC   OFCACCSM,MKTLTACC      ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK        A(SECRET BLOCK)                           
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',(R4)),ACOMFACS                               
         CLI   0(R1),0                                                          
         JE    VCOMMONX                                                         
*                                                                               
         MVC   ERROR,=Y(SECLOCK)                                                
         B     VCOMERR                                                          
*                                                                               
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* GETPROF - READ REQUIRED PROFILES *                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
GETPROF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,=C'S0A0'         READ A0 PROFILE                              
         LA    R5,SVA0PRF                                                       
         BRAS  RE,GETIT                                                         
         SPACE 1                                                                
         LA    R4,=C'S0A0'         READ AGY LEVEL A0 PROFILE                    
         O     R4,=X'80000000'     SET FLAG1 FOR GETIT                          
         LA    R5,WORK2                                                         
         BRAS  RE,GETIT                                                         
         MVC   SVA0PRF(1),WORK2   SET GROSS/NET FIELD ONLY                      
*                                                                               
         MVC   FULL,=C'SA0A'                                                    
         NI    FULL,X'BF'          CHANGE 'S' TO LOWER CASE                     
         LA    R4,FULL                                                          
         LA    R5,SVA0APRF                                                      
         BRAS  RE,GETIT                                                         
*                                                                               
         LA    R4,=C'S0MK'                 MATCHMAKER PROFILE                   
         LA    R5,SVMKPRF                                                       
         BRAS  RE,GETIT                                                         
*                                                                               
         LA    R4,=C'S0SD'         AGENCY  SUPERDESK  PROFILE                   
         O     R4,=X'80000000'     SET FLAG1 FOR GETIT                          
         LA    R5,SVSDPRF                                                       
         BRAS  RE,GETIT                                                         
*                                                                               
         LA    R4,=C'S000'                                                      
         LA    R5,SV00PRF                                                       
         BRAS  RE,GETIT                                                         
* CONVERT PROFILE DATES TO Y2K FORMAT                                           
         OC    SVA0APRF+5(2),SVA0APRF+5 SPECIAL OFFICE START                    
         BZ    GETPRF10                                                         
         MVC   DUB(2),SVA0APRF+5                                                
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK    MAKE EBCDIC                         
         GOTO1 (RF),(R1),WORK,(3,DUB)       MAKE BINARY                         
         MVC   SVA0APRF+5(2),DUB                                                
*                                                                               
GETPRF10 OC    SVA0APRF+7(3),SVA0APRF+7 MAKEGOOD AS MISSED                      
         BZ    GETPRFX                                                          
         MVC   DUB(3),SVA0APRF+7                                                
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
         GOTO1 (RF),(R1),WORK,(3,SVA0APRF+7)                                    
*                                                                               
GETPRFX  BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN EXPECTED HERE                      
         XIT1                                                                   
*                                                                               
GETIT    NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(R4)                                                    
         MVC   WORK+4(2),QAGY                                                   
         LTR   R4,R4               IF NEGATIVE, STOP AT AGY                     
         BM    GETIT2                                                           
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
GETIT2   XC    0(16,R5),0(R5)                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),(R5),VDATAMGR                             
         XIT1                                                                   
         EJECT                                                                  
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QTSAR)                                                       
PHASESN  EQU   *-PHASES                                                         
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FALINK MAP TABLE                                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0D                                                               
         DC    C'**FAMAP*'         EYE CATCHER                                  
FAMAP    DS    0D                                                               
                                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 01 - INITIAL MOMENT                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H01      DC   AL1(H01X-H01)           HEADER LENGTH                             
         DC   XL2'0001'               HEADER CODE                               
         DC   AL2(H01XX-H01)          DISP TO NEXT HEADER                       
H01X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCUSRCD),CL5'USERC',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INUSRCD)                                                     
         DC    AL1(14),AL2(MCOFFCD),CL5'OFFCD',AL1(MDTCHQ),AL1(2)               
         DC    AL4(INBYGRP)                                                     
         DC    AL1(14),AL2(MCUSRTP),CL5'USRTP',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INUSRTP)                                                     
*                                                                               
         DC    AL1(10),AL2(MCUSRNM),CL5'USRNM',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCAGYMD),CL5'AGYMD',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL1(10),AL2(MCDLMKT),CL5'DLMKT',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCDLCLT),CL5'DLCLT',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCDLCLTO),CL5'DLCLO',AL1(MDTCHQ),AL1(0)              
         DC    AL1(10),AL2(MCBYRCD),CL5'BYRCD',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCFLTDY),CL5'FLTDY',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(MCSPLAN),CL5'SPLAN',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(MCSDPRF),CL5'SDPRF',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCUDEFS),CL5'UDEFS',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCAGNCY),CL5'AGNCY',AL1(MDTCHQ),AL1(2)               
         DC    AL1(10),AL2(MCWEBDAV),CL5'WEBDV',AL1(MDTCHQ),AL1(0)              
         DC    X'00'                                                            
H01XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 02 - AUTHORIZATION TRANSACTION                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
H02      DC   AL1(H02X-H02)           HEADER LENGTH                             
         DC   XL2'0002'               HEADER CODE                               
         DC   AL2(H02XX-H02)          DISP TO NEXT HEADER                       
H02X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCTRANT),CL5'TRANT',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INTRANT)                                                     
         DC    AL1(14),AL2(MCDNLAY),CL5'DNLAY',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INTRANT)                                                     
         DC    AL1(14),AL2(MCREVNM),CL5'REVNM',AL1(MDTPKQ),AL1(2)               
         DC    AL4(INREVNM)                                                     
         DC    AL1(14),AL2(MCDUEDT),CL5'DUEDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INDUEDT)                                                     
         DC    AL1(14),AL2(MCSTADT),CL5'STADT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INSTADT)                                                     
         DC    AL1(14),AL2(MCENDDT),CL5'ENDDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INENDDT)                                                     
         DC    AL1(14),AL2(MCISSDT),CL5'ISSDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INISSDT)                                                     
         DC    AL1(14),AL2(MCBYBAS),CL5'BYBAS',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INBYBAS)                                                     
         DC    AL1(14),AL2(MCCANMK),CL5'CANMK',AL1(MDTCHQ),AL1(3)               
         DC    AL4(INCANMK)                                                     
         DC    AL1(14),AL2(MCDELMK),CL5'DELMK',AL1(MDTCHQ),AL1(3)               
         DC    AL4(INDELMK)                                                     
         DC    AL1(14),AL2(MCPPDDT),CL5'PPSDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INPPDDT)                                                     
         DC    AL1(14),AL2(MCTPPDT),CL5'TMPDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INTPPDT)                                                     
         DC    AL1(14),AL2(MCAPDDT),CL5'AFFDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INAFDDT)                                                     
         DC    AL1(14),AL2(MCMGPOL),CL5'MGPOL',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMGPOL)                                                     
         DC    AL1(14),AL2(MCMGWKS),CL5'MGWKS',AL1(MDTBIQ),AL1(1)               
         DC    AL4(INMGWKS)                                                     
         DC    AL1(14),AL2(MCAPPRQ),CL5'APPRQ',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INAPPRQ)                                                     
         DC    AL1(14),AL2(MCATACH),CL5'ATACH',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INATACH)                                                     
         DC    AL1(14),AL2(MCAPREV),CL5'APLRV',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INAPREV)                                                     
         DC    AL1(14),AL2(MCATHCM),CL5'ATHCM',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INATHCM)                                                     
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCSUVAL),CL5'SUVAL',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INUDEF)                                                      
         DC    AL1(14),AL2(MCMKTDD),CL5'MKTDD',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INMKTDD)                                                     
         DC    AL1(14),AL2(MCADDMKT),CL5'ADMKT',AL1(MDTBIQ),AL1(2)              
         DC    AL4(INADDMKT)                                                    
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCNTRES),CL5'NTRES',AL1(MDTMDQ),AL1(0)               
         DC    AL1(10),AL2(MCMKCNT),CL5'MKCNT',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(MCPCNPD),CL5'PCNPD',AL1(MDTPKQ),AL1(6)               
         DC    X'00'                                                            
H02XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 03 - ACTIVE MARKET DOWNLOAD                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H03      DC   AL1(H03X-H03)           HEADER LENGTH                             
         DC   XL2'0003'               HEADER CODE                               
         DC   AL2(H03XX-H03)          DISP TO NEXT HEADER                       
H03X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    X'00'                                                            
H03XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 04 - APPROVAL HISTORY EDIT                                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H04      DC   AL1(H04X-H04)           HEADER LENGTH                             
         DC   XL2'0004'               HEADER CODE                               
         DC   AL2(H04XX-H04)          DISP TO NEXT HEADER                       
H04X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCAPSTA),CL5'APSTA',AL1(MDTBIQ),AL1(1)               
         DC    AL4(INAPSTA)                                                     
         DC    AL1(14),AL2(MCAPCOM),CL5'APCOM',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INAPCOM)                                                     
         DC    X'00'                                                            
H04XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 05 - STATION DETAILS EDIT                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H05      DC   AL1(H05X-H05)           HEADER LENGTH                             
         DC   XL2'0005'               HEADER CODE                               
         DC   AL2(H05XX-H05)          DISP TO NEXT HEADER                       
H05X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCSTATN),CL5'STATN',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INSTA)                                                       
         DC    AL1(14),AL2(MCORSDT),CL5'ORSDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INORSDT)                                                     
         DC    AL1(14),AL2(MCORCNF),CL5'ORCNF',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INORCNF)                                                     
         DC    AL1(14),AL2(MCCNCDT),CL5'CNCDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INCNCDT)                                                     
         DC    AL1(14),AL2(MCSTACM),CL5'STACM',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INATHCM)                                                     
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H05XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 06 - AUTHORIZATION DOWNLOAD                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H06      DC   AL1(H06X-H06)           HEADER LENGTH                             
         DC   XL2'0006'               HEADER CODE                               
         DC   AL2(H06XX-H06)          DISP TO NEXT HEADER                       
H06X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCFSTDT),CL5'FSTDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INFSTDT)                                                     
         DC    AL1(14),AL2(MCFENDT),CL5'FENDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INFENDT)                                                     
         DC    AL1(14),AL2(MCTRANT),CL5'TRANT',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INTRANT)                                                     
         DC    AL1(14),AL2(MCDNLAY),CL5'DNLAY',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INDNLAY)                                                     
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKTFLT)                                                    
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLTLST)                                                    
         DC    AL1(14),AL2(MCSTDUE),CL5'STDUE',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INSTDUE)                                                     
*                                                                               
         DC    X'00'                                                            
H06XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 07 - AUTHORIZATION DOWNLOAD                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H07      DC   AL1(H07X-H07)           HEADER LENGTH                             
         DC   XL2'0007'               HEADER CODE                               
         DC   AL2(H07XX-H07)          DISP TO NEXT HEADER                       
H07X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
*                                                                               
         DC    AL1(14),AL2(MCRPDUE),CL5'RPDUE',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INRDUE)                                                      
*                                                                               
         DC    AL1(14),AL2(MCRPSNT),CL5'RPSNT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INRPSNT)                                                     
*                                                                               
         DC    X'00'                                                            
H07XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 08 - REVISION HISTORY TAB                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H08      DC   AL1(H08X-H08)           HEADER LENGTH                             
         DC   XL2'0008'               HEADER CODE                               
         DC   AL2(H08XX-H08)          DISP TO NEXT HEADER                       
H08X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCATHCM),CL5'ATHCM',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCATACH),CL5'ATACH',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCSUVAL),CL5'SUVAL',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCENDRV),CL5'ENDRV',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL1(10),AL2(MCMKTAD),CL5'MKTAD',AL1(MDTBIQ),AL1(2)               
         DC    AL1(10),AL2(MCMKTCA),CL5'MKTCA',AL1(MDTBIQ),AL1(2)               
         DC    AL1(10),AL2(MCMKTDL),CL5'MKTDL',AL1(MDTBIQ),AL1(2)               
         DC    X'00'                                                            
H08XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 0A - AUTHORIZATION APPROVAL HISTORY                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H0A      DC   AL1(H0AX-H0A)           HEADER LENGTH                             
         DC   XL2'000A'               HEADER CODE                               
         DC   AL2(H0AXX-H0A)          DISP TO NEXT HEADER                       
H0AX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(4)               
         DC    AL1(10),AL2(MCAPCOM),CL5'APCOM',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H0AXX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 0B - AUTHORIZATION COMMENTS AND ATTACHMENTS                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H0B      DC   AL1(H0BX-H0B)           HEADER LENGTH                             
         DC   XL2'000B'               HEADER CODE                               
         DC   AL2(H0BXX-H0B)          DISP TO NEXT HEADER                       
H0BX     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCATACH),CL5'ATACH',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCATHCM),CL5'ATHCM',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H0BXX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 0C - AUTHORIZATION KEY                                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H0C      DC   AL1(H0CX-H0C)           HEADER LENGTH                             
         DC   XL2'000C'               HEADER CODE                               
         DC   AL2(H0CXX-H0C)          DISP TO NEXT HEADER                       
H0CX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(MCPRD),CL5'PRD  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INPRD)                                                       
         DC    AL1(10),AL2(MCPNAME),CL5'PNAME',AL1(MDTCHQ),AL1(0)               
         DC    AL1(14),AL2(MCEST),CL5'EST  ',AL1(MDTBIQ),AL1(1)                 
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(MCVERNM),CL5'VERNM',AL1(MDTPKQ),AL1(2)               
         DC    AL4(INVERNM)                                                     
         DC    X'00'                                                            
H0CXX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 0E - ERROR RECORD                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H0E      DC   AL1(H0EX-H0E)           HEADER LENGTH                             
         DC   XL2'000E'               HEADER CODE                               
         DC   AL2(H0EXX-H0E)          DISP TO NEXT HEADER                       
H0EX     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCERRCD),CL5'ERRCD',AL1(MDTBIQ),AL1(1)               
         DC    AL1(10),AL2(MCMPCDE),CL5'MPCDE',AL1(MDTBIQ),AL1(1)               
         DC    X'00'                                                            
H0EXX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 0F - LIST OF MARKETS                                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H0F      DC   AL1(H0FX-H0F)           HEADER LENGTH                             
         DC   XL2'000F'               HEADER CODE                               
         DC   AL2(H0FXX-H0F)          DISP TO NEXT HEADER                       
H0FX     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H0FXX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 20 - MIS SCREENS                                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H20      DC   AL1(H20X-H20)           HEADER LENGTH                             
         DC   XL2'0020'               HEADER CODE                               
         DC   AL2(H20XX-H20)          DISP TO NEXT HEADER                       
H20X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(MCPRD),CL5'PRD  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INPRD)                                                       
         DC    AL1(14),AL2(MCEST),CL5'EST  ',AL1(MDTBIQ),AL1(1)                 
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCSTATN),CL5'STATN',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INSTA)                                                       
         DC    AL1(14),AL2(MCDATYP),CL5'DATYP',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INDATYP)                                                     
         DC    AL1(14),AL2(MCFMT),CL5'FMT  ',AL1(MDTCHQ),AL1(1)                 
         DC    AL4(INFMT)                                                       
         DC    AL1(14),AL2(MCFSTDT),CL5'FSTDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INFSTDT)                                                     
         DC    AL1(14),AL2(MCFENDT),CL5'FENDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INFENDT)                                                     
*                                                                               
         DC    AL1(10),AL2(MCEQVRD),CL5'EQVRD',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCDEMO),CL5'DEM',AL1(MDTCHQ),AL1(0)                  
         DC    AL1(10),AL2(MCMIS),CL5'MIS',AL1(MDTCHQ),AL1(0)                   
         DC    X'00'                                                            
H20XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 21 - INVOICE INFORMATION SCREEN                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H21      DC   AL1(H21X-H21)           HEADER LENGTH                             
         DC   XL2'0021'               HEADER CODE                               
         DC   AL2(H21XX-H21)          DISP TO NEXT HEADER                       
H21X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(MCPRD),CL5'PRD  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INPRD)                                                       
         DC    AL1(14),AL2(MCEST),CL5'EST  ',AL1(MDTBIQ),AL1(1)                 
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(MCVERNM),CL5'VERNM',AL1(MDTPKQ),AL1(2)               
         DC    AL4(INVERNM)                                                     
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCSTADT),CL5'STADT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INSTADT)                                                     
         DC    AL1(14),AL2(MCENDDT),CL5'ENDDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INENDDT)                                                     
*                                                                               
         DC    AL1(10),AL2(MCDLSTR),CL5'DLSTR',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCINVNM),CL5'INVNM',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H21XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 22 - SEND PERCENT PAID                                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H22      DC   AL1(H22X-H22)           HEADER LENGTH                             
         DC   XL2'0022'               HEADER CODE                               
         DC   AL2(H22XX-H22)          DISP TO NEXT HEADER                       
H22X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(MCPRD),CL5'PRD  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INPRD)                                                       
         DC    AL1(14),AL2(MCEST),CL5'EST  ',AL1(MDTBIQ),AL1(1)                 
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(MCMKT),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                 
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(MCSTADT),CL5'STADT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INSTADT)                                                     
         DC    AL1(14),AL2(MCENDDT),CL5'ENDDT',AL1(MDTBDQ),AL1(3)               
         DC    AL4(INENDDT)                                                     
*                                                                               
         DC    AL1(10),AL2(MCPCNPD),CL5'PCNPD',AL1(MDTCAQ),AL1(6)               
         DC    X'00'                                                            
H22XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 23 - CLIENT OFFICE                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H23      DC   AL1(H23X-H23)           HEADER LENGTH                             
         DC   XL2'0023'               HEADER CODE                               
         DC   AL2(H23XX-H23)          DISP TO NEXT HEADER                       
H23X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
*                                                                               
         DC    AL1(10),AL2(MCCLTOF),CL5'CLTOF',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H23XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 24 - CLIENT SD-UDEFS                                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H24      DC   AL1(H24X-H24)           HEADER LENGTH                             
         DC   XL2'0024'               HEADER CODE                               
         DC   AL2(H24XX-H24)          DISP TO NEXT HEADER                       
H24X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCMEDIA),CL5'MEDIA',AL1(MDTCHQ),AL1(1)               
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(MCCLT),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                 
         DC    AL4(INCLT)                                                       
*                                                                               
         DC    AL1(10),AL2(MCUDEFS),CL5'UDEFS',AL1(MDTCHQ),AL1(1)               
         DC    X'00'                                                            
H24XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 25 - MF DUMP                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H25      DC   AL1(H25X-H25)           HEADER LENGTH                             
         DC   XL2'0025'               HEADER CODE                               
         DC   AL2(H25XX-H25)          DISP TO NEXT HEADER                       
H25X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCDUMP),CL5'DUMP',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INDUMP)                                                      
*                                                                               
         DC    X'00'                                                            
H25XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FD - VERSION DATA                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
HFD      DC   AL1(HFDX-HFD)           HEADER LENGTH                             
         DC   XL2'00FD'               HEADER CODE                               
         DC   AL2(HFDXX-HFD)          DISP TO NEXT HEADER                       
HFDX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFDXX    EQU   *                                                                
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FE - VERSION DATA                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
HFE      DC   AL1(HFEX-HFE)              HEADER LENGTH                          
         DC   XL2'00FE'                  HEADER CODE                            
         DC   AL2(HFEXX-HFE)             DISP TO NEXT HEADER                    
HFEX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFEXX    EQU   *                                                                
*                                                                               
         DC    X'00'                     END OF TABLE                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPSDEWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                    480 BYTES IN OVWORK                    
LASTMKT  DS    A                         DISPLACEMENT OF LAST MARKET            
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE FASECRETD                                                      
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
**PAN#1  DC    CL21'023SPSDE00   08/26/13'                                      
         END                                                                    
