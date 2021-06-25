*          DATA SET SPSFM61    AT LEVEL 048 AS OF 02/18/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21761A                                                                  
*INCLUDE NETNET                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21761  -- BILLING HOLD FILE                         *         
*                                                                     *         
*  COMMENTS:     MAINTAINS BILLING HOLD RECS ON XSPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SCSFM56(MAINT), AND SCSFM57(LIST)            *         
*                                                                     *         
*  OUTPUTS:      CURRENT BILLING HOLD RECORDS                         *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21761 - BHOLD RECORD MAINTENANCE'                              
T21761   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1761**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,DMCB                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000A79'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPPWCALC,DMCB                                                    
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
*                   SET FILE                                      *             
*******************************************************************             
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
VK       DS    0H                                                               
*                                                                               
         MVC   BHLMNM,SPACES                                                    
         OI    BHLMNMH+6,X'80'                                                  
         MVC   BHLCNM,SPACES                                                    
         OI    BHLCNMH+6,X'80'                                                  
         MVC   BHLPNM,SPACES                                                    
         OI    BHLPNMH+6,X'80'                                                  
         MVC   BHLENM,SPACES                                                    
         OI    BHLENMH+6,X'80'                                                  
         MVC   BHLMKN,SPACES                                                    
         OI    BHLMKNH+6,X'80'                                                  
*                                                                               
         XC    BHKEY,BHKEY         START BUILDING THE KEY X'0E07'               
         LA    R4,BHKEY                                                         
         USING BHLDRECD,R4                                                      
         MVI   BHLDKSYS,BHLDKSYQ                                                
         MVI   BHLDKSTP,BHLDKSTQ                                                
*                                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         LA    R2,BHLMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   BHLMNM(L'MEDNM),MEDNM                                            
         OI    BHLMNMH+6,X'80'                                                  
         MVC   BHLDKAM,BAGYMD                                                   
*                                                                               
         LA    R2,BHLCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BNE   ERRMIS                                                           
         CLI   BHLPRDH+5,0         BUT MAKE SURE, NO PRD                        
         BNE   ERRMIS                                                           
         CLI   BHLESTH+5,0         AND NO EST THEN                              
         BNE   ERRMIS                                                           
         B     VK40                                                             
*                                                                               
VK10     MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   BHLCNM(L'CLTNM),CLTNM                                            
         OI    BHLCNMH+6,X'80'                                                  
         MVC   BHLDKCLI,BCLT                                                    
*                                                                               
VK20     LA    R2,BHLPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK25                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK30                                                             
         B     ERRMIS                                                           
*                                                                               
VK25     MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   BHLPNM,PRDNM                                                     
         OI    BHLPNMH+6,X'80'                                                  
         MVC   BHLDKPRD,BPRD                                                    
*                                                                               
VK30     LA    R2,BHLESTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK40                                                             
         B     ERRINV                                                           
*                                                                               
VK35     CLI   BHLPRDH+5,0         PRODUT ENTERED ?                             
         BNE   VK37                YES, CNA USE VALIEST                         
         TM    4(R2),X'08'         IF THIS IS A FILTER, JUST CHECK              
         BZ    ERRINV              THAT EST IS NUMERIC                          
         SR    R1,R1                                                            
         IC    R1,BHLESTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BHLEST(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,BHLDKEST                                                      
         B     VK40                                                             
VK37     MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
         L     R6,AIO2                                                          
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,BHLENM)                               
         GOTO1 DATCON,DMCB,(0,ESTART),(2,ESTARTD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,BHLENM+11)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,ESENDD)                                  
         MVI   BHLENM+9,C'-'                                                    
         OI    BHLENMH+6,X'80'                                                  
         MVC   BHLDKEST,BEST                                                    
         MVC   MYESTART,ESTART                                                  
         MVC   MYEEND,EEND                                                      
         DROP  R6                                                               
*                                                                               
VK40     LA    R2,BHLSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK45                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK50                                                             
         B     ERRMIS                                                           
*                                                                               
VK45     MVI   USEIONUM,2                                                       
         GOTO1 VALISTA                                                          
         MVC   BHLMKN,MKTNM                                                     
         OI    BHLMKNH+6,X'80'                                                  
         MVC   BHLDKMKT,BMKT                                                    
         MVC   BHLDKSTA,BSTA                                                    
*                                                                               
VK50     LA    R2,BHLMOSH          MONTH OF SERVICE DATE                        
         CLI   5(R2),0                                                          
         BNE   VK55                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK100                                                            
         B     ERRMIS                                                           
*                                                                               
VK55     DS    0H                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),CHARYMD                                    
         OC    DMCB,DMCB           IS DATE VALID?                               
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,CHARYMD),(3,BINYMD)                               
         MVC   BHLDKMOS,BINYMD                                                  
*                                                                               
VK100    BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         MVC   KEY,BHKEY                                                        
*                                                                               
*                                                                               
VKX      MVC  AIO,AIO1                                                          
         DROP  R4                                                               
         B     XIT                                                              
*                                                                               
*                                                                               
*******************************************************************             
*                      VALREC                                                   
*******************************************************************             
*                                                                               
VR       DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,HLDELCDQ     GET RID OF MAIN ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,BHLEDTH          A(EFFECTIVE DATE FIELD)                      
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ERRMIS                                                           
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         USING HLDELD,R6                                                        
         LA    R6,ELEM                                                          
         MVI   HLDELCOD,HLDELCDQ   ELEMENT CODE                                 
         MVI   HLDELLEN,HLDELLNQ   ELEMENT LENGTH                               
*&&DO                                                                           
         XC    WORK,WORK           OUTPUT BLOCK FOR PERVAL                      
         GOTO1 PERVAL,DMCB,(BHLEDTH+5,BHLEDTH),(X'40',WORK)                     
         CLI   DMCB+4,X'01'                                                     
         BNZ   ERRINV                                                           
K@       USING PERVALD,WORK                                                     
         MVC   HLDELDAT,K@.PVALCSTA                                             
         DROP  K@                                                               
*                                                                               
*&&                                                                             
         XC    CHARYMD,CHARYMD                                                  
         GOTO1 DATVAL,DMCB,(0,8(R2)),CHARYMD                                    
         OC    DMCB,DMCB           TEST VALID M/D/Y                             
         BZ    ERRINV              YES                                          
         GOTO1 DATCON,DMCB,(0,CHARYMD),(2,HLDELDAT)                             
*                                                                               
         LA    R2,BHLGRSH          GROSS DOLLARS                                
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ERRMIS                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   HLDELGRS,DMCB+4                                                  
*                                                                               
         LA    R2,BHLNETH          NET DOLLARS                                  
         CLI   5(R2),0             TEST ANY DATA                                
         BE    VR40                NO, THEN DEFAULT TO 85%                      
*                                                                               
         ZIC   RE,5(R2)            USE USER'S OVERRIDE NET                      
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   HLDELNET,DMCB+4                                                  
         B     VR50                                                             
*                                                                               
VR40     MVC   FULL,HLDELGRS       CALCULATE 85% OF GROSS                       
         SR    R0,R0                                                            
*                                                                               
         GOTO1 =V(NETNET),DMCB,((R0),FULL),DUB,RR=RELO                          
         MVC   HLDELNET,DUB+4            NET                                    
*                                                                               
VR50     GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     DR                                                               
*                                                                               
*******************************************************************             
*        DISPREC                                                                
*******************************************************************             
DR       BAS   RE,CLR                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,HLDELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING  HLDELD,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,HLDELDAT),(11,BHLEDT)                             
         OI    BHLEDTH+6,X'80'                                                  
*                                                 GROSS                         
         EDIT  HLDELGRS,BHLGRS,2,ALIGN=LEFT,FLOAT=$,COMMAS=YES                  
         OI    BHLGRSH+6,X'80'                                                  
*                                                 NET                           
         EDIT  HLDELNET,BHLNET,2,ALIGN=LEFT,FLOAT=$,COMMAS=YES                  
         OI    BHLNETH+6,X'80'                                                  
*                                                                               
         DROP R6                                                                
*                                                                               
DRX      B     XIT                                                              
*******************************************************************             
*        DISPKEY                                                                
*******************************************************************             
*                                                                               
DK       DS     0H                                                              
*                                                                               
         L      R6,AIO                                                          
         USING  BHLDRECD,R6                                                     
*                                                                               
         MVC   BYTE,BHLDKAM        ISOLATE MEDIA CODE                           
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   BHLMED,0(R5)                                                     
         OI    BHLMEDH+6,X'80'                                                  
         MVI   BHLMEDH+5,1          TRANSMIT MEDIA CODE TO SCREEN               
*                                                                               
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BHLDKCLI),BHLCLT                             
         OI    BHLCLTH+6,X'80'                                                  
         MVI   BHLCLTH+5,3          TRANSMIT CLIENT CODE TO SCREEN              
         CLI   BHLCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   BHLCLTH+5,2                                                      
*                                                                               
         BAS   RE,RTPRNM                                                        
         MVC   BHLPRD,TEMPPRD                                                   
         MVI   BHLPRDH+5,3                                                      
         CLI   BHLPRD+2,C' '                                                    
         BH    *+8                                                              
         MVI   BHLPRDH+5,2                                                      
*                                                                               
         EDIT  BHLDKEST,BHLEST,FILL=0                                           
         OI    BHLESTH+4,X'08'      NUMERIC CODE                                
         MVI   BHLESTH+5,3                                                      
*                                                                               
         GOTO1 MSUNPK,DMCB,BHLDKMKS,FULL,BHLSTA                                 
         OI    BHLSTAH+6,X'80'                                                  
         MVI   BHLSTAH+5,5                                                      
         CLI   BHLSTA+4,C' '                                                    
         BH    *+8                                                              
         MVI   BHLSTAH+5,4                                                      
*                                                                               
         MVC   BINYMD(L'BHLDKMOS),BHLDKMOS                                      
         MVI   BINYMD+2,1          FAKE THE DATE TO 1ST OF THE MONTH            
         GOTO1 DATCON,DMCB,(3,BINYMD),(6,BHLMOS)                                
         OI    BHLMOSH+6,X'80'                                                  
         MVI   BHLMOSH+5,6                                                      
*                                                                               
*                                                                               
DKX      B     VK                                                               
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
*        LISTRECS                                                               
*******************************************************************             
*                                                                               
LR       LA     R4,KEY                                                          
         USING  BHLDRECD,R4                                                     
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
         MVC   KEY,BHKEY                                                        
*                                                                               
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
LR20     CLC   KEY(3),BHKEY        ANY MORE BHOLD RECS FOR THAT MEDIA           
         BNE   LRX                 NO                                           
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   FILTFLAG,0                                                       
         BAS   RE,FILTERS                                                       
         TM    FILTFLAG,FLTSKIP    X'80' RECORD TO BE FILTERED OUT?             
         BO    LR15                      YES                                    
*                                                                               
         GOTO1 GETREC                                                           
         XC    LISTAR,LISTAR                                                    
*                                                                               
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BHLDKCLI),LSTCLT                             
         BAS   RE,RTPRNM           RETURN THE PRODUCT                           
         MVC   LSTPRD,TEMPPRD                                                   
         GOTO1 MSUNPK,DMCB,BHLDKMKS,HALF,LSTSTA                                 
         EDIT  BHLDKEST,(3,LSTEST),FILL=0                                       
*                                                                               
         MVC   BINYMD(L'BHLDKMOS),BHLDKMOS                                      
         MVI   BINYMD+2,1          FAKE THE DATE TO 1ST OF THE MONTH            
         GOTO1 DATCON,DMCB,(3,BINYMD),(9,LSTMOS)                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,HLDELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING HLDELD,R6                                                        
         EDIT  HLDELGRS,(16,LSTGROSS),2,ALIGN=LEFT,FLOAT=$,COMMAS=YES           
         EDIT  HLDELNET,(16,LSTNET),2,ALIGN=LEFT,FLOAT=$,COMMAS=YES             
         GOTO1 DATCON,DMCB,(2,HLDELDAT),(11,LSTEDATE)                           
         DROP  R6                                                               
*                                                                               
LR30     GOTO1 LISTMON                                                          
         MVC   KEY,SAVEKEY                                                      
         B     LR15                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*******************************************************************             
*        CHECK THE FILTERS                                        *             
*******************************************************************             
FILTERS  NTR1                                                                   
         LA    R4,BHKEY                                                         
         USING BHLDRECD,R4                  FILTER BY:                          
*                                                                               
         OC    BHLDKCLI,BHLDKCLI            CLIENT?                             
         BZ    FLT10                                                            
         CLC   BHLDKCLI,KEY+BHLDKCLI-BHLDKEY                                    
         BNE   FLT100                                                           
*                                                                               
FLT10    OC    BHLDKPRD,BHLDKPRD             PRODUCT?                           
         BZ    FLT20                                                            
         CLC   BHLDKPRD,KEY+BHLDKPRD-BHLDKEY                                    
         BNE   FLT100                                                           
*                                                                               
FLT20    OC    BHLDKEST,BHLDKEST             ESTIMATE?                          
         BZ    FLT30                                                            
         CLC   BHLDKEST,KEY+BHLDKEST-BHLDKEY                                    
         BNE   FLT100                                                           
*                                                                               
FLT30    OC    BHLDKMKS,BHLDKMKS             MARKET/STATION                     
         BZ    FLT40                                                            
         CLC   BHLDKMKS,KEY+BHLDKMKS-BHLDKEY                                    
         BNE   FLT100                                                           
*                                                                               
*                                                                               
FLT40    OC    BHLDKMOS,BHLDKMOS             MONTH OF SERVICE?                  
         BZ    FLTX                                                             
         CLC   BHLDKMOS,KEY+BHLDKMOS-BHLDKEY                                    
         BNE   FLT100                                                           
         B     FLTX                                                             
*                                                                               
FLT100   OI    FILTFLAG,FLTSKIP   TURNED ON TO FILTER THIS RECORD OUT           
FLTX     B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*******************************************************************             
*        RETRIEVE THE PRODUCT NAME                                              
*******************************************************************             
RTPRNM   NTR1                                                                   
         BAS   RE,RSV              RESET SYSTEM VALUES                          
                                                                                
         L     R4,AIO                                                           
         USING BHLDRECD,R4                                                      
*                                                                               
         MVC   AIO,AIO3            SAVE EVERYTHING                              
         MVC   SKEY,KEY                                                         
*                                                                               
         XC    KEY,KEY             SET UP CLIENT RECORD KEY                     
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   CKEYAM,BHLDKAM                                                   
         MVC   CKEYCLT,BHLDKCLI                                                 
*                                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              PULL PROD NAME FROM LIST                     
         LA    R2,CLIST                                                         
*                                                                               
RTPR10   CLC   BHLDKPRD,3(R2)      MATCH PRD CODE WITH MNEUMONIC                
         BE    RTPR20                                                           
         LA    R2,4(R2)                                                         
         B     RTPR10                                                           
*                                                                               
RTPR20   MVC   TEMPPRD,0(R2)       MNEUMONIC GOES IN TEMPPRD                    
RTPRX    MVC   AIO,AIO1            RESTORE AIO,KEY,SAVEKEY AND FILES            
         BAS   RE,SSV              SET SYSTEM VALUES                            
         MVC   KEY,SKEY                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC              RESTORE POINTER TO PROPER DA                 
         B     XIT                                                              
         DROP  R4,R6                                                            
*                                                                               
         EJECT                                                                  
*******************************************************************             
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              CLEAR SCREEN                                                     
***********************************************************************         
CLR     NTR1                                                                    
        LA     R2,BHLEDTH                                                       
CLR10   CLI    0(R2),0             END OF SCREEN ?                              
        BE     CLRX                DONE                                         
        TM     1(R2),X'20'         DO NOT CLEAR PROTECTED FIELDS                
        BO     CLR50                                                            
        ZIC    RE,0(R2)                                                         
        SH     RE,=H'8'                                                         
        TM     1(R2),X'02'         EXTENDED HEADER ?                            
        BZ     *+8                 YES, SUBTRACT EXTENSION LEN                  
        SH     RE,=H'8'                                                         
        BCTR   RE,0                                                             
        EX     RE,*+8                                                           
        B      *+10                                                             
        XC     8(0,R2),8(R2)                                                    
        OI     6(R2),X'80'                                                      
CLR50   ZIC    RE,0(R2)                                                         
        AR     R2,RE                                                            
        B      CLR10                                                            
CLRX    XIT1                                                                    
***********************************************************************         
*              NEXT FIELD HEADER                                                
***********************************************************************         
NXTFLD   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
**************************************************************                  
*  GET CPROF+6 (AAN) FOR CLUNPK                                                 
**************************************************************                  
GETAAN   NTR1                                                                   
*                                                                               
         MVI   CLTAAN,C'N'         JUST IN CASE WE DONT FIND CLT REC            
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         MVC   AIO,AIO3            AIO3 IS FREE                                 
*                                                                               
         L     R3,AIO1             UCOMM RECORD                                 
         USING BHLDRECD,R3                                                      
         MVC   KEY+1(1),BHLDKAM    AGENCY/MEDIA                                 
         MVC   KEY+2(2),BHLDKCLI   CLIENT                                       
         DROP  R3                                                               
*                                                                               
         BAS   RE,RSV              SET TO SPTFIL                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     HAVE CLIENT RECORD?                          
         BNE   GAAN10              NO                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTAAN,CPROF+6-CLTHDRD(R6)                                       
*                                                                               
GAAN10   BAS   RE,SSV              RESTORE XSPFIL                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   KEY(L'BHLDKEY),SAVEKEY   RESTORE DIR FOR SEQ READING             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
        LTORG                                                                   
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM56D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBHOLD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
ESTARTD  DS    H          ESTIMATE START DATE COMPRESSED                        
ESENDD   DS    H          ESTIMATE END DATE COMPRESSED                          
ERRFLAG  DS    X                                                                
FILTFLAG DS    X                                                                
FLTSKIP  EQU   X'80'                                                            
BHKEY    DS    CL32                                                             
SKEY     DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
MYESTART DS    CL6                                                              
MYEEND   DS    CL6                                                              
TEMPPRD  DS    CL3                                                              
ATABEND  DS    XL4                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
CLTAAN   DS    CL1                 SAVED CPROF+6 FROM CLT RECORD                
VDEMOVAL DS    F                                                                
SPPWCALC DS    F                                                                
*                                                                               
*                                                                               
* --- USED FOR CONVERTING THE DATES (EX. JAN/96 -TO- JAN01/96)                  
NTMPDT   DS    0CL8                                                             
NTMPMON  DS    CL3                                                              
NTMPDAY  DS    CL2                                                              
NTMPYR   DS    CL3                                                              
CHARYMD  DS    CL6                                                              
COMPYMD  DS    CL2                                                              
BINYMD   DS    XL3                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL1                                                              
LSTPRD   DS    CL3                                                              
         DS    CL1                                                              
LSTEST   DS    CL3                                                              
         DS    CL1                                                              
LSTSTA   DS    CL5                                                              
         DS    CL1                                                              
LSTMOS   DS    CL6                                                              
         DS    CL1                                                              
LSTGROSS DS    CL16                                                             
         DS    CL1                                                              
LSTNET   DS    CL16                                                             
         DS    CL1                                                              
LSTEDATE DS    CL8                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENNDOV                                                      
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPPWBLOCK                                                      
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048SPSFM61   02/18/15'                                      
         END                                                                    
