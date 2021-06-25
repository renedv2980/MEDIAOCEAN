*          DATA SET SPTRA2F    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T2162FA                                                                  
         TITLE 'T2162F NETWORK TRAFFIC SUPPLIER RECORD DISPLAY, CHANGE,X        
                AND LIST'                                                       
***********************************************************************         
*             PROGRAM-SPTRA2F MAINT-SPTRA4F LIST-SPTRA3F                        
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE                                                       
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                         CHANGE LOG                                  *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T2162F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2162F*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   MAIN10                                                           
         CLI   ACTNUM,ACTDEL                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    ACTERR                                                           
         B     VK                                                               
         SPACE                                                                  
MAIN10   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+16                                                             
         CLI   ACTNUM,ACTADD       ACTION ADD IS NOT SUPPORTED                  
         BE    ACTERR                                                           
         B     VR                                                               
         SPACE                                                                  
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         SPACE                                                                  
ACTERR   MVI   ERROR,INVACT        ADD, DELETE, RESTORE IS INVALID              
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         GOTO1 ERREX                                                            
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE                                                                  
VK       LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         XC    CLIENT,CLIENT                                                    
         LA    R2,TRACLTH          CLIENT                                       
         CLI   5(R2),0             ANY CLIENT                                   
         BE    VK10                OPTIONAL                                     
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVC   CLIENT,BCLT                                                      
         SPACE                                                                  
VK10     XC    NETWORK,NETWORK     NETWORK                                      
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BE    *+8                 OPTIONAL                                     
         BAS   RE,VNET                                                          
         SPACE                                                                  
         XC    PROGRAM,PROGRAM                                                  
         LA    R2,TRAPRGH          PROGRAM                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK15                OPTIONAL                                     
         CLI   TRANETH+5,0         ANY NETWORK                                  
         BNZ   *+12                                                             
         LA    R2,TRANETH                                                       
         B     MISSERR             MUST HAVE NETWORK                            
         SPACE                                                                  
         BAS   RE,VPROG                                                         
         SPACE                                                                  
VK15     XC    TSUPP,TSUPP                                                      
         LA    R2,TRATSUPH         TRAFFIC SUPPLIER                             
         CLI   5(R2),0             ANY TRAFFIC SUPPLIER                         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         B     MISSERR                                                          
         SPACE                                                                  
         MVC   TSUPP,8(R2)                                                      
         OC    TSUPP,SPACES                                                     
         SPACE                                                                  
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
VK20     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TSUPNRECD,R4        NETWORK FAX RECORD                           
         MVC   TSUPKID,=X'27'      RECORD ID                                    
         MVC   TSUPKAM,BAGYMD      AGY/MED                                      
         MVC   TSUPKCLT,CLIENT                                                  
         MVC   TSUPKNET,NETWORK                                                 
         MVC   TSUPKPRG,PROGRAM                                                 
         MVC   TSUPKTS,TSUPP                                                    
         SPACE                                                                  
         MVC   MYSVKEY,KEY         SAVE THE KEY                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 2                                                                
VR       L     R4,AIO                                                           
         USING TSUPKEY,R4                                                       
         MVC   BAGYMD,TSUPKAM                                                   
         MVC   CLIENT,TSUPKCLT                                                  
         MVC   NETWORK,TSUPKNET                                                 
         MVC   PROGRAM,TSUPKPRG                                                 
         MVC   TSUPP,TSUPKTS                                                    
         DROP  R4                                                               
         SPACE                                                                  
         LR    R6,R4                                                            
         SPACE                                                                  
         LA    R2,TRANAMEH                                                      
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TSUPCOEL,R6                                                      
         SPACE                                                                  
         MVI   TSUPCOEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   TSUPCOLN,32         ELEMENT LENGTH                               
         SPACE                                                                  
         GOTO1 ANY                 MOVES DATA LEFT JUST                         
         MVC   TSUPCONT,WORK                                                    
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         LA    R2,TRAFAXH          PREFIX                                       
         USING FAXD,R2                                                          
         SPACE                                                                  
         MVI   ELCODE,X'20'        FAX DATA ELEMENT                             
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TSUPNOEL,R6                                                      
         SPACE                                                                  
         MVI   TSUPNOEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   TSUPNOLN,TSUPNOEQ-TSUPNOEL ELEMENT LENGTH                        
         SPACE                                                                  
         MVI   SVPREFIX,C' '                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    VR10                 NO                                          
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        CANADA?                                      
         BNE   PREFERR             NO, ERROR                                    
         SPACE                                                                  
         TM    4(R2),X'08'         IS IT NUMERIC                                
         BZ    FXNUMERR                                                         
         SPACE                                                                  
         CLI   FAX1,C'1'           PREFIX MUST BE 1                             
         BNE   PREFIX1                                                          
         SPACE                                                                  
         MVC   SVPREFIX,FAX1       SAVE PREFIX                                  
         SPACE                                                                  
VR10     LA    RF,TSUPNO                                                        
         CLI   FAXAH+5,0           WAS AREA CODE ENTERED                        
         BE    MISSERR                                                          
         CLI   FAXAH+5,3                                                        
         BNE   FAXNERR             INVALID NUMBER RE-ENTER                      
         TM    FAXAH+4,X'08'       IS IT NUMERIC                                
         BZ    FXNUMERR                                                         
         SPACE                                                                  
         MVC   0(L'FAXA,RF),FAXA   MOVE AREA CODE INTO THE ELEM                 
         LA    RF,L'FAXA(RF)                                                    
         MVI   0(RF),C' '                                                       
         SPACE                                                                  
         LA    RF,1(RF)            POINT TO THE EXCHANGE                        
         CLI   FAXEH+5,0           WAS EXCHANGE ENTERED                         
         BE    MISSERR                                                          
         CLI   FAXEH+5,3                                                        
         BNE   FAXNERR             INVALID FAX NUMBER                           
         TM    FAXEH+4,X'08'       IS IT NUMERIC                                
         BZ    FXNUMERR                                                         
         MVC   0(L'FAXE,RF),FAXE   MOVE EXCHANGE INTO THE ELEM                  
         LA    RF,L'FAXE(RF)                                                    
         MVI   0(RF),C'-'                                                       
         SPACE                                                                  
         LA    RF,1(RF)            POINT TO THE NUMBER                          
         CLI   FAXNH+5,0           WAS NUMBER ENTERED                           
         BE    MISSERR                                                          
         CLI   FAXNH+5,4                                                        
         BNE   FAXNERR             INVALID FAX NUMBER                           
         TM    FAXNH+4,X'08'       IS IT NUMERIC                                
         BZ    FXNUMERR                                                         
         MVC   0(L'FAXN,RF),FAXN   MOVE NUMBER INTO THE ELEM                    
         LA    RF,L'FAXN(RF)       POINT TO THE NUMBER                          
         MVI   0(RF),C' '                                                       
         SPACE                                                                  
         LA    RF,1(RF)                                                         
         MVC   0(1,RF),SVPREFIX    MOVE PREFIX INTO THE ELEM                    
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         LA    R2,TRADESCH                                                      
         SPACE                                                                  
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         CLI   5(R2),0             ANY DESCRIPTION (OPTIONAL)                   
         BE    DR                   NO, DO DISPLAY                              
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TSUPDSEL,R6                                                      
         SPACE                                                                  
         MVI   TSUPDSEL,X'30'      ELEMENT IDENTIFIER                           
         MVI   TSUPDSLN,32         ELEMENT LENGTH                               
         SPACE                                                                  
         GOTO1 ANY                 MOVES DATA LEFT JUST                         
         MVC   TSUPDESC,WORK                                                    
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         B     DR                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       L     R6,AIO                                                           
         XC    TRADESC,TRADESC                                                  
         LA    R2,TRADESCH         DESCRIPTION                                  
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL            GET DESCRIPTION ELEMENT                      
         BNE   DR05                                                             
         SPACE                                                                  
         USING TSUPDSEL,R6                                                      
         SPACE                                                                  
         MVC   WORK(L'TRADESC),SPACES                                           
         MVC   WORK(L'TSUPDESC),TSUPDESC                                        
         MVC   8(L'TRADESC,R2),WORK                                             
         OI    6(R2),X'80'         TRANSMIT NEW FIELD                           
         SPACE                                                                  
DR05     L     R6,AIO                                                           
         XC    TRANAME,TRANAME                                                  
         LA    R2,TRANAMEH         CONTACT NAME                                 
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DR10                                                             
         SPACE                                                                  
         USING TSUPCOEL,R6                                                      
         SPACE                                                                  
         MVC   WORK(L'TRANAME),SPACES                                           
         MVC   WORK(L'TSUPCONT),TSUPCONT                                        
         MVC   8(L'TRANAME,R2),WORK                                             
         OI    6(R2),X'80'         TRANSMIT NEW FIELD                           
         SPACE                                                                  
DR10     L     R6,AIO                                                           
         XC    TRAFAX,TRAFAX                                                    
         LA    R2,TRAFAXH          PREFIX                                       
         USING FAXD,R2                                                          
         SPACE                                                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         SPACE                                                                  
         USING TSUPNOEL,R6                                                      
         SPACE                                                                  
         XC    FAX1,FAX1                                                        
         MVC   FAX1,TSUPNO1        PREFIX                                       
         OI    FAX1H+6,X'80'       TRANSMIT                                     
         SPACE                                                                  
         XC    FAXA,FAXA           AREA CODE                                    
         MVC   FAXA,TSUPNOA                                                     
         OI    FAXAH+6,X'80'       TRANSMIT                                     
         SPACE                                                                  
         XC    FAXE,FAXE           EXTENSION                                    
         MVC   FAXE,TSUPNOE                                                     
         OI    FAXEH+6,X'80'       TRANSMIT                                     
         SPACE                                                                  
         XC    FAXN,FAXN           NUMBER                                       
         MVC   FAXN,TSUPNON                                                     
         OI    FAXNH+6,X'80'       TRANSMIT                                     
         SPACE                                                                  
DRX      B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       L     R4,AIO                                                           
         USING TSUPNRECD,R4                                                     
         SPACE                                                                  
         XC    TRACLT,TRACLT                                                    
         XC    TRANET,TRANET                                                    
         XC    TRAPRG,TRAPRG                                                    
         XC    TRATSUP,TRATSUP                                                  
         SPACE                                                                  
         OC    TSUPKCLT,TSUPKCLT   ANY CLIENT?                                  
         BZ    DK10                 NO                                          
         GOTO1 CLUNPK,DMCB,TSUPKCLT,QCLT                                        
         MVC   TRACLT,QCLT                 CLIENT                               
         SPACE                                                                  
DK10     MVC   TRANET,TSUPKNET     DISPLAY NETWORK                              
         MVC   TRAPRG,TSUPKPRG             PROGRAM                              
         MVC   TRATSUP,TSUPKTS             OFFICE NUMBER                        
         SPACE                                                                  
         OI    TRACLTH+6,X'80'                                                  
         OI    TRANETH+6,X'80'     SET ON TRANSMIT BIT                          
         OI    TRAPRGH+6,X'80'                                                  
         OI    TRATSUPH+6,X'80'                                                 
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         USING TSUPKEY,R4                                                       
         OC    KEY(20),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                NO, GOTO HIGH                                
         SPACE                                                                  
         MVC   KEY,MYSVKEY         MOVE IN SAVED KEY                            
         SPACE                                                                  
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         SPACE                                                                  
* DO READHI                                                                     
         SPACE                                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         MVC   NET,TSUPKNET                                                     
         CLC   MYSVKEY(2),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         SPACE                                                                  
LR16     CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(33),=C'NO TRAFFIC SUPPLIER RECORD FOUND'                       
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         SPACE                                                                  
LR22     CLC   KEY(2),MYSVKEY      AT END OF THIS AGENCY/MEDIA                  
         BNE   EXIT                YES                                          
         SPACE                                                                  
         OC    CLIENT,CLIENT       SEE IF CLIENT WAS ENTERED                    
         BZ    *+14                                                             
         CLC   BCLT,TSUPKCLT       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
         SPACE                                                                  
         OC    NETWORK,NETWORK     SEE IF NETWORK WAS ENTERED                   
         BZ    *+14                                                             
         CLC   NETWORK,TSUPKNET    IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
         SPACE                                                                  
         OC    PROGRAM,PROGRAM     SEE IF PROGRAM WAS ENTERED                   
         BZ    *+14                                                             
         CLC   PROGRAM,TSUPKPRG    IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
         SPACE                                                                  
         OC    TSUPP,TSUPP         WAS TRAFFIC SUPPLIER ENTERED                 
         BZ    *+14                                                             
         CLC   TSUPP,TSUPKTS       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
         SPACE                                                                  
         CLC   NET,TSUPKNET        NETWORK CHANGE ?                             
         BE    LR50                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   NET,TSUPKNET        NETWORK CHANGE                               
         SPACE                                                                  
LR50     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         SPACE                                                                  
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      LA    R5,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R5                                                       
         SPACE                                                                  
         OC    TSUPKCLT,TSUPKCLT                                                
         BZ    LRL05                                                            
         GOTO1 CLUNPK,DMCB,TSUPKCLT,QCLT                                        
         MVC   LCLT,QCLT           CLIENT                                       
         SPACE                                                                  
LRL05    MVC   LNET,TSUPKNET       NETWORK                                      
         MVC   LPRG,TSUPKPRG       PROGRAM                                      
         MVC   LTSUP,TSUPKTS       TRAFFIC SUPPLIER                             
         SPACE                                                                  
         MVI   ELCODE,X'10'        CONTACT NAME                                 
         BAS   RE,GETEL                                                         
         BNE   LRL10                                                            
         SPACE                                                                  
         USING TSUPCOEL,R6                                                      
         SPACE                                                                  
         MVC   LCNTNAME,TSUPCONT                                                
         SPACE                                                                  
LRL10    L     R6,AIO              AIO                                          
         MVI   ELCODE,X'20'        FAX ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   LRL20                                                            
         SPACE                                                                  
         USING TSUPNOEL,R6                                                      
         SPACE                                                                  
         MVC   LFAX(1),TSUPNO1     PREFIX IF ANY                                
         MVI   LFAX+1,C'('                                                      
         MVC   LFAX+2(3),TSUPNOA   AREA CODE                                    
         MVI   LFAX+5,C')'                                                      
         MVC   LFAX+7(8),TSUPNOE   EXTENTION AND NUMBER                         
         SPACE                                                                  
LRL20    L     R6,AIO              AIO                                          
         MVI   ELCODE,X'30'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LRL30                                                            
         SPACE                                                                  
         USING TSUPDSEL,R6                                                      
         SPACE                                                                  
         MVC   LDESC,TSUPDESC                                                   
         SPACE                                                                  
LRL30    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         LA    R4,KEY                                                           
         B     LR20                                                             
         SPACE 3                                                                
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      LA    R5,P                PRINT LINE ADDRESS                           
         MVC   P,SPACES                                                         
         USING PRTLINE,R5                                                       
         SPACE                                                                  
         OC    TSUPKCLT,TSUPKCLT                                                
         BZ    LRR10                                                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,TSUPKCLT,PCLT                                        
         SPACE                                                                  
LRR10    MVC   PNET,TSUPKNET       NETWORK                                      
         MVC   PPRG,TSUPKPRG       PROGRAM                                      
         MVC   PTSUP,TSUPKTS       TRAFFIC SUPPLIER                             
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR15                                                            
         SPACE                                                                  
         USING TSUPCOEL,R6                                                      
         SPACE                                                                  
         MVC   PCNTNAME,TSUPCONT   PRINT CONTACT NAME                           
         SPACE                                                                  
LRR15    L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR20                                                            
         SPACE                                                                  
         USING TSUPNOEL,R6                                                      
         SPACE                                                                  
         MVC   PFAX(1),TSUPNO1      PREFIX IF ANY                               
         MVI   PFAX+1,C'('                                                      
         MVC   PFAX+2(3),TSUPNOA    AREA CODE                                   
         MVI   PFAX+5,C')'                                                      
         MVC   PFAX+6(8),TSUPNOE    EXTENTION AND NUMBER                        
         SPACE                                                                  
LRR20    L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR25                                                            
         SPACE                                                                  
         USING TSUPDSEL,R6                                                      
         SPACE                                                                  
         MVC   PDESC,TSUPDESC                                                   
         SPACE                                                                  
LRR25    CLC   P,SPACES                                                         
         BE    LR20                                                             
         OC    P,P                                                              
         BZ    LR20                                                             
LRR30    GOTO1 SPOOL,DMCB,(R8)     PRINT                                        
         B     LR20                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIE           
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
         SPACE                                                                  
VPROG    NTR1                                                                   
         USING NPGRECD,R4                                                       
         SPACE                                                                  
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVC   NPGKTYP,=X'0D20'    NETWORK PROGRAM RECORD                       
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BNET                                                     
         MVC   NPGKPROG,WORK                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     TEST KEYS WITHOUT END DATE                   
         BE    *+8                                                              
         B     PROGERR             PROGRAM DOES NOT EXIST                       
         MVC   PROGRAM,WORK        SAVE IT                                      
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
         SPACE                                                                  
FAXNERR  XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELINVAL),TELINVAL TEL NMBR INVALID                    
         B     ERREXIT                                                          
TELINVAL DC    C'* ERROR * TELEPHONE NUMBER INVALID. PLEASE RE-ENTER *'         
         SPACE                                                                  
FXNUMERR XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELNNUM),TELNNUM TEL NMBR MUST BE NUMERI               
         B     ERREXIT                                                          
TELNNUM  DC    C'* ERROR * TELEPHONE NUMBER MUST BE NUMERIC *'                  
         SPACE                                                                  
PREFERR  XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'NPREFIX),NPREFIX FOR CANADIAN AGY ONLY                 
         B     ERREXIT                                                          
NPREFIX  DC    C'* ERROR * VALID FOR CANADIAN AGENCY ONLY *'                    
         SPACE                                                                  
PREFIX1  XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'PRFIX1),PRFIX1                                         
         B     ERREXIT                                                          
PRFIX1   DC    C'* ERROR * 1 IS THE ONLY VALID ENTRY *'                         
         SPACE                                                                  
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGERRMS),PRGERRMS                                     
         B     ERREXIT                                                          
PRGERRMS DC    C'* ERROR * PROGRAM NOT FOUND *'                                 
         SPACE                                                                  
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         B     ERREXIT                                                          
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'______________'                                           
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,35,C'TRAFFIC SUPPLIER LIST'                                   
         SSPEC H2,35,C'_____________________'                                   
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H7,3,C'TRAFFIC'                                                  
         SSPEC H8,3,C'SUPPLIER'                                                 
         SSPEC H9,3,C'--------'                                                 
         SSPEC H8,13,C'DESCRIPTION'                                             
         SSPEC H9,13,C'-----------'                                             
         SSPEC H8,46,C'CONTACT NAME'                                            
         SSPEC H9,46,C'------------'                                            
         SSPEC H8,78,C'FAX'                                                     
         SSPEC H9,78,C'---'                                                     
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
         SPACE                                                                  
* ONLINE FAX NUMBER LINE                                                        
         SPACE                                                                  
FAXD     DSECT                                                                  
FAX1H    DS    CL8                                                              
FAX1     DS    CL1                                                              
         DS    CL8                                                              
         DS    CL1                                                              
         DS    CL8                                                              
         DS    CL1                                                              
FAXAH    DS    CL8                                                              
FAXA     DS    CL3                                                              
         DS    CL8                                                              
         DS    CL1                                                              
FAXEH    DS    CL8                                                              
FAXE     DS    CL3                                                              
         DS    CL8                                                              
         DS    CL1                                                              
FAXNH    DS    CL8                                                              
FAXN     DS    CL4                                                              
         SPACE                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
PRTLINE  DSECT                                                                  
         DS    CL2                                                              
PTSUP    DS    CL5                                                              
         DS    CL5                                                              
PDESC    DS    CL30                                                             
         DS    CL3                                                              
PCNTNAME DS    CL30                                                             
         DS    CL1                                                              
PFAX     DS    CL20                                                             
PCLT     DS    0C                                                               
PNET     DS    0C                                                               
PPRG     DS    0C                                                               
         SPACE                                                                  
*ONLINE LIST LINE                                                               
         SPACE                                                                  
LSTLINE  DSECT                                                                  
LTSUP    DS    CL5                                                              
         DS    CL2                                                              
LDESC    DS    CL15                                                             
         DS    CL4                                                              
LCNTNAME DS    CL30                                                             
         DS    CL2                                                              
LFAX     DS    CL14                                                             
LCLT     DS    0C                                                               
LNET     DS    0C                                                               
LPRG     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE SPTRNTSUPP                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA4FD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
MYSVKEY  DS    CL48                                                             
NETWORK  DS    CL4                                                              
TSUPP    DS    CL5                                                              
NET      DS    CL4                                                              
PROGRAM  DS    CL6                                                              
CLIENT   DS    CL2                                                              
BNET     DS    H                                                                
*                                                                               
SVPREFIX DS    CL1                                                              
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPTRA2F   05/01/02'                                      
         END                                                                    
