*          DATA SET SPTRA45    AT LEVEL 020 AS OF 08/13/10                      
*PHASE T21645C                                                                  
         TITLE 'T21645 SHIPPING RECAP '                                         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*                    AND SHIP RECAP RECS                              *         
*             AIO2 - TRAFFIC STATION ADDRESS IF T1 PROF11 ON          *         
*             AIO3 - COMML RECORD IN RECAP REPORT RPR                 *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE       *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - UNUSED                                                  *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  LEV 09-10 MAY05/88 FIX RUNNING OVER END OF CLIENT INTO NEXT        *         
*  LEV 11-12 MAR01/90 SHOW STATION COMML TYPE IF PROFILE SET          *         
*  LEV 13    MAY20/92 SHOW MARKET NUMBER, AFFILIATE FILTER,           *         
*                      RDUPDATE                                       *         
*  LEV 14    MAR18/93 CABLE HEAD                                      *         
*  LEV 15    MAR13/96 ADD FILTER ON SHIP DATE                         *         
*  LEV 16 BOTH FEB09/98 FIX R2 FOR REPORT TOO BIG                     *         
*  LEV 17 SMUR JUN04/01 USE TRAFFIC OFFICE                            *         
*  LEV 18 BGRI MAR03/04 DELETE MVI GCMODE,C'X' - CAUSES DUMP IN PRTQ  *         
*                                                                     *         
***********************************************************************         
*                                                                               
T21645   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21645**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       DISPLAY RECAP ONLINE                         
         BE    RPL                                                              
         CLI   MODE,PRINTREP       PRINT RECAP REPORT                           
         BE    RPR                                                              
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 VALICLT                                                          
*                                                                               
* READ T1 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T1'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
*                                                                               
* READ TS (SHIPPING) PROFILE *                                                  
*                                                                               
         MVI   WORK+3,C'S'                                                      
         GOTO1 (RF),(R1),,SVTSPROF                                              
*                                                                               
         LA    R2,TRASTAH          STATION                                      
         XC    BMKTSTA,BMKTSTA                                                  
         XC    QSTA,QSTA                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK10                NO, ERROR                                    
         GOTO1 ANY                                                              
         CLC   =C'ALL',WORK                                                     
         BE    VK10                                                             
         GOTO1 VALISTA                                                          
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   VK10                                                             
         XC    BMKT,BMKT                                                        
*                                                                               
VK10     LA    R2,TRAFLTRH                                                      
         BAS   RE,VFTR                                                          
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SHPKEY,R4                                                        
         MVC   SHPKID,=XL2'0A25'                                                
         MVC   SHPKAM,BAGYMD                                                    
         MVC   SHPKCLT,BCLT                                                     
         MVC   SHPKMKT(5),BMKTSTA                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY2,KEY                SVKEY2 FOR OFFLINE RECAP               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE RECAP LIST                                                             
*                                                                               
RPL      LA    R4,KEY                                                           
         USING SHPKEY,R4                                                        
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   RPL14               NO, FIND POSITION IN LIST                    
         XC    SVKEY,SVKEY                                                      
         MVC   SHPKID(2),=XL2'0A25' BUILD KEY FOR READ HI                       
         MVC   SHPKAM(3),BAGYMD & BCLT                                          
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   *+14                                                             
         MVC   SHPKSTA,BSTA        ONLY CHECK ON STATION                        
         B     RPL10                                                            
*                                                                               
         MVC   SHPKMKT(5),BMKTSTA  MOVE IN MARKET/STATION                       
RPL10    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(5),KEYSAVE      IS THERE SUCH A RECORD                       
         BNE   NOSHIPER            NO, ISSUE ERROR MSG                          
         OC    BMKTSTA,BMKTSTA     STATION=ALL                                  
         BZ    RPL22                YES                                         
         OC    BMKT,BMKT           ONLY FILTER ON STATION                       
         BZ    RPL22                YES                                         
         CLC   KEY+5(5),KEYSAVE+5                                               
         BNE   NOSHIPER            NO, ISSUE ERROR MSG                          
         B     RPL22                                                            
*                                                                               
* CK RESTART IS SAME AGENCY & CLIENT *                                          
*                                                                               
RPL14    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(5),KEYSAVE      IS THERE SUCH A RECORD                       
         BNE   EXIT                NO, DONE                                     
         MVC   WORK(2),=XL2'0A25'  BUILD KEY FOR COMPARE                        
         MVC   WORK+2(3),BAGYMD                                                 
         CLC   WORK(5),KEY                                                      
         BE    RPL22                                                            
         B     EXIT                                                             
*                                                                               
RPL20    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
*                                                                               
RPL22    OC    SVKEY,SVKEY         WAS LAST REC COMPLETELY USED?                
         BZ    RPL26                YES                                         
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE DISK ADDR                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET LAST REC                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RPL50               EMPTY RECORD, GET NEXT                       
         SR    R3,R3                                                            
*                                                                               
RPL24    LA    R3,1(,R3)                                                        
         C     R3,ELEMCT           RESTORE ELEMENT POINTER                      
         BE    RPL30               AND LIST REMAINDER OF ELEMS                  
         BAS   RE,NEXTEL                                                        
         BNE   RPL20                                                            
         B     RPL24                                                            
*                                                                               
RPL26    CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         OC    BMKTSTA,BMKTSTA     STATION=ALL                                  
         BZ    RPL28                YES                                         
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   *+18                                                             
         CLC   KEY+7(3),BSTA       ONLY CHECK ON STATION                        
         BNE   RPL20                NO, BUT READ ALL                            
         B     RPL28                                                            
*                                                                               
         CLC   KEY+5(5),KEYSAVE+5                                               
         BNE   EXIT                NO, DONE                                     
*                                                                               
RPL28    BAS   RE,FMTSTA           FORMAT STATION                               
         BNE   RPL20                FILTERS BYPASS                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING SHPKEY,R4                                                        
         LR    R6,R4                                                            
         EJECT                                                                  
* FORMAT ONLINE RECAP LIST                                                      
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SHPKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RMKT,DUB                                                         
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   *+10                                                             
         MVC   RMKT,QMKT                                                        
*                                                                               
         MVC   RSTA,STAPRNT                                                     
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   RSTA(8),STANET                                                   
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RPL50               IF NO ELEMENT, READ NEXT REC                 
         USING SHPDTAEL,R6                                                      
         MVC   SVKEY,KEY           SAVE REC KEY/DISK ADDR UNTIL DONE            
         SR    R3,R3               ZERO ELEM CT                                 
*                                                                               
RPL30    LA    R3,1(,R3)           ADD TO ELEM CT                               
         CLI   SHPPIG,0            IS THIS A PIGGY-BACK                         
         BNE   RPL40               YES, FOR REF ONLY, DON'T PRINT TWICE         
         OC    CMLFTR,CMLFTR                                                    
         BZ    RPL30C                                                           
         ZIC   R1,CMLFTRLN                                                      
         EX    R1,RPLCLC1                                                       
         BE    RPL30C                                                           
         CLI   SHPCMML2,0                                                       
         BE    RPL40                                                            
         EX    R1,RPRCLC2                                                       
         BNE   RPL40                                                            
*                                                                               
*MNADID                                                                         
RPL30C   DS    0H                                                               
         CLI   SVADILEN,0         ANY ADID FILTER ENTERED?                      
         BE    RPL30H                                                           
         TM    SHPNOSHP,X'01'                                                   
         BZ    RPL40                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),SVADIWRK                             
         ZIC   R1,SVADILEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVADIWRK(0),SVCMLADI                                             
         BE    RPL30H                                                           
         CLI   SHPCMML2,0                                                       
         BE    RPL40                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),SVADIWRK                            
         ZIC   R1,SVADILEN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVADIWRK(0),SVCMLADI                                             
         BNE   RPL40                                                            
*MNADID                                                                         
*                                                                               
RPL30H   OC    SVSHPDT,SVSHPDT                                                  
         BZ    RPL31                                                            
         CLC   SVSHPDT,SHPSHPDT    THIS REQUESTED DATE                          
         BNE   RPL40                                                            
         B     RPL31                                                            
*                                                                               
RPLCLC1  CLC   CMLFTR(0),SHPCMML                                                
RPLCLC2  CLC   CMLFTR(0),SHPCMML2                                               
*                                                                               
RPL31    MVC   RCMLIDA(8),SHPCMML                                               
         OC    SHPCMML2,SHPCMML2   IS THERE A PIGGY-BACK CML                    
         BZ    RPL32               NO                                           
         MVI   RCMLID,C'-'                                                      
         MVC   RCMLIDB(8),SHPCMML2                                              
*                                                                               
RPL32    TM    SHPNOSHP,SHPISADI                                                
         BZ    RPL33                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),RCMLIDA                              
         OC    SHPCMML2,SHPCMML2                                                
         BZ    RPL33                                                            
         GOTO1 (RF),(R1),(C'U',SHPCMML2),RCMLIDB                                
*                                                                               
RPL33    OC    SHPSHPDT,SHPSHPDT   IS THERE A SHIPPING DATE                     
         BZ    RPL34               NO                                           
         GOTO1 DATCON,DMCB,(3,SHPSHPDT),(5,RSHPDTE)                             
*                                                                               
RPL34    GOTO1 DATCON,DMCB,(3,SHPLTD),(5,RSHPLTD)                               
         TM    SHPNOSHP,X'40'      TEST FOR MANUAL SHIP                         
         BZ    RPL36                                                            
         MVI   RSHPMS,C'*'                                                      
*                                                                               
RPL36    TM    SHPNOSHP,X'80'      TEST FOR NO SHIP                             
         BZ    RPL38                                                            
         MVI   RSHPNS,C'*'                                                      
*                                                                               
RPL38    ST    R3,ELEMCT           SAVE ELEMENT POINTER                         
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
*                                                                               
RPL40    BAS   RE,NEXTEL                                                        
         BE    RPL30                                                            
*                                                                               
RPL50    XC    SVKEY,SVKEY         ZERO SVKEY, DONE WITH THIS REC               
         B     RPL20                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
* OFFLINE RECAP REPORT                                                          
*                                                                               
RPR      LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R2,P                                                             
         MVI   PIGYSW,C'N'                                                      
         MVI   ELCODE,X'10'                                                     
         MVC   KEY,SVKEY2                    SVKEY2 IS SET IN VALKEY            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                              - AND BEFORE LISTMON.          
         B     RPR10                                                            
*                                                                               
RPR05    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
RPR10    CLC   KEY(5),SVKEY2       IS THERE SUCH A RECORD                       
         BNE   EXIT                                                             
         OC    BMKTSTA,BMKTSTA     STATION=ALL                                  
         BZ    RPR14                                                            
         CLC   KEY+5(5),KEYSAVE+5                                               
         BNE   EXIT                                                             
*                                                                               
RPR14    CLI   OFFLINE,C'Y'        IF OFFLINE, RUN ANYTHING                     
         BE    RPR16                                                            
*                                                                               
* CHECK IF APPROACHING MAX I/O'S *                                              
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(,R1)          COMFACS                                      
         USING COMFACSD,R1                                                      
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB             FAFACTS                                      
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         ICM   RE,3,FATIOCNT       CURRENT I/O CT                               
         LA    RE,50(RE)           ADD 50                                       
         CLM   RE,3,FATMAXIO       AT OR NEAR MAX                               
         BNL   REPTERR             TOO LONG TO RUN ONLINE                       
         DROP  R1                                                               
RPR16    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLC   KEY+5(5),SVMKTSTA   CHANGE IN STATION                            
         BE    RPR18                                                            
         BAS   RE,FMTSTA                                                        
         BNE   RPR05                                                            
         MVC   SVMKTSTA,KEY+5                                                   
         MVI   FORCEHED,C'Y'                                                    
RPR18    L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING SHPDTAEL,R6                                                      
         BNE   RPR05                                                            
*                                                                               
RPR20    DS    0H                                                               
         CLI   SHPPIG,0            IS IT A PIGGYBACK(FOR REF ONLY)              
         BNE   RPR40               YES, GET NXT EL                              
         OC    CMLFTR,CMLFTR                                                    
         BZ    RPR21                                                            
         ZIC   R1,CMLFTRLN                                                      
         EX    R1,RPRCLC1                                                       
         BE    RPR21                                                            
         CLI   SHPCMML2,0                                                       
         BE    RPR40                                                            
         EX    R1,RPRCLC2                                                       
         BNE   RPR40                                                            
*                                                                               
*MNADID                                                                         
RPR21    DS    0H                                                               
         CLI   SVADILEN,0         ANY ADID FILTER ENTERED?                      
         BE    RPR21H                                                           
         TM    SHPNOSHP,X'01'                                                   
         BZ    RPR40                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),SVADIWRK                             
         ZIC   R1,SVADILEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVADIWRK(0),SVCMLADI                                             
         BE    RPR21H                                                           
         CLI   SHPCMML2,0                                                       
         BE    RPR40                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),SVADIWRK                            
         ZIC   R1,SVADILEN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVADIWRK(0),SVCMLADI                                             
         BNE   RPR40                                                            
*MNADID                                                                         
*                                                                               
RPR21H   OC    SVSHPDT,SVSHPDT                                                  
         BZ    RPR22                                                            
         CLC   SVSHPDT,SHPSHPDT    THIS REQUESTED DATE                          
         BNE   RPR40                                                            
         B     RPR22                                                            
*                                                                               
RPRCLC1  CLC   CMLFTR(0),SHPCMML                                                
RPRCLC2  CLC   CMLFTR(0),SHPCMML2                                               
*                                                                               
RPR22    MVC   PCML,SHPCMML                                                     
         MVC   CMLIDSV,SHPCMML                                                  
         CLI   SHPCMML2,0                                                       
         BE    RPR24                                                            
         MVI   PCMLD,C'-'                                                       
         MVC   PCMLP,SHPCMML2                                                   
         MVC   CMLIDSV2,SHPCMML2                                                
         MVI   PIGYSW,C'Y'                                                      
*                                                                               
RPR24    TM    SHPNOSHP,SHPISADI                                                
         BZ    RPR28                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),PCML                                 
         CLI   SHPCMML2,0                                                       
         BE    RPR28                                                            
         GOTO1 (RF),(R1),(C'U',SHPCMML2),PCMLP                                  
*                                                                               
RPR28    CLI   SHPSHPDT,0                                                       
         BE    RPR30                                                            
         GOTO1 DATCON,DMCB,(3,SHPSHPDT),(5,PSHPDTE)                             
*                                                                               
RPR30    GOTO1 DATCON,DMCB,(3,SHPLTD),(5,PLSTTEL)                               
         TM    SHPNOSHP,X'40'                                                   
         BZ    *+10                                                             
         MVC   PMANSHP,=C'MS'                                                   
         TM    SHPNOSHP,X'80'                                                   
         BZ    *+10                                                             
         MVC   PNOSHP,=C'NS'                                                    
*                                                                               
         BAS   RE,GTCMLRC         GETS CML REC/FILLS IN TITLE, TYPE             
*                                                                               
         MVC   SVKEY2,KEY                                                       
         LA    R2,P                                                             
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)     WILL PRINT 1 LINE/2 FOR PIGGYBACKS           
*                                                                               
RPR40    BAS   RE,NEXTEL                                                        
         BNE   RPR05                                                            
         B     RPR20                                                            
         EJECT                                                                  
* GET CMML PROFILE REC. EXITS WITH P (AND P2) SET WITH TITLE, TYPE *            
*                                                                               
GTCMLRC  NTR1                                                                   
         MVI   TYPESW,C'Y'                                                      
         MVC   SVKEY2,KEY                                                       
         MVC   AIO,AIO3                                                         
         MVC   DMDSKSV,DMDSKADD                                                 
*                                                                               
         MVC   KEY(2),=X'0A21'      CMML PROFILE ID                             
         MVC   KEY+5(8),CMLIDSV                                                 
GTC10    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GTC12                                                            
         MVC   PCMLTLE-P(18,R2),=C'CMML REC NOT FOUND'                          
         B     GTC20                                                            
GTC12    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING CMLDTAEL,R6                                                      
         BE    GTC14                                                            
         MVC   PCMLTLE-P(23,R2),=C'CMML REC ELEM NOT FOUND'                     
         B     GTC20                                                            
GTC14    MVC   PCMLTLE-P(15,R2),CMLTITLE                                        
         CLI   TYPESW,C'N'         IS TYPESW ON                                 
         BE    GTCX                NO,EXIT                                      
         MVC   PCMLTYP-P(,R2),CMLTYPE                                           
*                                                                               
         OC    SVSTCMLT,SVSTCMLT                                                
         BZ    GTC20                                                            
         MVC   PCMLTYP-P(,R2),SVSTCMLT                                          
*                                                                               
GTC20    CLI   PIGYSW,C'N'         IS THERE A PIGGY                             
         BE    GTCX                NO,EXIT                                      
*                                                                               
         MVC   KEY+5(8),CMLIDSV2   YES, PIGGY, SET NEW CMLID                    
         MVI   PIGYSW,C'N'                            -   PIGY                  
         MVI   TYPESW,C'N'                            -   TYPESW                
         LA    R2,132(R2)          SET R2 TO P LINE 2                           
         B     GTC10                                                            
*                                                                               
GTCX     MVC   KEY(13),SVKEY2          RESTORE KEY                              
         MVC   AIO,AIO1                   - I/O AREA                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                       - READ SEQ                            
         MVC   DMDSKADD,DMDSKSV           - REC DSK ADDRS                       
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING                                            
*                                                                               
FMTSTA   NTR1                                                                   
*                                                                               
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',KEY+5),QMKT,WORK                              
*                                                                               
         CLC   WORK+5(3),SPACES                                                 
         BE    *+14                                                             
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
*                                                                               
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         CLI   QMED,C'T'                                                        
         BE    FMTSTA10                                                         
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTA10                                                         
         MVI   3(RE),C' '                                                       
*                                                                               
FMTSTA10 XC    SVSTCMLT,SVSTCMLT                                                
         XC    SVKEY,SVKEY                                                      
         CLI   SVT1PR11,C'Y'       USE STATION COMML TYPE                       
         BNE   FMTSTA20                                                         
         CLI   MODE,PRINTREP       PRINT RECAP REPORT                           
         BNE   FMTSTA20                                                         
*                                                                               
* NOW SAVE AND BUILD STATION KEY                                                
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   DMDSKSV,DMDSKADD                                                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         MVC   STAKID,=XL2'0A28'                                                
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,WORK                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'STADDKEY),KEYSAVE                                          
         BNE   FMTSTA20                                                         
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STADTAEL,R6                                                      
         MVC   SVSTCMLT,STACMLT                                                 
*                                                                               
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
*                                                                               
FMTSTA20 OC    SVKEY,SVKEY                                                      
         BNZ   *+10                                                             
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),WORK                                                    
         MVC   KEY+7(2),AGENCY                                                  
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BE    *+10                                                             
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         L     R6,AIO2                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,(R6)                     
*                                                                               
         USING STAMASD,R6                                                       
         MVC   SVSTAFF,SNETWRK                                                  
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   FMTSTA30                                                         
         MVC   QMKT,SMKT                                                        
*                                                                               
FMTSTA30 OC    SVKEY,SVKEY             WAS KEY USED                             
         BZ    FMTSTAX                                                          
         MVC   KEY(13),SVKEY           RESTORE KEY                              
         XC    SVKEY,SVKEY              CLEAR KEY                               
         MVC   AIO,AIO1                   - I/O AREA                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                    FOR READ SEQ                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DMDSKADD,DMDSKSV           - REC DSK ADDRS                       
*                                                                               
         OC    AFFFTR,AFFFTR          FILTER BY AFFILIATE                       
         BZ    FMTSTAX                                                          
*                                                                               
         CLC   AFFFTR,SNETWRK         THIS REQUESTED AFFFILIATE                 
         BNE   EXIT                                                             
FMTSTAX  CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE FILTER ROUTINES                                                      
*                                                                               
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRH                                                            
*                                                                               
         LA    R4,BLOCK+240        ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(7,(R4))                              
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR              NO, ERROR                                   
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VFTRCLCA         THIS A COMMERCIAL                            
         BNE   VFTR20                                                           
         CLI   1(R4),8             MAX LEN                                      
         BH    CMLNER                                                           
         BL    VFTR14                                                           
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD                                                 
         MVC   CMLKCML,22(R4)                                                   
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CMLERR                                                           
VFTR14   MVC   CMLFTR,22(R4)                                                    
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         STC   R1,CMLFTRLN                                                      
         B     VFTRNXT                                                          
*                                                                               
VFTR20   EX    R1,VFTRCLCB         THIS AN AFFILIATE                            
         BNE   VFTR30                                                           
         CLI   1(R4),0             MUST HAVE AN ENTRY                           
         BE    AFFERR                                                           
         MVC   AFFFTR,22(R4)                                                    
         B     VFTRNXT                                                          
*                                                                               
VFTR30   EX    R1,VFTRCLCS         THIS A SHIP DATE                             
         BNE   VFTR40                                                           
         CLI   1(R4),0             MUST HAVE AN ENTRY                           
         BE    SHPDTERR                                                         
*                                                                               
         GOTO1 DATVAL,DMCB,22(R4),WORK                                          
         ICM   R0,15,DMCB          GET LENGTH OF FIELD                          
         BZ    SHPDTERR                                                         
         GOTO1 DATCON,(R1),(0,WORK),(3,SVSHPDT)                                 
         B     VFTRNXT                                                          
*                                                                               
*MNADID                                                                         
VFTR40   EX    R1,VFTRCLCC         THIS A SHIP DATE                             
         BNE   VFTRH                                                            
         CLI   1(R4),0             MUST HAVE AN ENTRY                           
         BE    ADIDERR                                                          
         CLI   1(R4),1             MUST BE BETWEEN 4 AND                        
         BL    ADIDERR                                                          
         CLI   1(R4),12            12 CHARACTERS LONG                           
         BH    ADIDERR                                                          
                                                                                
         MVC   SVADILEN,1(R4)                                                   
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCMLADI(0),22(R4)                                               
         B     VFTRNXT                                                          
*                                                                               
*MNADID                                                                         
VFTRNXT  LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         B     EXIT                                                             
VFTRCLCA CLC   12(0,R4),=C'CML '   COMMERCIAL FILTER                            
VFTRCLCB CLC   12(0,R4),=C'AFFIL ' AFFILIATE FILTER                             
VFTRCLCC CLC   12(0,R4),=C'ADID  ' ADID FILTER                                  
VFTRCLCS CLC   12(0,R4),=C'SDATE ' SHIP DATE                                    
         BE    SHPDTERR                                                         
VFTRH    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'VALID FILTERS ARE CML,AFFIL,SDATE='               
         B     EREXIT2                                                          
AFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'* ERROR * ENTER 3 CHARACTER AFFILIATE *'          
         B     EREXIT2                                                          
*                                                                               
ADIDERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'* ERROR * ENTER A VALID ADID FILTER *'            
         B     EREXIT2                                                          
*                                                                               
SHPDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'* ERROR * SDATE= NEEDS DATE *'                    
*                                                                               
EREXIT2  GOTO1 ERREX2                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*EPTERR  MVI   GCMODE,C'X'         SET TO PURGE REPORT                          
REPTERR  DS   0H                                                                
         MVC   CONHEAD,REPTERRM                                                 
         LA    R2,TRAMEDH                                                       
         B     EREXIT2                                                          
CMLERR   MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
CMLNER   MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
NOSHIPER MVI   ERROR,NOTFOUND                                                   
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         MVC   H5+15(L'STAPRNT),STAPRNT                                         
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   H5+15(L'STANET),STANET                                           
*                                                                               
         LA    R5,H3+40                                                         
         OC    CMLFTR,CMLFTR       FILTER ON COMMERCIAL                         
         BZ    HDHK10               NO                                          
         MVC   0(4,R5),=C'CML='                                                 
         MVC   4(8,R5),CMLFTR                                                   
         LA    R5,132(,R5)                                                      
*                                                                               
HDHK10   OC    AFFFTR,AFFFTR       FILTER ON AFFILIATE                          
         BZ    EXIT                 NO                                          
         MVC   0(4,R5),=C'AFF='                                                 
         MVC   4(3,R5),AFFFTR                                                   
         B     EXIT                                                             
         EJECT                                                                  
REPTERRM DC    CL60'* ERROR * REPORT TOO BIG FOR NOW, RESUBMIT *'               
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,34,C'S H I P P I N G   R E C A P'                             
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,34,C'---------------------------'                             
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H5,3,C'STATION'                                                  
         SSPEC H8,3,C'COMMERCIAL    PARTNER     '                               
         SSPEC H9,3,C'  -ID-         -ID-'                                      
         SSPEC H10,3,C'-------------------------'                               
         SSPEC H9,33,C'TITLE'                                                   
         SSPEC H10,33,C'--------------------'                                   
         SSPEC H9,56,C'TYPE'                                                    
         SSPEC H10,56,C'----'                                                   
         SSPEC H8,64,C'DATE'                                                    
         SSPEC H9,62,C'SHIPPED'                                                 
         SSPEC H10,62,C'--------'                                               
         SSPEC H8,74,C'LAST'                                                    
         SSPEC H9,72,C'TELECAST'                                                
         SSPEC H10,72,C'--------'                                               
         SSPEC H8,86,C'MAN'                                                     
         SSPEC H9,85,C'SHIP'                                                    
         SSPEC H10,85,C'----'                                                   
         SSPEC H8,97,C'NO'                                                      
         SSPEC H9,96,C'SHIP'                                                    
         SSPEC H10,96,C'----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STAMASD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD5D                                                       
* INCLUDED DSECTS                                                               
* INCLUDE DDCOMFACS                                                             
* INCLUDE FAFACTS                                                               
* INCLUDE SPTRAWORKD                                                            
*        PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
ELEMCT   DS    F                                                                
*                                                                               
* SAVE AREA FOR OFFLINE RECAP                                                   
         DS    0F                                                               
VTRPACK  DS    A                                                                
DMDSKSV  DS    F                                                                
*                                                                               
SVTSPROF DS    CL16                                                             
SVTSPRO1 EQU   SVTSPROF+0          USE GENERIC STATION FOR MARKET               
SVTSPRO2 EQU   SVTSPROF+1          PAGE BREAK BY STATION AFFILIATE              
SVTSPRO3 EQU   SVTSPROF+2          FORCE 1 COMMERCIAL PER LIST                  
SVTSPRO4 EQU   SVTSPROF+3          SUPPRESS COMMERCIAL TYPE                     
SVTSPRO5 EQU   SVTSPROF+4          SUPPRESS COMMERCIAL COUNTS                   
*                                                                               
CMLIDSV  DS    CL8                                                              
CMLIDSV2 DS    CL8                                                              
PIGYSW   DS    CL1                                                              
TYPESW   DS    CL1                                                              
SHPSEQSV DS    CL3                                                              
SVKEY2   DS    CL13                                                             
SVMKTSTA DS    XL5                                                              
SVSTAFF  DS    XL3                                                              
SVSTCMLT DS    CL4                 SAVED COMML TYPE FROM STA ADDR REC           
*                                                                               
FILTERS  DS    0CL48                                                            
CMLFTR   DS    CL8                                                              
CMLFTRLN DS    XL1                                                              
AFFFTR   DS    CL3                                                              
SVSHPDT  DS    XL3                                                              
*                                                                               
SVADILEN DS    XL1                                                              
SVCMLADI DS    CL12                                                             
SVADIWRK DS    CL12                                                             
SVCMLADP DS    XL8                                                              
FLTRLEN  EQU   *-FILTERS                                                        
*                                                                               
* DSECT FOR SPOOL REPORT *                                                      
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PCML     DS    CL12    3                                                        
PCMLD    DS    CL1    15                                                        
PCMLP    DS    CL12   16                                                        
         DS    CL5    28                                                        
PCMLTLE  DS    CL20                                                             
         DS    CL3                                                              
PCMLTYP  DS    CL3                                                              
         DS    CL3                                                              
PSHPDTE  DS    CL8                                                              
         DS    CL2                                                              
PLSTTEL  DS    CL8                                                              
         DS    CL2                                                              
PMANSHP  DS    CL2                                                              
         DS    CL3                                                              
PNOSHP   DS    CL2                                                              
*                                                                               
* DSECT FOR LISTAR ONLINE LIST *                                                
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
RMKT     DS    CL4                                                              
         DS    CL2                                                              
RSTA     DS    CL7                                                              
         DS    CL2                                                              
RCMLIDA  DS    CL12                                                             
RCMLID   DS    C                                                                
RCMLIDB  DS    CL12                                                             
         DS    CL1                                                              
RSHPDTE  DS    CL8                                                              
         DS    CL2                                                              
RSHPLTD  DS    CL8                                                              
         DS    CL4                                                              
RSHPMS   DS    CL1                                                              
         DS    CL6                                                              
RSHPNS   DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPTRA45   08/13/10'                                      
         END                                                                    
