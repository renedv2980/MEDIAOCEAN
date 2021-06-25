*          DATA SET SPSFM4F    AT LEVEL 018 AS OF 04/26/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T2174FA  <=====                                                          
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174F  -- MARKET GROUP DEFINITION                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS OFFICE RECORDS ON SPFILE                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMCF, AND SPSFMDF                         *         
*                                                                     *         
*  OUTPUTS:                                                           *         
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
         TITLE 'T2174F - MARKET GROUP DEFINITION'                               
T2174F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174F**,R7,RR=R3                                              
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
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (LIST)                           
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,RECDEL         DELETE A RECORD?                             
         BE    DEL                 YES - MEDIA N/C NOT ALLOWED                  
         CLI   MODE,RECREST        RESTORE A RECORD?                            
         BE    DEL                 YES - MEDIA N/C NOT ALLOWED                  
         CLI   MODE,XRECDEL        JUST DELETED A MGRDEF?                       
         BE    XDEL                YES - DELETE MEDIA N/C MGRDEF RECS           
         CLI   MODE,XRECREST       JUST RESTORED A MGRDEF?                      
         BE    XRES                YES - RESTORE CANADIAN N/C MGROUPS           
         CLI   MODE,XRECADD                                                     
         BE    XR                  AFTER A RECORD IS ADDED                      
         CLI   MODE,XRECPUT                                                     
         BE    XR                  AFTER A RECORD IS CHANGED                    
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R5,MYSPECS                                                       
         ST    R5,SPECS                                                         
         B     LR                  PRINT REPORT BASE ON LISTED RECORDS          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         MVC   CLIENT,SPACES                                                    
         MVC   MKTGID,SPACES                                                    
         XC    BAGY,BAGY           BINARY AGENCY ONLY                           
         XC    BAGYHIGH,BAGYHIGH   NEEDED FOR CK SAME AGNECY                    
         XC    LKEYAGMD,LKEYAGMD   PART OF KEY FOR LIST FILTERING               
         XC    LKEYBCLT,LKEYBCLT   "                                            
         XC    LKEYMID,LKEYMID     "                                            
         XC    HMKTGID,HMKTGID     "                                            
         MVI   LKEYFLAG,0          INITIALIZE FLAG FOR LIST KEY                 
*        MVI   JUSTSEL,0                                                        
*                                                                               
VK05     LA    R2,MDMMEDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   ERRINV                                                           
         XC    TEMPFLD,TEMPFLD     FOR BLANK MEDIA                              
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   BAGY,BAGYMD                                                      
         NI    BAGY,X'F0'          TURN OFF MEDIA PORTION                       
         MVC   LKEYAGMD,BAGY                                                    
         MVC   BAGYHIGH,BAGYMD                                                  
         OI    BAGYHIGH,X'0F'                                                   
         B     BLKEY                                                            
*                                                                               
VK10     DS    0X                                                               
         GOTO1 VALIMED             FILLS IN BAGYMDY, MEDNM                      
         MVC   MDMMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    MDMMDNH+6,X'80'                                                  
         MVC   BAGYHIGH,BAGYMD                                                  
         OI    BAGYHIGH,X'0F'                                                   
         MVC   LKEYAGMD,BAGYMD     BINARY AGENCY-MEDIA                          
         OI    LKEYFLAG,X'80'      HAS AGENCY-MEDIA IN IT                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    BLKEY               MEDIA FOR LIST KEY IS DONE                   
*                                                                               
         LA    R2,MDMMIDH          VALIDATE MKTGRP ID                           
         CLI   8(R2),C' '          SPACE (X'40')?                               
         BNH   ERRMIS                                                           
         TM    4(R2),X'04'         INPUT FIELD IS VALID ALPHA?                  
         BNO   ERRINV                                                           
*                                                                               
*        MVC   MKTGID(1),8(R2)                                                  
         CLI   5(R2),1             TEST 1 CHAR INPUT                            
         BH    *+8                                                              
         OI    9(R2),X'40'         SPACE PAD FOR CLC WITH SPMRGTAB              
*                                                                               
* LOOK UP ONE CHAR EQUIVALENT IN TABLE                                          
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VK12     CLC   8(2,R2),0(RE)                                                    
         BE    VK14                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,VK12                                                          
*                                                                               
         B     ERRMGID                                                          
*                                                                               
VK14     MVC   MKTGID,2(RE)        MOVE HEX VALUE FROM TABLE                    
*                                                                               
VK20     LA    R2,MDMCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0             CLIENT INPUT DATA LENGTH = 0?                
         BE    ERRMIS                                                           
*                                                                               
         CLC   =C'ALL',8(R2)       CLIENT INPUT = 'ALL' ?                       
         BNE   VK30                NO, SKIP AHEAD                               
         XC    MDMCLN,MDMCLN       NO CLIENT NAME                               
         OI    MDMCLNH+6,X'80'                                                  
         CLI   MKTGID,C' '         01-3F FOR ALL CLIENT GROUPS                  
         BL    *+12                                                             
         CLI   MKTGID,C'G'         CLT 'ALL', MGRID LESS THAN 'G'?              
         BL    ERRINV              ONLY INPUT 'ALL' IF MGRID > G                
         MVC   CLIENT,8(R2)                                                     
         XC    BCLT,BCLT                                                        
         B     VK40                                                             
*                                                                               
VK30     CLI   MKTGID,C'F'                                                      
         BH    ERRINV              ONLY A-F IF CLIENT INPUT                     
         CLI   MKTGID,C' '                                                      
         BL    ERRINV                                                           
*                                                                               
         GOTO1 VALICLT             FILL IN BCLT                                 
         MVC   MDMCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MDMCLNH+6,X'80'                                                  
*                                  EXCEPTION FOR CLIENT POL?                    
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
VK40     DS    0X                                                               
         LA    R4,KEY              BUILD MARKET GROUP KEY                       
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD     AGENCY-MEDIA                                 
         MVC   MKGKCLT,BCLT        BINARY CLIENT CODE                           
         MVC   MKGKMID,MKTGID                                                   
         DROP  R4                  KEY FOR MAINTENACE ACTION IS BUILT           
         B     VK50                                                             
*                                                                               
BLKEY    DS    0H                                                               
         CLI   MDMMEDH+5,0                                                      
         BNE   *+14                                                             
         XC    MDMMDN,MDMMDN       CLEAR MEDIA NAME                             
         OI    MDMMDNH+6,X'80'                                                  
         CLI   MDMCLTH+5,0                                                      
         BE    *+14                                                             
         CLC   =C'ALL',MDMCLT                                                   
         BNE   *+14                                                             
         XC    MDMCLN,MDMCLN       CLEAR CLIENT NAME                            
         OI    MDMCLNH+6,X'80'                                                  
         LA    R2,MDMCLTH                                                       
         CLI   5(R2),0                                                          
         BE    BLKEY10                                                          
         CLC   =C'ALL',MDMCLT                                                   
         BNE   *+12                                                             
         OI    LKEYFLAG,X'40'      HAS CLIENT IN IT (ALL)                       
         B     BLKEY10                                                          
*                                                                               
         CLI   MDMMEDH+5,0                                                      
         BNE   BLKEY05                                                          
         XC    TEMPFLD,TEMPFLD     FOR BLANK MEDIA                              
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
BLKEY05  LA    R2,MDMCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   LKEYBCLT,BCLT       GET THE BINARY CLIENT                        
         OI    LKEYFLAG,X'40'      HAS CLIENT IN IT                             
         MVC   MDMCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MDMCLNH+6,X'80'                                                  
*                                                                               
BLKEY10  CLI   MDMMIDH+5,0                                                      
         BE    BLKEY20                                                          
         LA    R2,MDMMIDH          VALIDATE MARKET ID                           
         CLI   8(R2),C'N'                                                       
         BE    ERRINV                                                           
         TM    4(R2),X'04'         INPUT FIELD IS VALID ALPHA?                  
         BNO   ERRINV                                                           
         OI    LKEYFLAG,X'20'      HAS MKT GRP ID IN IT                         
         MVC   LKEYMID,8(R2)       TAKE THE MKT GRP ID                          
         MVC   HMKTGID,SPACES                                                   
         MVC   HMKTGID(1),8(R2)    HEX VALUE FOR FILTERING                      
         CLI   5(R2),1             TEST 1 CHAR INPUT                            
         BE    BLKEY20                                                          
*                                                                               
* LOOK UP ONE CHAR EQUIVALENT IN TABLE                                          
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
BLKEY12  CLC   8(2,R2),0(RE)                                                    
         BE    BLKEY14                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,BLKEY12                                                       
*                                                                               
         B     ERRMGID                                                          
*                                                                               
BLKEY14  MVC   LKEYMID,2(RE)       MOVE HEX VALUE FROM TABLE                    
         MVC   HMKTGID,0(RE)       HEX VALUE FOR FILTERING                      
*                                                                               
BLKEY20  LA    R4,KEY                                                           
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,LKEYAGMD                                                
         MVC   MKGKCLT,LKEYBCLT                                                 
         MVI   MKGKMID,0           JUST READ ALL MKTGRPID                       
         DROP  R4                  KEY FOR LIST ACTION IS BUILT                 
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
VK50     CLC   SAVEKEY,KEY         HAS CHANGED SINCE LAST TIME?                 
         BE    *+8                 NO                                           
         OI    STATUS,KEYCHG       YES                                          
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR05                NO                                           
         LA    R2,MDMMEDH                                                       
         CLI   MDMMED,C'N'         "N" MEDIA RECORDS?                           
         BE    ERRNOCHA                                                         
         CLI   MDMMED,C'C'         "C" MEDIA RECORDS?                           
         BE    ERRNOCHA                                                         
*                                                                               
VR05     MVI   ELCODE,X'01'        REMOVE MKTGRP BREAK DESP ELEMENT             
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEM                                                          
         USING MKGEL01,R4          MAPPING ONTO MKTGRP BREAK DESP ELEM          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'01'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,48           ELEMENT LENGTH (ALSO NO SYMBOL)              
         XC    SUMBKLN,SUMBKLN     SUM OF BREAK LENGTHS IS CLEARED              
*                                                                               
         LA    R2,MDMBK1H          BREAK 1 HEADER                               
         CLI   MDMBK1H+5,0         IS BREAK 1 ZERO LENGTH?                      
         BE    ERRMIS              AT LEAST ONE BREAK IS REQUIRED               
         LA    R2,MDMLN1H          LENGTH 1 HEADER                              
*                                                                               
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
*                                                                               
         MVC   MKGBK1,MDMBK1       BUILDING MKT BREAK NAME                      
         MVC   MKGBK1LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
         LA    R2,MDMLN2H          LENGTH 2 HEADER                              
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         LA    R2,MDMBK2H                                                       
         BAS   RE,NOBKCHA          CAN'T ADD A NEW BK TITLE                     
*                                                                               
         LA    R2,MDMBK2H                                                       
         CLI   MDMBK2H+5,0         BREAK AND LENGTH COME IN PAIR                
         BNE   CKBKLN2             IF BREAK CONTAINS INPUT, THEN                
         CLI   MDMLN2H+5,0         LENGTH MUST CONTAIN INPUT.                   
         BNE   ERRMIS              THE REVERSE IS ALSO TRUE                     
*                                                                               
         XC    MDMBK2,MDMBK2       NOTHING IS IN BREAK AND LENGTH               
         XC    SVBKLNB,SVBKLNB                                                  
         MVC   MKGBK2,MDMBK2       BUILDING MKT BREAK NAME                      
         MVC   MKGBK2LN,SVBKLNB    BUILDING BINARY BK LN                        
         B     DOBKLN3             DO THE 3RD BREAK LENGTH                      
*                                                                               
CKBKLN2  LA    R2,MDMLN2H          LENGTH 2 HEADER                              
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
         MVC   MKGBK2,MDMBK2       BUILDING MKT BREAK NAME                      
         MVC   MKGBK2LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
DOBKLN3  LA    R2,MDMLN3H          LENGTH 3 HEADER                              
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         LA    R2,MDMBK3H                                                       
         BAS   RE,NOBKCHA          CAN'T ADD A NEW BK TITLE                     
*                                                                               
         LA    R2,MDMBK3H                                                       
         CLI   MDMBK3H+5,0         BREAK AND LENGTH COME IN PAIR                
         BNE   CKBKLN3             IF BREAK CONTAINS INPUT, THEN                
         CLI   MDMLN3H+5,0         LENGTH MUST CONTAIN INPUT.                   
         BNE   ERRMIS              THE REVERSE IS ALSO TRUE                     
*                                                                               
         XC    MDMBK3,MDMBK3       NOTHING IS IN BREAK AND LENGTH               
         XC    SVBKLNB,SVBKLNB                                                  
         MVC   MKGBK3,MDMBK3       BUILDING MKT BREAK NAME                      
         MVC   MKGBK3LN,SVBKLNB    BUILDING BINARY BK LN                        
         B     CKORDER                                                          
*                                                                               
CKBKLN3  LA    R2,MDMLN3H          LENGTH 3 HEADER                              
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
         MVC   MKGBK3,MDMBK3       BUILDING MKT BREAK NAME                      
         MVC   MKGBK3LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
CKORDER  CLI   MKGBK3LN,0          NO BLANK INPUTS BETWEEN BK LN                
         BE    DOPGA                                                            
         CLI   MKGBK2LN,0                                                       
         BNE   DOPGA                                                            
         LA    R2,MDMBK2H                                                       
         B     ERRMIS              CANNOT HAVE LENGTH 3 W/O LENGTH 2            
*                                                                               
DOPGA    LA    R2,MDMPGAH          PRODUCT GROUP ASSIGNMENT                     
         MVI   MKGPGA,0            SET FOR NO PRD GRP ASSIGNMENT                
         CLI   MDMPGAH+5,0                                                      
         BE    DOALLMKT            NO INPUT MEANS 'N'                           
         CLI   MDMPGA,C'N'                                                      
         BE    DOALLMKT                                                         
         CLI   MKTGID,C' '         01-3F ARE ALL CLIENT GROUPS                  
         BL    *+12                                                             
         CLI   MKTGID,C'F'         SO ARE ALL GROUPS > F                        
         BH    ERRINV                                                           
         CLI   MDMPGA,C'Y'                                                      
         BNE   ERRINV                                                           
         MVC   MKGPGA,MDMPGA       BUILDING PRD GRP ASSIGNMENT                  
*                                                                               
DOALLMKT LA    R2,MDMAMKH                                                       
         MVI   MKGALLM,0                                                        
         CLI   MDMAMKH+5,0                                                      
         BE    VR50                NO INPUT MEANS 'N'                           
         CLI   MDMAMK,C'N'                                                      
         BE    VR50                INPUT 'N' IS OKAY                            
         CLI   MDMAMK,C'Y'                                                      
         BNE   ERRINV                                                           
         MVC   MKGALLM,MDMAMK      BUILDING 'ALL MARKETS'                       
         DROP  R4                                                               
*                                                                               
VR50     GOTO1 ADDELEM             ELEMENT READY TO BE ADDED                    
*                                                                               
         LA    R2,MDMAEXH          ADD EXCEPTION HEADER                         
         CLI   MDMAEXH+5,0                                                      
         BE    VR60                NO EXCEPTION NEED TO BE ADDED                
         CLI   MKTGID,C' '         01-3F ARE ALL CLIENT GROUPS                  
         BL    VR55                                                             
         CLI   MKTGID,C'F'         MKT GRP ID > F??                             
         BH    VR55                                                             
*                                                                               
         BAS   RE,ADDPRDEX         APPLIES TO MKT GRP ID A-F                    
         B     VR60                                                             
*                                                                               
VR55     BAS   RE,ADDCLTEX         APPLIES TO MKT GRP ID NOT A-F                
*                                                                               
VR60     LA    R2,MDMBTKH          FOR BUYTRACKER FIELD HEADER                  
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    VR65                NONE, CLEAR FLAG IN THE ELEM                 
         CLI   8(R2),C'N'                                                       
         BE    VR65                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV              YES, NO, OR NOTHING                          
VR65     L     R1,AIO              BECAUSE X'01' ELEM ADDED TO AIO              
         LA    R1,MKGEL-MKGKEY(R1) WE HAVE TO JUST MODIFY DATA IN IT            
         CLI   0(R1),X'01'         MKTGRP BREAK DESC ELEM?                      
         BE    *+6                                                              
         DC    H'0'                IT BETTER BE                                 
         MVC   MKGBKBTK-MKGEL01(1,R1),8(R2)                                     
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       XRECPUT/XRECADD                               *         
***********************************************************************         
XR       DS    0X                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   XRX                                                              
         CLI   QMED,C'T'           MEDIA IS "T"?                                
         BNE   XRX                                                              
*                                                                               
XR10     XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'         MAKING MEDIA, IN KEY, INTO "C"               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR20                NO "C" REC THAT CORRESPONDS TO "T"           
*                                                                               
         MVC   AIO,AIO2            LEAVING AIO1 ALONE                           
         GOTO1 GETREC              FOR PUTREC PURPOSE                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         L     R4,AIO2                                                          
         MVC   0(200,R4),0(R6)     COPY EVERYTHING IN AIO1 TO AIO2              
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'         MAKING MEDIA, IN REC, INTO "C"               
         GOTO1 PUTREC              AUTO UPDATE OF "C" REC FROM "T"              
         B     XR30                                                             
*                                                                               
XR20     L     R6,AIO1             NO "C" REC FOUND, SO ADD IT                  
         NI    2(R6),X'F0'                                                      
         OI    2(R6),X'08'         MAKING MEDIA, IN REC, INTO "C"               
         GOTO1 ADDREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
XR30     MVC   AIO,AIO1            NOW CHEKCING FOR "N" REC                     
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'         MAKING MEDIA, IN KEY, INTO "N"               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR40                NO "N" REC THAT CORRESPONDS TO "T"           
*                                                                               
         MVC   AIO,AIO2            LEAVING AIO1 ALONE                           
         GOTO1 GETREC              FOR PUTREC PURPOSE                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         L     R4,AIO2                                                          
         MVC   0(200,R4),0(R6)     COPY EVERYTHING IN AIO1 TO AIO2              
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'03'         MAKING MEDIA, IN REC, INTO "N"               
         GOTO1 PUTREC              AUTO UPDATE OF "N" REC FROM "T"              
         B     XRX                                                              
*                                                                               
XR40     L     R6,AIO1             NO "N" REC FOUND, SO ADD IT                  
         NI    2(R6),X'F0'                                                      
         OI    2(R6),X'03'         MAKING MEDIA, IN REC, INTO "N"               
         GOTO1 ADDREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
XRX      MVC   AIO,AIO1                                                         
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING MKGEL01,R6                                                       
         MVC   MDMBK1,MKGBK1       DISPLAY BREAK TITLE1                         
         OI    MDMBK1H+6,X'80'                                                  
         MVC   MDMBK2,MKGBK2       DISPLAY BREAK TITLE2                         
         OI    MDMBK2H+6,X'80'                                                  
         MVC   MDMBK3,MKGBK3       DISPLAY BREAK TITLE3                         
         OI    MDMBK3H+6,X'80'                                                  
*                                                                               
         MVC   MDMBTK,MKGBKBTK     DISPLAY FOR BUYTRACKER                       
         OI    MDMBTKH+6,X'80'                                                  
*                                                                               
         EDIT  MKGBK1LN,MDMLN1     DISPLAY BREAK LENGTH1                        
         OI    MDMLN1H+6,X'80'                                                  
         OI    MDMLN1H+4,X'20'     VALID'D BEFORE, FOR ACTCHA                   
         EDIT  MKGBK2LN,MDMLN2     DISPLAY BREAK LENGTH2                        
         OI    MDMLN2H+6,X'80'                                                  
         OI    MDMLN2H+4,X'20'                                                  
         EDIT  MKGBK3LN,MDMLN3     DISPLAY BREAK LENGTH3                        
         OI    MDMLN3H+6,X'80'                                                  
         OI    MDMLN3H+4,X'20'                                                  
*                                                                               
         MVI   MDMPGA,C'N'                                                      
         CLI   MKGPGA,0            NO DATA, PRINT 'N'                           
         BE    *+10                                                             
         MVC   MDMPGA,MKGPGA                                                    
         OI    MDMPGAH+6,X'80'                                                  
*                                                                               
         MVI   MDMAMK,C'N'                                                      
         CLI   MKGALLM,0           NO DATA, PRINT 'N'                           
         BE    *+10                                                             
         MVC   MDMAMK,MKGALLM                                                   
         OI    MDMAMKH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         CLI   MKTGID,C' '         01-3F ARE ALL CLIENT SCHEMES                 
         BL    DR2                                                              
         CLI   MKTGID,C'F'         CLIENT OR PRODUCT EXCEPTION?                 
         BH    DR2                 G-K ARE CLIENT EXCEPTIONS                    
*                                                                               
         BAS   RE,PRDEXCP          PRODUCT EXCEPTIONS                           
         B     DR10                                                             
*                                                                               
DR2      BAS   RE,CLTEXCP          CLIENT EXCEPTIONS                            
*                                                                               
DR10     CLI   ACTNUM,ACTCHA       SAVED ACTION = CHANGE?                       
         BE    DR20                                                             
         CLI   ACTNUM,ACTSEL       SAVED ACTION = SEL?                          
         BNE   DRX                                                              
         CLI   LISTSEL,C'C'        SEL ACTION = CHANGE?                         
         BNE   DR20                                                             
*                                                                               
DR20     MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         NI    STATUS,X'FF'-KEYCHG ALWAYS TURN OFF KEYCHG BIT                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         MVI   JUSTSEL,1                                                        
         L     R6,AIO                                                           
         USING MKGRECD,R6                                                       
*                                                                               
         CLI    THISLSEL,C'C'                                                   
         BNE    DK05                                                            
         CLI    T217FFD+1,C'*'      TEST DDS TERM                               
         BE     DK05                                                            
         TM     T217FFD+12,X'01'                                                
         BO     ERRSEC2             ON = NO CHANGE                              
*                                                                               
DK05     MVC   MKTGID,MKGKMID      COPY OF MKT GRP ID                           
         MVC   WKAGMD,MKGKAGMD                                                  
         NI    WKAGMD,X'0F'        TURN OFF THE AGENCY PORTION                  
         LA    R4,MEDTAB                                                        
*                                                                               
DK10     CLC   WKAGMD,1(R4)        COMPARE THE MEDIA CODE                       
         BE    DK20                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DK10                                                             
         B     ERRINV                                                           
*                                                                               
DK20     MVC   LSTMED,0(R4)        1 CHAR MEDIA                                 
         OI    LSTMEDH+6,X'80'                                                  
         LA    R2,LSTMEDH                                                       
         MVI   LSTMEDH+5,1         SKIP VALIMED                                 
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   LSTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    LSTMDNH+6,X'80'                                                  
*                                                                               
         CLC   MKGKCLT,=X'0000'                                                 
         BNE   DK30                                                             
         MVC   LSTCLT,=C'ALL'      DISPLAY "ALL"                                
         OI    LSTCLTH+6,X'80'                                                  
         XC    LSTCLN,LSTCLN       THERE'S NO CLIENT NAME                       
         OI    LSTCLNH+6,X'80'                                                  
         XC    BCLT,BCLT                                                        
         B     DK40                                                             
*                                                                               
DK30     GOTO1 CLUNPK,DMCB,MKGKCLT,CLIENT                                       
         MVC   LSTCLT,CLIENT       3 CHARACTER CLIENT                           
         OI    LSTCLTH+6,X'80'                                                  
         LA    R2,LSTCLTH                                                       
         MVI   LSTCLTH+5,3         FAKE THE VALICLT                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO1                                                         
         MVC   LSTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    LSTCLNH+6,X'80'                                                  
*                                                                               
DK40     L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
DK42     CLC   MKGKMID,2(RE)                                                    
         BE    DK44                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,DK42                                                          
*                                                                               
         DC    H'0'                HOW DID AN INVALID MKTGRPID GET HERE         
*                                                                               
DK44     MVC   LSTMID(2),0(RE)     1/2 CHAR MKTGRP ID                           
         OI    LSTMIDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         MVC   LISTSEL,THISLSEL    THE SELECTED ACTION ON LIST                  
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   DKX                                                              
         CLI   LISTSEL,C'C'        SEL ACTION = CHANGE?                         
         BE    *+12                                                             
         CLI   LISTSEL,C'A'        SEL ACTION = ADD?                            
         BNE   DKX                                                              
         CLI   MDMMED,C'N'         "N" MEDIA RECORDS?                           
         BE    ERRNOCHA                                                         
         CLI   MDMMED,C'C'         "C" MEDIA RECORDS?                           
         BE    ERRNOCHA                                                         
*                                                                               
DKX      DS    0H                                                               
         LA    R4,KEY              BUILD MARKET GROUP KEY                       
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD     AGENCY-MEDIA                                 
         MVC   MKGKCLT,BCLT        BINARY CLIENT CODE                           
         MVC   MKGKMID,MKTGID                                                   
         DROP  R4                  KEY FOR DISP REC IS BUILT                    
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ONLINE RECORD LISTING                         *         
***********************************************************************         
LR       DS    0H                                                               
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR44                NO, CONTINUE LISTING                         
*                                                                               
*        OI    GENSTAT2,RETEQSEL+DISTHSPG                                       
         XC    NUMRECS,NUMRECS     NUMBER OF RECORDS TO SORT                    
         XC    NEXTREC,NEXTREC     INDEX INTO SORTED RECS                       
         LA    R5,SORTRECS         IO AREA TO DO SORT (MAX 250 RECS)            
         USING SORTMGRD,R5                                                      
*                                                                               
         LA    R1,KEY                                                           
         USING MKGRECD,R1                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R1,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),KEYSAVE      SAME SYSTEM/PROGRAM?                         
         BNE   LR40                NOPE, DONE                                   
*                                                                               
         TM    LKEYFLAG,X'80'      MEDIA FILTER?                                
         BO    LR30A               YES, WANT EXACT MEDIA                        
         CLC   BAGYHIGH,KEY+2      SAME AGENCY?                                 
         BL    LR40                NO, DONE                                     
         B     LR30B                                                            
*                                                                               
LR30A    CLC   BAGYMD,KEY+2        SAME A/M?                                    
         BNE   LR40                NOPE, DONE                                   
*                                                                               
LR30B    CLI   KEY+8,0             HAVE MARKET GROUP ID?                        
         BE    LR20                NO, READ NEXT                                
*                                                                               
         TM    LKEYFLAG,X'40'      CLIENT FILTER?                               
         BNO   LR30C               NO                                           
         CLC   KEY+3(2),LKEYBCLT   YES, SAME CLIENT?                            
         BNE   LR20                NO, READ NEXT                                
*                                                                               
LR30C    TM    LKEYFLAG,X'20'      MARKET GROUP ID FILTER?                      
         BNO   LR35                NOPE                                         
***                                                                             
* HERE WE GO A BIG MESS...                                                      
***                                                                             
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
LR31     CLC   KEY+8(1),2(RE)                                                   
         BE    LR32                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,LR31                                                          
*                                                                               
         DC    H'0'                HOW DID THIS GET INTO A KEY???               
*                                                                               
LR32     CLI   HMKTGID+1,X'40'     DID WE WANT TO FILT ON 2 BYTE MGRPID         
         BNE   LR33                YES                                          
         CLI   1(RE),X'40'         IS CURRENT REC WE HAVE 2 BYTE MGRPID         
         BNE   LR33                YES                                          
         CLC   2(1,RE),LKEYMID     COMPARE 2 REGULAR MGRPID                     
         BL    LR20                                                             
         B     LR35                                                             
*                                                                               
LR33     CLC   0(2,RE),HMKTGID     LESS THAN WHAT WERE FILTERING FOR?           
         BL    LR20                YES, READ NEXT REC                           
*                                                                               
LR35     MVC   SRTDA,KEY+14        SAVE THE D/A FOR QSORT                       
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         BAS   RE,GETEL                                                         
         BNE   LR20                NOT FOUND, DON'T WANT THIS REC               
*                                                                               
         L     R6,AIO                                                           
         CLI   MKGKMID,0           ANY MARKET GROUP NUM?                        
         BE    LR20                NO...IGNORE THIS ONE                         
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         CLC   2(1,RE),MKGKMID                                                  
         BE    *+14                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   SRTMGRID,0(RE)      MARKET GROUP ID FOR SORTER                   
         MVC   WKAGMD,KEY+2                                                     
         NI    WKAGMD,X'0F'        TURN OFF THE AGENCY PORTION                  
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
LR36     CLC   WKAGMD,1(R4)        COMPARE THE MEDIA CODE                       
         BE    LR37                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   LR36                                                             
         B     ERRINV                                                           
*                                                                               
LR37     MVC   SRTMED,1(R4)        MEDIA FOR QSORT                              
         MVC   SRTCLT,KEY+3        CLIENT FOR QSORT                             
*                                                                               
         L     R2,NUMRECS                                                       
         AHI   R2,1                ADD 1 TO REC COUNTER                         
         ST    R2,NUMRECS                                                       
         CHI   R2,250              CAN ONLY HANDLE SORTING 250 RECS             
         BNH   LR38                                                             
         LA    R2,LSTSELH          CURSOR ON SELECT FIELD                       
         B     ERRRECS             GIVE ERROR                                   
*                                                                               
LR38     LA    R5,SRTRECLQ(R5)     ADD NEXT REC HERE                            
         B     LR20                                                             
***                                                                             
* ALL RECS ARE IN SORTRECS                                                      
***                                                                             
LR40     L     R3,NUMRECS                                                       
         CHI   R3,0                DID WE GET ANY RECORDS?                      
         BE    LRX                 NO                                           
*                                                                               
         GOTO1 QSORT,DMCB,SORTRECS,(R3),SRTRECLQ,L'SORTON,0                     
         LA    R5,SORTRECS                                                      
         B     LR45                                                             
*                                                                               
LR44     CLI   JUSTSEL,1           JUST CAME FROM SELECT?                       
         BNE   LR44A               NOPE                                         
         MVI   JUSTSEL,0           YUP, NOW ITS TIME TO RE-LIST THEM            
         MVC   NEXTREC,LASTREC                                                  
         MVC   NUMRECS,LASTNUM                                                  
*                                                                               
LR44A    LA    R5,SORTRECS         INDEX INTO SORTRECS                          
         L     R6,NEXTREC                                                       
         LA    R5,0(R5,R6)                                                      
         L     R3,NUMRECS          NUMBER OF RECS LEFT +1                       
         BCTR  R3,0                                                             
         CHI   R3,0                ARE WE DONE?                                 
         BE    LRX                 FRAID SO                                     
*                                                                               
LR45     MVC   LASTREC,NEXTREC     IN CASE WE HAVE TO RE-LIST THEM              
         MVC   LASTNUM,NUMRECS     IN CASE WE HAVE TO RE-LIST THEM              
*                                                                               
LR45A    XC    KEY,KEY                                                          
         MVC   KEY+14(4),5(R5)     MOVE IN JUST THE D/A                         
         GOTO1 GETREC                                                           
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)       GET THE KEY FROM THE RECORD                  
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LSMID,3(R5)         MARKET GROUP ID                              
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
LR50     CLC   0(1,R5),1(R4)       COMPARE THE MEDIA CODE                       
         BE    LR51                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   LR50                                                             
         DC    H'0'                ALREADY CHECKED! DID QSORT FUCKUP?           
*                                                                               
LR51     MVC   LSMED,0(R4)         1 CHAR MEDIA                                 
         MVC   WKBCLI,1(R5)        CLIENT                                       
         CLC   WKBCLI,=X'0000'     IS IT NULL?                                  
         BNE   *+14                                                             
         MVC   CLIENT,=C'ALL'                                                   
         B     LR60                                                             
         CLC   WKBCLI,SVBCLT       IS IT THE SAME AS PREVIOUS ONE?              
         BE    LR52                YES                                          
         BAS   RE,AAN              CHECK TO SEE IF CLIENT DISPLAY AAN           
         MVC   SVBCLT,WKBCLI                                                    
LR52     CLI   CLTAAN,C'Y'         DISPLAY CLIENT IN FORMAT AAN?                
         BNE   LR55                NOPE                                         
         GOTO1 CLUNPK,DMCB,(C'Y',WKBCLI),CLIENT                                 
         B     LR60                                                             
LR55     GOTO1 CLUNPK,DMCB,WKBCLI,CLIENT                                        
*                                                                               
LR60     MVC   LSCLT,CLIENT        MOVE CLIENT TO THE LIST                      
         MVI   ELCODE,X'01'        MARKET GROUP BREAK DESCRIPTION               
         BAS   RE,GETEL            WE GOT THIS BEFORE                           
         BE    *+6                                                              
         DC    H'0'                WHAT THE HELL HAPPENED??                     
         USING MKGEL01,R6                                                       
         MVC   LSBKT1,MKGBK1       BREAK 1 TITLE                                
         EDIT  MKGBK1LN,LSBKLN1                                                 
         MVC   LSBKT2,MKGBK2       BREAK 2 TITLE                                
         EDIT  MKGBK2LN,LSBKLN2                                                 
         MVC   LSBKT3,MKGBK3       BREAK 3 TITLE                                
         EDIT  MKGBK3LN,LSBKLN3                                                 
*                                                                               
         CLI   MODE,PRINTREP       CK IF ACTION IS PRINT REPORT                 
         BNE   LR70                NO, THUS ACTION IS LIST                      
         XC    TEMPKEY,TEMPKEY                                                  
         MVC   TEMPKEY,KEY         SAVE AWAY KEY FOR RESTORATION                
         MVC   P,SPACES                                                         
         MVI   P1A,C'('                                                         
         MVC   PRTMED,LSMED                                                     
         MVI   P1B,C')'                                                         
         XC    TEMPFLD,TEMPFLD                                                  
         MVI   TEMPFLD,9                                                        
         MVI   TEMPFLD+5,1                                                      
         MVC   TEMPFLD+8(L'LSMED),LSMED                                         
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   PRTMEDN,MEDNM                                                    
         MVI   P2A,C'('                                                         
         MVC   PRTCLT,=C'ALL'                                                   
         MVI   P2B,C')'                                                         
         CLC   LSCLT,=C'ALL'       CLIENT IS "ALL"?                             
         BE    LR65                NO, NO NEED TO GET CLIENT NAME               
         MVC   PRTCLT,LSCLT                                                     
         XC    TEMPFLD,TEMPFLD                                                  
         MVI   TEMPFLD,11                                                       
         MVI   TEMPFLD+5,3                                                      
         MVC   TEMPFLD+8(L'LSCLT),LSCLT                                         
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   PRTCLTN,CLTNM                                                    
LR65     MVC   PRTMID,LSMID                                                     
         MVC   PRTBKT1,LSBKT1                                                   
         MVC   PRTBKLN1,LSBKLN1                                                 
         MVC   PRTBKT2,LSBKT2                                                   
         MVC   PRTBKLN2,LSBKLN2                                                 
         MVC   PRTBKT3,LSBKT3                                                   
         MVC   PRTBKLN3,LSBKLN3                                                 
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         LA    R5,SRTRECLQ(R5)     BUMP TO NEXT REC                             
         B     LR75                                                             
*                                                                               
LR70     LA    R5,SRTRECLQ(R5)     BUMP TO NEXT REC                             
         L     R2,NEXTREC                                                       
         AHI   R2,SRTRECLQ                                                      
         ST    R2,NEXTREC          SAVE INDEX INTO SORTED REC TABLE             
         ST    R3,NUMRECS          SAVE NUMBER OF RECS LEFT                     
         GOTO1 LISTMON                                                          
*                                                                               
         CHI   R3,1                DONE LISTING ALL THE RECS?                   
         BNE   LR75                                                             
         OI    MDMMEDH+4,X'80'     TRICK INPUT TRANSLATOR                       
         OI    MDMMEDH+6,X'80'     AND FORCE A VK                               
*                                                                               
LR75     BCT   R3,LR45A                                                         
*                                                                               
LRX      B     XIT                                                              
         DROP  R1,R5,R6                                                         
         EJECT                                                                  
*                                                                               
DEL      CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   DEL10               NO                                           
         CLI   QMED,C'N'           MEDIA N?                                     
         BE    ERRNODEL            YES - ERROR                                  
         CLI   QMED,C'C'           MEDIA C?                                     
         BE    ERRNODEL            YES - ERROR                                  
*                                                                               
DEL10    MVC   KEY(13),SAVEKEY     RESTORE THE KEY                              
         GOTO1 HIGH                RE-READ MGRDEF REC & SET UP FOR SEQ          
         CLC   KEY(13),KEYSAVE     HAVE THE RIGHT KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH!                                  
*                                                                               
DEL15    GOTO1 SEQ                 READ SEQ                                     
*                                                                               
         TM    KEY+13,X'80'        DELETED?                                     
         BO    DEL15               YES - READ SEQ                               
         CLC   KEY(5),SAVEKEY      X'0D02' A/M CLT?                             
         BNE   DEL20               NO - MGROUP REC DOES NOT EXIST               
         CLC   KEY+8(1),SAVEKEY+8  FOUND A MGROUP REC UNDER THIS ID?            
         BE    ERRDEL              YES - CANNOT DELETE RECORD                   
*                                                                               
DEL20    MVC   KEY(13),SAVEKEY     RESTORE THE KEY                              
         GOTO1 HIGH                RE-READ MGRDEF RECORD                        
         CLC   KEY(13),KEYSAVE     HAVE THE RIGHT KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH!                                  
*                                                                               
         B     XIT                 EXIT                                         
***********************************************************************         
* JUST DELETED A MARKET GROUP RECORD SO...                            *         
* 1) DELETE MEDIA N/C MGROUP RECORD IF CANADIAN TV                    *         
***********************************************************************         
XDEL     CLI   SVAPROF+7,C'C'       CANADIAN AGENCY?                            
         BNE   XDELX                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XDELX                NO                                          
*                                                                               
         MVC   AIO,AIO2             DON'T CLOBBER AIO1!                         
         MVC   THISAM,SAVEKEY+2     A/M                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'03'         START WITH MEDIA N                          
         MVC   KEY(13),SAVEKEY      MARKET GROUP KEY                            
         L     R6,AIO               A(AIO2)                                     
*                                                                               
XDEL05   MVC   KEY+2(1),THISAM      NEW A/M                                     
         GOTO1 HIGH                 READ HIGH                                   
                                                                                
         CLC   KEY(13),KEYSAVE      FOUND MGRDEF FOR NEW A/M?                   
         BNE   XDEL10               NO - NOTHING TO DELETE                      
*                                                                               
         GOTO1 GETREC               GET THE RECORD                              
         OI    15(R6),X'80'         DELETE THE RECORD                           
         GOTO1 PUTREC               WRITE IT BACK                               
         OI    KEY+13,X'80'         DELETE THE KEY                              
         GOTO1 WRITE                WRITE IT BACK                               
*                                                                               
XDEL10   TM    THISAM,X'08'         JUST PROCESSED MEDIA C?                     
         BNZ   XDEL20               YES                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'08'         SET FOR MEDIA C                             
         B     XDEL05               GO PROCESS MEDIA C                          
*                                                                               
XDEL20   MVC   AIO,AIO1             RESTORE AIO1                                
*                                                                               
XDELX    B     XIT                  EXIT                                        
***********************************************************************         
* JUST RESTORED A MARKET GROUP RECORD SO...                           *         
* 1) RESTORE MEDIA N/C MGROUP RECORD IF CANADIAN TV                   *         
***********************************************************************         
XRES     CLI   SVAPROF+7,C'C'       CANADIAN AGENCY?                            
         BNE   XRESX                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XRESX                NO                                          
*                                                                               
         OI    DMINBTS,X'08'        READ DELETED RECORDS                        
         MVC   AIO,AIO2             DON'T CLOBBER AIO1!                         
         MVC   THISAM,SAVEKEY+2     A/M                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'03'         START WITH MEDIA N                          
         MVC   KEY(13),SAVEKEY      MARKET GROUP KEY                            
         L     R6,AIO               A(AIO2)                                     
*                                                                               
XRES05   MVC   KEY+2(1),THISAM      NEW A/M                                     
         GOTO1 HIGH                 READ HIGH                                   
         CLC   KEY(13),KEYSAVE      FOUND MGROUP FOR NEW A/M?                   
         BNE   XRES10               NO - NOTHING TO RESTORE                     
*                                                                               
         GOTO1 GETREC               GET THE RECORD                              
         NI    15(R6),X'FF'-X'80'   RESTORE THE RECORD                          
         GOTO1 PUTREC               WRITE IT BACK                               
         NI    KEY+13,X'FF'-X'80'   RESTORE THE KEY                             
         GOTO1 WRITE                WRITE IT BACK                               
*                                                                               
XRES10   TM    THISAM,X'08'         JUST PROCESSED MEDIA C?                     
         BNZ   XRES20               YES                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'08'         SET FOR MEDIA C                             
         B     XRES05               GO PROCESS MEDIA C                          
*                                                                               
XRES20   MVC   AIO,AIO1             RESTORE AIO1                                
         NI    DMINBTS,X'FF'-X'08'  TURN OFF READ FOR DELETES                   
*                                                                               
XRESX    B     XIT                  EXIT                                        
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRTOTLN MVC   ERRNUM,=AL2(BKLNTOT)                                             
         B     SPERREX                                                          
ERREXIST MVC   ERRNUM,=AL2(EXCPEXIS)                                            
         B     SPERREX                                                          
ERRCHALN MVC   ERRNUM,=AL2(CHABKLN)                                             
         B     SPERREX                                                          
ERRCHABK MVC   ERRNUM,=AL2(CHANUMBK)                                            
         B     SPERREX                                                          
ERREXCEP MVC   ERRNUM,=AL2(MOREEXCP)                                            
         B     SPERREX                                                          
ERRNOCHA MVC   ERRNUM,=AL2(NCNOCHA)                                             
         B     SPERREX                                                          
ERRNODEL MVC   ERRNUM,=AL2(1370)    CAN'T DEL/RES MEDIA N/C RECORDS             
         B     SPERREX                                                          
ERRDEL   MVC   ERRNUM,=AL2(1384)    CAN'T DELETE - MGROUP RECS EXIST!           
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRRECS  MVC   ERRNUM,=AL2(1143)    CANNOT SORT > 250 RECS IN LR                
         B     SPERREX                                                          
ERRMGID  MVC   ERRNUM,=AL2(1144)    INVALID MARKET GROUP ID                     
         B     SPERREX                                                          
*                                                                               
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
*                                  SHORT DESP OF THE ERROR MSGS                 
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
GPOVFL   EQU   443                                                              
CHABKLN  EQU   444                 CAN'T CHANGE BREAK LENGTH                    
CHANUMBK EQU   445                 CAN'T CHANGE # OF BREAK TITLES               
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
BKLNTOT  EQU   450                 SUM OF BREAK LN MUST BE 1-4                  
EXCPEXIS EQU   451                 EXCEPTION ALREADY EXIST                      
MOREEXCP EQU   452                 EXCEPTION CAN'T BE ALL DISPLAYED             
NCNOCHA  EQU   470                 N & C REC (MEDIA) CAN'T BE CHANGED           
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SUBROUTINE TO READ AAN IN CLIENT PROFILE, PASS TO CLUNPK     *         
***********************************************************************         
AAN      NTR1                                                                   
*                                                                               
         XC    SVKEY1,SVKEY1                                                    
         MVC   SVKEY1,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY1+2     A/M                                        
         MVC   KEY+2(2),SVKEY1+3     CLIENT                                     
         GOTO1 HIGH                  READ CLIENT RECORD                         
         CLC   KEYSAVE(13),KEY       FOUND?                                     
         BNE   AAN2                  NO                                         
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         MVC   CLTAAN,CPROF+6        Y = AAN                                    
         DROP  R6                                                               
AAN2     MVC   KEY,SVKEY1            RESTORE KEY                                
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
         GOTO1 GETREC                                                           
AANX     B     XIT                                                              
*                                                                               
***********************************************************************         
*          IF ACTION = CHANGE, BREAK LENGTH CANNOT BE CHANGED         *         
***********************************************************************         
NOLNCHA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    XNOLNCHA                                                         
         TM    4(R2),X'20'         BREAK LENGTH CHANGED?                        
         BNZ   XIT                 NO                                           
*                                                                               
         GOTO1 HIGH                RE-READ MGRDEF REC & SET UP FOR SEQ          
         CLC   KEY(13),KEYSAVE     HAVE THE RIGHT KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH!                                  
*                                                                               
         GOTO1 SEQ                 READ SEQ                                     
         CLC   KEY(9),KEYSAVE      FOUND A MGROUP REC UNDER THIS ID?            
         BE    ERRCHALN            YES - CANNOT CHANGE BREAK LENGTH             
         MVC   KEY(13),SAVEKEY     RESTORE THE KEY                              
         GOTO1 HIGH                RE-READ MGRDEF RECORD                        
         CLC   KEY(13),KEYSAVE     HAVE THE RIGHT KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH!                                  
*                                                                               
XNOLNCHA B     XIT                 END OF CLTEXCEP                              
         SPACE 5                                                                
***********************************************************************         
*          IF ACTION = CHANGE, NO NEW BREAK TITLE CAN BE ADDED.       *         
***********************************************************************         
NOBKCHA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    XNOBKCHA                                                         
         LR    R5,R2               SAVE THE SCREEN FIELD POINTER                
         CLI   5(R2),0             BRAND NEW BREAK TITLE?                       
         BE    XNOBKCHA                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   XNOBKCHA                                                         
         LR    R2,R5               POINT BACK WHERE R2 INITIALLY AT             
         B     ERRCHABK                                                         
*                                                                               
XNOBKCHA B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*                ADDING CLIENT EXCEPTIONS                             *         
***********************************************************************         
ADDCLTEX NTR1                                                                   
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                RESTORING THE ORIGINAL RECORD                
         GOTO1 GETREC              TO CHECK IF THE EXCEPTION ELEM               
         MVC   AIO,AIO1            IS ALREADY THERE.                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BUILTCEL            BUILT CLIENT EXCEPTION ELEMENT               
*                                                                               
MORECEX  CLC   MDMAEX(3),2(R6)     CK IF EXCEPTION ELEM ALREADY EXIST           
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         CLI   MDMAEXH+5,2         IF CLIENT EXCEPTION CODE IS CL2              
         BNE   *+18                                                             
         MVI   MDMAEX+2,C' '       MOVE A SPACE TO THE 3RD BYTE                 
         CLC   MDMAEX(3),2(R6)     CK FOR 2 CHARACTERS CLIENT CODES             
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         BAS   RE,NEXTEL                                                        
         BE    MORECEX             MORE CLIENT EXCEPTION ELEMENT                
*                                                                               
BUILTCEL XC    ELEM,ELEM           START BUILDING EXCEPTION ELEM                
         MVI   ELEM,X'02'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,5            ELEMENT LENGTH (ALSO NO SYMBOL)              
         MVC   ELEM+2(3),MDMAEX    "ADD EXCEPTION", SCREEN FIELD                
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                 END OF ADD CLIENT EXCEPTION                  
         EJECT                                                                  
***********************************************************************         
*                ADDING PRODUCT EXCEPTIONS                            *         
***********************************************************************         
ADDPRDEX NTR1                                                                   
         LA    R1,KEY                                                           
         USING PRGRECD,R1          BUILT KEY FOR PRD GRP DEF                    
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'    PRODUCT GRP RECORD ID                        
         MVC   KEY+2(3),SAVEKEY+2  AGY-MEDIA/CLT, SAME AS BEFORE                
         MVC   PRGKID,8(R2)        GRP ID, GOT IT FROM THE SCREEN               
         DROP  R1                  KEY IS BUILT                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE     MIGHT NOT BE THE SAME RECORD                 
         BE    *+8                                                              
         B     ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         SR    RE,RE                                                            
         IC    RE,PRGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,BYTE2            SAVE TOTAL DIGITS                            
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)            INPUT LENGTH OF 'ADD EXCEPTION'              
         BCTR  R0,0                MINUS 1 FOR THE LEADING CHAR                 
         CR    RE,R0               THEY SHOULD BE EQUAL                         
         BNE   ERRINV                                                           
         MVC   WORK(4),=4C'0'                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2)       8(R2) IS THE CHARACTER                       
         PACK  DUB,WORK(5)                                                      
         MVC   KEY+6(2),DUB+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   ERRINV                                                           
         MVC   SVPIDGRP,KEYSAVE+5  SAVE THE PRD ID AND PRD GRP                  
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                RESTORING THE ORIGINAL RECORD                
         GOTO1 GETREC              TO CHECK IF THE EXCEPTION ELEM               
         MVC   AIO,AIO1            IS ALREADY THERE.                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BUILTPEL            BUILT PRODUCT EXCEPTION ELEMENT              
*                                                                               
MOREPEX  CLC   SVPIDGRP,2(R6)      CK IF EXCEPTION ELEM ALREADY EXIST           
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         BAS   RE,NEXTEL                                                        
         BE    MOREPEX             MORE PRODUCT EXCEPTION ELEMENT               
*                                                                               
BUILTPEL XC    ELEM,ELEM           START BUILDING EXCEPTION ELEM                
         MVI   ELEM,X'02'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,5            ELEMENT LENGTH (ALSO NO SYMBOL)              
         MVC   ELEM+2(3),SVPIDGRP                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                 END OF ADD PRODUCT EXCEPTION                 
         EJECT                                                                  
***********************************************************************         
*                LISTING THE CLIENT EXCEPTIONS                        *         
***********************************************************************         
CLTEXCP  NTR1                                                                   
         LA    R2,MDMLISTH                                                      
         LA    R3,8(R2)            POINT TO THE LISTING DATA AREA               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         USING MKGEL02,R6          MAP ONTO THE EXCEPTION ELEMENT               
         SR    R4,R4               EXCEPTIONS PER LISTING LINE COUNTER          
         SR    R5,R5               LISTING LINES COUNTER                        
MORECLEX MVC   0(3,R3),2(R6)       DISPLAY CLIENT EXCEPTION                     
         OI    MDMLISTH+6,X'80'                                                 
         C     R5,=F'9'                                                         
         BNH   *+8                                                              
         B     ERREXCEP            MAX NUMB OF EXCEPTION LINES IS 9             
*                                                                               
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)            ONLY 10 EXCEPTION PER LINE                   
         C     R4,=F'10'                                                        
         BL    NEXTCLEX                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         LA    R3,8(R2)                                                         
         LA    R4,0                RESET THE COUNTER                            
         LA    R5,1(R5)            INCREMENT LISTING LINES COUNTER              
*                                                                               
NEXTCLEX BAS   RE,NEXTEL           NEXT CLIENT EXCEPTION ELEMENT                
         BE    MORECLEX            MORE  CLIENT EXCEPTION ELEMENT               
         DROP  R6                                                               
         B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*             LISTING THE PRODUCT EXCEPTIONS                          *         
***********************************************************************         
PRDEXCP  NTR1                                                                   
         LA    R2,MDMLISTH                                                      
         LA    R3,8(R2)            POINT TO THE LISTING DATA AREA               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BNE   XPDEX                                                            
*                                                                               
         SR    R4,R4               EXCEPTIONS PER LISTING LINE COUNTER          
         SR    R5,R5               LISTING LINES COUNTER                        
*                                                                               
MOREPDEX ST    R6,SAVER6                                                        
         MVC   0(1,R3),2(R6)       1 CHARACTER PRODUCT EXCEPTION CODE           
         UNPK  DUB,3(3,R6)                                                      
*                                                                               
         LA    R1,KEY                                                           
         USING PRGRECD,R1                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'    PRODUCT GRP RECORD ID                        
         MVC   KEY+2(3),SAVEKEY+2  AGENCY-MEDIA/CLT ARE SAME AS BEFORE          
         MVC   PRGKID,2(R6)        GROUP ID, PRODUCT EXCEPTION CODE             
         DROP  R1                  KEY IS BUILT                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE     MIGHT NOT BE THE SAME RECORD                 
         BE    *+8                                                              
         B     ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         SR    RE,RE                                                            
         IC    RE,PRGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,BYTE2            SAVE TOTAL DIGITS                            
*                                                                               
         LA    R2,MDMLISTH                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),DUB+3       PRODUCT EXCEPTION DIGITS                     
         OI    MDMLISTH+6,X'80'    SHOW THE DIGITS                              
         DROP  R6                                                               
*                                                                               
         CH    R5,=H'9'                                                         
         BNH   *+8                                                              
         B     ERREXCEP            MAX NUMB OF LINES DISPLAYED IS 9             
*                                                                               
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'9'            9 EXCEPTION PER LINE                         
         BL    NEXTPDEX                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         LA    R3,8(R2)                                                         
         LA    R4,0                RESET THE COUNTER                            
         LA    R5,1(R5)            INCREMENT LISTING LINES COUNTER              
*                                                                               
NEXTPDEX MVC   AIO,AIO1            NEXT PRODUCT EXCEPTION ELEMENT               
         L     R6,SAVER6                                                        
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,NEXTEL                                                        
         BE    MOREPDEX            MORE PRODUCT EXCEPTION ELEMENT               
*                                                                               
XPDEX    B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*                VALIDATING THE BREAK LENGTHS                         *         
***********************************************************************         
VALBKLN  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C'1'          CAN ONLY BE 1,2,3, AND 4                     
         BL    ERRBKLN                                                          
         CLI   8(R2),C'4'                                                       
         BH    ERRBKLN                                                          
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB,8(1,R2)                                                      
         CVB   RE,DUB              LENGTH NOW IS IN BINARY                      
         STC   RE,SVBKLNB          SAVE AWAY THE VALID BINARY BK LN             
         SR    R0,R0                                                            
         IC    R0,SUMBKLN                                                       
         AR    RE,R0                                                            
         CH    RE,=H'4'                                                         
         BH    ERRTOTLN            MAX SUM OF THE 3 LENGTHS IS 4                
         STC   RE,SUMBKLN          SAVE AWAY THE VALID TOTAL BK LN              
XVALBKLN B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*         CLEARS THE ENTIRE SCREEN, BUT NOT THE KEY FIELDS            *         
***********************************************************************         
CLRSCR   NTR1                                                                   
         XC    MDMBK1,MDMBK1       CLEAR BREAK TITLES1                          
         MVI   MDMBK1H+5,0                                                      
         OI    MDMBK1H+6,X'80'                                                  
         XC    MDMBK2,MDMBK2       CLEAR BREAK TITLES2                          
         MVI   MDMBK2H+5,0                                                      
         OI    MDMBK2H+6,X'80'                                                  
         XC    MDMBK3,MDMBK3       CLEAR BREAK TITLES3                          
         MVI   MDMBK3H+5,0                                                      
         OI    MDMBK3H+6,X'80'                                                  
         XC    MDMLN1,MDMLN1       CLEAR BREAK LENGTH1                          
         MVI   MDMLN1H+5,0                                                      
         OI    MDMLN1H+6,X'80'                                                  
         XC    MDMLN2,MDMLN2       CLEAR BREAK LENGTH2                          
         MVI   MDMLN2H+5,0                                                      
         OI    MDMLN2H+6,X'80'                                                  
         XC    MDMLN3,MDMLN3       CLEAR BREAK LENGTH3                          
         MVI   MDMLN3H+5,0                                                      
         OI    MDMLN3H+6,X'80'                                                  
         XC    MDMPGA,MDMPGA       CLEAR PRODUCT GROUP ASSGNS                   
         MVI   MDMPGAH+5,0                                                      
         OI    MDMPGAH+6,X'80'                                                  
         XC    MDMAMK,MDMAMK       CLEAR ALL MARKETS                            
         MVI   MDMAMKH+5,0                                                      
         OI    MDMAMKH+6,X'80'                                                  
         XC    MDMAEX,MDMAEX       CLEAR ADD EXCEPTION                          
         MVI   MDMAEXH+5,0                                                      
         OI    MDMAEXH+6,X'80'                                                  
*                                                                               
         LA    R2,MDMLISTH                                                      
         LA    R3,MDMENDH                                                       
*                                                                               
CLRSCR10 SR    R1,R1                                                            
         IC    R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0             CLEAR OUT THE FIELD                          
         OI    6(R2),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               PAST LAST OUTPUT FIELD?                      
         BNH   CLRSCR10            NO, CONTINUE                                 
*                                                                               
CLSCX    B     XIT                 YES, DONE                                    
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
*                                                                               
         LA    R2,CONACTH          POINT TO ACTION                              
         CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BE    SETUP01                                                          
         TM    T217FFD+12,X'01'                                                 
         BNO   SETUP01             NOT ON = ALL OK                              
         CLI   ACTNUM,ACTCHA                                                    
         BE    ERRSEC2             CHANGE NOT ALLOWED                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRSEC2             ADD NOT ALLOWED                              
*                                                                               
SETUP01  OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION FROM THE LIST                    
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'REP LIST'                                                
         SSPEC H2,30,C'--------'                                                
         SPACE 1                                                                
         SSPEC H4,1,C'MEDIA'                                                    
         SSPEC H5,1,C'-----'                                                    
         SSPEC H4,17,C'CLIENT'                                                  
         SSPEC H5,17,C'------'                                                  
         SSPEC H4,41,C'MKT GRP ID'                                              
         SSPEC H5,41,C'----------'                                              
         SSPEC H4,53,C'BREAK TITLE 1'                                           
         SSPEC H5,53,C'-------------'                                           
         SSPEC H4,68,C'LEN'                                                     
         SSPEC H5,68,C'---'                                                     
         SSPEC H4,73,C'BREAK TITLE 2'                                           
         SSPEC H5,73,C'-------------'                                           
         SSPEC H4,88,C'LEN'                                                     
         SSPEC H5,88,C'---'                                                     
         SSPEC H4,93,C'BREAK TITLE 3'                                           
         SSPEC H5,93,C'-------------'                                           
         SSPEC H4,108,C'LEN'                                                    
         SSPEC H5,108,C'---'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
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
       ++INCLUDE SPSFMCFD          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDFD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
STATUS   DS    XL1                 STATUS BYTE FLAG                             
KEYCHG   EQU   X'80'               KEY HAS CHANGED                              
CLIENT   DS    CL3                                                              
MKTGID   DS    CL1                                                              
HMKTGID  DS    CL2                                                              
SAVER6   DS    F                   SAVES R6, THE AIO POINTER                    
WKAGMD   DS    XL1                 WORK FIELD: AGENCY-MEDIA CODE                
WKBCLI   DS    XL2                 WORK FIELD: BINARY CLIENT                    
SUMBKLN  DS    XL1                 SUM OF THE 3 BREAK LENGTHS                   
SVPIDGRP DS    XL3                 SAVE PRD ID AND PRD GRP                      
LISTSEL  DS    C                   FOR USE WITH LIST SELECTIONS                 
SAVEKEY  DS    CL13                                                             
TEMPKEY  DS    CL13                FOR RESTORATION PURPOSES                     
SVBKLNS  DS    XL2                 SAVED BREAK LENGTHS                          
SVBKLNB  DS    XL1                 SAVED BINARY BREAK LENGTH                    
BYTE2    DS    XL1                 TOTAL BREAK LENGTHS                          
BAGY     DS    XL1                 BINARY AGENCY (NO MEDIA)                     
BAGYHIGH DS    XL1                                                              
LKEYAGMD DS    XL1                 PARTS OF KEY FOR ACT = LIST                  
LKEYBCLT DS    XL2                                                              
LKEYMID  DS    CL1                                                              
LKEYFLAG DS    XL1                 FLAG FOR KEY ORDERS FOR LIST                 
TEMPFLD  DS    XL11                TEMP FIELD FOR BLANK MEDIA                   
*                                                                               
SVPGB1   DS    CL12                SAVE FIELDS OF OLD 01 DATA                   
SVPGBL1  DS    XL1                                                              
SVPGB2   DS    CL12                                                             
SVPGBL2  DS    XL1                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
SVBCLT   DS    XL2                 BACKUP BINARY CLIENT CODE FOR LIST           
SVKEY1   DS    XL20                USED TO BACK UP KEY BEFORE READ CLT          
SVAIO    DS    F                                                                
CLTAAN   DS    CL1                                                              
THISAM   DS    XL1                                                              
*                                                                               
JUSTSEL  DS    C                                                                
NUMRECS  DS    F                                                                
NEXTREC  DS    F                                                                
LASTREC  DS    F                                                                
LASTNUM  DS    F                                                                
SORTRECS DS    CL2250              ROOM TO SORT 250 MARKET GROUP RECS           
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMED    DS    CL1                                                              
         DS    CL4                                                              
LSCLT    DS    CL3                                                              
         DS    CL2                                                              
LSMID    DS    CL2                                                              
         DS    CL5                                                              
LSBKT1   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN1  DS    CL1                                                              
         DS    CL3                                                              
LSBKT2   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN2  DS    CL1                                                              
         DS    CL3                                                              
LSBKT3   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN3  DS    CL1                                                              
         EJECT                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                   LABELS FOR SPOOL                             
P1A      DS    CL1                                                              
PRTMED   DS    CL1                                                              
P1B      DS    CL1                                                              
         DS    CL1                                                              
PRTMEDN  DS    CL10                                                             
         DS    CL2                                                              
P2A      DS    CL1                                                              
PRTCLT   DS    CL3                                                              
P2B      DS    CL1                                                              
         DS    CL1                                                              
PRTCLTN  DS    CL16                                                             
         DS    CL2                                                              
PRTMID   DS    CL1                                                              
         DS    CL11                                                             
PRTBKT1  DS    CL12                                                             
         DS    CL3                                                              
PRTBKLN1 DS    CL1                                                              
         DS    CL4                                                              
PRTBKT2  DS    CL12                                                             
         DS    CL3                                                              
PRTBKLN2 DS    CL1                                                              
         DS    CL4                                                              
PRTBKT3  DS    CL12                                                             
         DS    CL3                                                              
PRTBKLN3 DS    CL1                                                              
*                                                                               
SORTMGRD DSECT                                                                  
SORTON   DS    0CL5                                                             
SRTMED   DS    CL1                                                              
SRTCLT   DS    CL2                                                              
SRTMGRID DS    CL2                                                              
SRTDA    DS    CL4                                                              
SRTRECLQ EQU   *-SRTMED                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPSFM4F   04/26/16'                                      
         END                                                                    
