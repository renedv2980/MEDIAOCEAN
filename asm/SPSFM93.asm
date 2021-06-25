*          DATA SET SPSFM93    AT LEVEL 217 AS OF 02/27/06                      
*PHASE T21793A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21793 -- DESTINATION RECORD MAINTENANCE AND LIST    *         
*                                                                     *         
*  COMMENTS:     MAINTAINS DESTINATION RECORDS                        *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM49 (MAINT) & SPSFM4A (LIST)              *         
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
         TITLE 'T21793 - DESTINATION RECORD MAINTENANCE AND LIST'               
T21793   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1793**,R7,RR=R3                                              
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
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        RECORD HAS JUST BEEN ADDED                   
         BE    DR25                                                             
         CLI   MODE,XRECPUT        RECORD HAS JUST BEEN PUT                     
         BE    DR25                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
*                                                                               
         MVI   DESTFLAG,0                                                       
         XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
*                                                                               
         LA    R4,SVKEY                                                         
         USING DESTNRCD,R4                                                      
         MVI   DSRKTYPE,DSRKTYPQ   MOVE RECORD TYPE X'0D' TO KEY                
         MVI   DSRKSBTY,DSRKSBTQ   MOVE RECORD SUBTYPE X'3D' TO KEY             
*                                                                               
         MVI   USEIONUM,3          USING AIO3 FOR TEMPORARY STORAGE...          
         LA    R2,DSRMEDKH         ...FOR THE VALIDATION SUBROUTINES            
*                                                                               
         CLI   ACTNUM,ACTLIST      IS IT LIST/REPORT? (REPORT=12>LIST)          
         BNL   VK05                 YEAH IT IS, NO NEED FOR NONSENSE            
         OI    DSRDESLH+1,X'0C'    ZERO INTENSITY                               
         OI    DSRDESTH+1,X'0C'      "      "                                   
         OI    DSRDESTH+1,X'20'    PROTECT THE FIELD                            
         OI    DSRROULH+1,X'0C'    ZERO INTENSITY                               
         OI    DSRROUTH+1,X'0C'      "      "                                   
         OI    DSRROUTH+1,X'20'    PROTECT THE FIELD                            
*                                                                               
         BAS   RE,ZEROUP           DEALS WITH THE RADIO REP CODES               
*                                                                               
         CLI   DSRMEDK,C'T'        IS THE MEDIA TV?                             
         BE    VK04                                                             
*                                                                               
         CLI   DSRMEDK,C'R'        IS THE MEDIA RADIO?                          
         BNE   ERRMED               NAH, JUMP TO ERROR                          
         NI    DSRDESLH+1,X'FF'-X'04'   CHANGE FIELD TO HIGH INTENSITY          
         NI    DSRDESTH+1,X'FF'-X'04'    "      "      "                        
         NI    DSRDESTH+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
         NI    DSRROULH+1,X'FF'-X'0C'   TAKE OFF ZERO INTENSITY                 
         NI    DSRROUTH+1,X'FF'-X'0C'    "      "      "                        
         NI    DSRROUTH+1,X'FF'-X'20'   TAKE OFF PROTECTION                     
*                                                                               
         BAS   RE,UNZERO           DEALS WITH THE RADIO REP CODES               
*                                                                               
         BAS   RE,BLDREPS          BUILD THE REP LIST FROM DDDARETAB            
*                                                                               
VK04     OI    DSRDESLH+6,X'80'    TRANSMIT THESE FIELDS                        
         OI    DSRDESTH+6,X'80'                                                 
         OI    DSRROULH+6,X'80'                                                 
         OI    DSRROUTH+6,X'80'                                                 
*                                                                               
         BAS   RE,REPXMIT                                                       
*                                                                               
VK05     GOTO1 VALIMED                                                          
*                                                                               
*****  NEW ERROR MESSAGE TO PREVENT NOW OPTION FOR "MEDIA" REPORTS              
         CLI   ACTNUM,ACTREP       WE DOING REPORT?                             
         BNE   VK07                 - NOPE, CONTINUE NORMALLY                   
         CLC   =C'NOW',CONWHEN     IS IT A NOW REPORT?                          
         BNE   VK07                 - NOPE                                      
         CLI   DSRSTAH+5,0         ANY FILTERS?                                 
         BE    ERRIO                - NOPE, ERROR!!                             
*****                                  MHC  04/20/04                            
*                                                                               
VK07     MVC   DSRKAGMD,BAGYMD     MOVE 1 BYTE BINARY AGENCY/MEDIA CODE         
         OI    DSRMEDKH+6,X'80'                                                 
*                                                                               
         MVC   DSRMEDN,MEDNM                                                    
         OI    DSRMEDNH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    DSRSTAH+6,X'80'                                                  
         LA    R2,DSRSTAH                                                       
         CLI   8(R2),C'0'          IS IT LOWER THAN A NUMBER 0?                 
         BNL   VK08                 - NO IT ISN'T, CHECK IF IT'S CABLE          
         CLI   9(R2),C'0'                                                       
         BNL   VK08                                                             
         CLI   10(R2),C'0'                                                      
         BNL   VK08                                                             
         CLI   11(R2),C'0'                                                      
         BNL   VK08                                                             
         B     VK10                                                             
VK08     CLI   DSRMEDK,C'C'        IS THE MEDIA TYPE 'C' (CABLE)?               
***      BNE   ERRNOCAB             NO, ERROR....                               
*                                                                               
VK10     CLI   5(R2),0             ANY STATION INPUT?                           
         BNE   VK15                 YES                                         
***********************************************************************         
* NOTE:  ACTLIST == 10   ACTREP == 12                                 *         
*        ALL PREVIOUS BE ARE CHANGED TO BNL AND BNE ARE CHANGED TO BL *         
*         TO ACCOMODATE FOR NEW REPORT ACTION IMPLEMENTATION          *         
*         WHEREVER A "CLI   ACTNUM,ACTLIST" EXISTS    MHC 05/06/02    *         
***********************************************************************         
         CLI   ACTNUM,ACTLIST      IS IT LIST/REPORT? (REPORT=12>LIST)          
         BL    ERRNOSTA             NO, ERROR, IT IS REQUIRED                   
         B     VK30                 YEAH, GO ON                                 
*                                                                               
VK15     CLI   DSRSTAH+5,5                                                      
         BH    ERRSTA5             STATION HAS MORE THAN 5 CHARS, ERROR         
         CLI   ACTNUM,ACTLIST      IS IT LIST OR REPORT ACTION?                 
         BL    VK25                 NO                                          
*                                                                               
VK20     LA    R6,KEY              GOING OFF TO READ STATION (MASTER)           
         USING STARECD,R6           RECORDS TO GET A VALID STATION TO           
         MVI   STAKTYPE,STAKTYPQ    START ON IN CASE USER TYPES...              
         MVC   STAKMED,DSRMEDK      ...AN INVALID STATION                       
         MVC   STAKCALL,DSRSTA                                                  
***  CABLE                                                                      
         TM    DSRSTAH+4,X'08'     IS THE FIELD NUMERIC?                        
         BZ    VK22                 - NOPE IT'S NOT                             
         MVC   STAKCALL(4),=C'0000'                                             
*                                                                               
         CLI   DSRSTA+1,C' '                                                    
         BNE   VK21C                                                            
         MVC   STAKCALL+3,DSRSTA                                                
         B     VK22                                                             
*                                                                               
VK21C    CLI   DSRSTA+2,C' '                                                    
         BNE   VK21E                                                            
         MVC   STAKCALL+2,DSRSTA                                                
         B     VK22                                                             
*                                                                               
VK21E    CLI   DSRSTA+1,C' '                                                    
         BNE   VK21G                                                            
         MVC   STAKCALL+1,DSRSTA                                                
         B     VK22                                                             
*                                                                               
VK21G    CLI   DSRSTA+4,C' '                                                    
         BNE   VK22                                                             
         MVC   STAKCALL(4),DSRSTA                                               
         B     VK22                                                             
***                                                                             
VK22     CLI   DSRMEDK,C'T'        IS THE MEDIA T?                              
         BNE   *+10                 NO, SKIP NEXT LINE                          
         MVC   STAKCALL+4,DSRMEDK   MOVE IN MEDIA IN 5TH BYTE                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO2                 
         L     R6,AIO2                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),SMKT                                                     
***      MVC   WORK+4(7),STAKCALL                                               
         MVC   WORK+4(5),STAKCALL   DO WE NEED 7 BYTES?                         
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+11                                  
         MVC   DSRKSTA,WORK+13                                                  
*        XC    WORK,WORK                                                        
*        MVC   WORK+13(3),DSRKSTA   MOVE THE STATION BACK                       
*        GOTO1 MSUNPK,DMCB,WORK,WORK+4,WORK+11                                  
*        XC    DSRSTA,DSRSTA                                                    
*        MVI   DSRSTAH+5,5                                                      
*        MVC   DSRSTA(5),WORK+4                                                 
         B     VK30                                                             
         DROP  R6                                                               
*                                                                               
VK25     CLI   DSRMEDK,C'R'        IS THE MEDIA R?                              
         BNE   VK27                                                             
         CLI   DSRSTA+3,C' '       IS IT BLANK??                                
         BE    ERNODASH             YES IT IS!  NO GOOD!  NEED A DASH           
*                                                                               
VK27     GOTO1 VALISTA                                                          
         MVC   DSRKSTA,BSTA        MOVE 3 BYTE STATION CODE                     
*                                                                               
VK30     CLI   ACTNUM,ACTLIST      IS IT LIST OR REPORT?                        
         BNL   VKX                  YES, FINISH HIM!!                           
         LA    R2,DSRCLTH                                                       
         CLI   5(R2),0             IS IT EMPTY?                                 
         BNH   VKX                  YES, YOU ARE FINISHED                       
         CLI   8(R2),C'*'          IS IT AN ASTERISK?                           
         BE    *+12                 YUP                                         
         CLI   8(R2),C'$'          IS IT A DOLLAR SIGN?                         
         BNE   VK33                 YUP                                         
         CLI   5(R2),2             IS IT 2 CHARS LONG?                          
         BNE   VK33                 NO, FORGET IT                               
*                                                                               
         CLI   9(R2),C'9'                                                       
         BH    ERROFF2                                                          
         CLI   9(R2),C'A'                                                       
         BL    ERROFF2                                                          
         MVC   DSRKCLT,DSRCLT                                                   
         B     VKX                                                              
*                                                                               
         USING CLTRECD,R6                                                       
VK33     MVI   USEIONUM,2          LEAVE THE CLIENT RECORD IN AIO2              
         GOTO1 VALICLT                                                          
         L     R6,AIO                                                           
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),DSRCLT                             
         DROP  R6                                                               
         OI    DSRCLTH+6,X'80'                                                  
         MVC   DSRKCLT,BCLT                                                     
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),SVKEY                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         CLI   ACTNUM,ACTADD       IS THE ACTION ADD?                           
         BE    VR10                                                             
*                                                                               
         MVI   ELCODE,DSRIDELQ     THE FAX ELEMENT (X'10')                      
         GOTO1 REMELEM             REBUILD THE ELEMENT                          
VR10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DSRIDELD,R6                                                      
         MVI   DSRIDEL,DSRIDELQ    ELEMENT CODE X'10' - FAX NUM                 
*                                                                               
         LA    R2,DSRFAXH                                                       
         CLI   5(R2),0             WAS THERE ANY INPUT?                         
         BNH   ERRNOFAX             NOPE, MUST HAVE A FAX NUMBER                
         TM    4(R2),X'08'         IS THE FAX NUMBER VALID NUMERIC?             
         BNO   ERRFAXNO             NOPE, TYPE A DAMN NUMBER PLZ                
         MVC   DSRIDFAX,DSRFAX                                                  
*                                                                               
         LA    R2,DSRATTNH                                                      
         CLI   5(R2),0             WAS THERE ANY INPUT?                         
         BNH   VR30                 NO INPUT, BUT IT'S FINE                     
         MVC   DSRIDATT,DSRATTN                                                 
*                                                                               
VR30     LA    R2,DSRPHONH                                                      
         CLI   5(R2),0             WAS THERE ANY INPUT?                         
         BNH   VR40                 NO INPUT, BUT IT'S OK                       
         TM    4(R2),X'08'         IS THE FAX NUMBER VALID NUMERIC?             
         BNO   ERRFAXNO             NOPE, TYPE A DAMN NUMBER PLZ                
         MVC   DSRIDPHN,DSRPHON                                                 
*                                                                               
VR40     MVI   DSRIDLEN,DSRIDLNQ   LENGTH OF 59                                 
         GOTO1 ADDELEM             ADD FAX NUM (X'10') ELEMENT                  
         DROP  R6                                                               
*                                                                               
         CLI   DSRMEDK,C'R'        IS THIS DESTINE RADIO?                       
         BNE   VRX                  NOPE, END IT                                
         MVI   ELCODE,DSRDRELQ     THE RADIO DEST/ROUTE ELEMENT (X'10')         
         GOTO1 REMELEM             GET RID OF IT AND THEN REBUILD               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DSRDRELD,R6                                                      
         MVI   DSRDREL,DSRDRELQ    ELEMENT CODE X'20' - RADIO DES/ROU           
*                                                                               
         LA    R2,DSRDESTH                                                      
         CLI   5(R2),0                                                          
         BNH   ERNODEST                                                         
         OC    8(3,R2),SPACES        PAD WITH SPACES                            
         CLC   =C'REP',8(R2)                                                    
         BE    VR57                                                             
         CLC   =C'RE ',8(R2)                                                    
         BE    VR57                                                             
         CLC   =C'R  ',8(R2)                                                    
         BE    VR57                                                             
         CLC   =C'STA',8(R2)                                                    
         BE    VR58                                                             
         CLC   =C'ST ',8(R2)                                                    
         BE    VR58                                                             
         CLC   =C'S  ',8(R2)                                                    
         BE    VR58                                                             
         B     ERBADDES                                                         
*                                                                               
VR57     MVC   DSRDEST,=C'REP'                                                  
         B     *+10                                                             
VR58     MVC   DSRDEST,=C'STA'                                                  
         MVC   DSRDRDES,DSRDEST    MOVE ONLY THE FIRST BYTE (R/S)               
         OI    DSRDESTH+6,X'80'                                                 
*                                                                               
VR60     LA    R2,DSRROUTH                                                      
         CLC   DSRDEST,=C'REP'                                                  
         BE    VR63                                                             
         CLI   5(R2),0                                                          
         BNE   ERBADROU                                                         
         B     VR65                                                             
*                                                                               
VR63     BAS   RE,REPCHK                                                        
         BNE   ERBADREP                                                         
*                                                                               
VR65     MVC   DSRDRROU,DSRROUT    SAVE THE ROUTE (3 CHARS)                     
*                                                                               
VR70     MVI   DSRDRLEN,DSRDRLNQ   LENGTH OF 20                                 
         GOTO1 ADDELEM             ADD RADIO DEST/ROUTE (X'20') ELEMENT         
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
*                                                                               
         TWAXC DSRFAXH                                                          
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         MVI   DESTFLAG,0          NEED TO WIPE OUT FLAG EVERYTIME!!            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSRIDELQ     GET THE PRIMARY ID ELEMENT (X'10')           
         BAS   RE,GETEL                                                         
         BNE   ERRFAXEL            FAX ELEMENT MUST EXIST                       
*                                                                               
         USING DSRIDELD,R6                                                      
         MVC   DSRFAX,DSRIDFAX     ALL FIELDS PREVIOUSLY TRANSMITTED            
         MVC   DSRATTN,DSRIDATT     IN TWAXC MACRO                              
         MVC   DSRPHON,DSRIDPHN                                                 
*                                                                               
         CLI   DSRMEDK,C'R'        IS IT DESTINE RADIO?                         
         BNE   DR20                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,DSRDRELQ     GET RADIO DEST/ROUTE ELEMENT (X'20')         
         BAS   RE,GETEL                                                         
         BNE   ERRRDREL            DEST/ROUTE MUST EXIST FOR RADIO              
*                                                                               
         USING DSRDRELD,R6                                                      
         CLI   DSRDRDES,DSRDRREP   IS THE DESTINATION REP?                      
         BNE   DR12                 NO IT ISN'T, SKIP TO NEXT CHECK             
         MVC   DSRDEST,=C'REP'                                                  
         B     DR15                                                             
*                                                                               
DR12     CLI   DSRDRDES,DSRDRSTA   IS THE DESTINATION STA?                      
         BNE   DR13                                                             
         MVC   DSRDEST,=C'STA'                                                  
         B     DR15                                                             
*                                                                               
DR13     MVC   DSRDEST,=C'???'                                                  
         OI    DESTFLAG,BADROUTE   X'80'                                        
*                                                                               
DR15     OC    DSRDRROU,DSRDRROU   MOVE THE ROUTE IN, IF ANY                    
         BZ    DR20                                                             
         MVC   DSRROUT,DSRDRROU                                                 
*                                                                               
DR20     CLI   ACTNUM,ACTADD       IS THIS AN ADD?                              
         BE    DRX                  YES                                         
*                                                                               
DR25     L     R6,AIO              *** THIS IS THE ENTRY FOR XREC ***           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            MUST BE THERE                                
         BNE   DRX                 END IT IF NOT THERE, FOR NOW                 
*        BE    *+6                                                              
*        DC    H'0'                                                             
         USING ACTVD,R6                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,DSRCDTE)   ADDED DATE                
         OI    DSRCDTEH+6,X'80'                                                 
*                                                                               
         OC    ACTVCHDT,ACTVCHDT   HAS RECORD BEEN CHANGED BEFORE?              
         BZ    DR30                 NO, SKIP NEXT DATCON CALL                   
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,DSRADTE)   CHANGED DATE              
         OI    DSRADTEH+6,X'80'                                                 
*                                                                               
DR30     XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SVSECAGY     SECURITY AGENCY                             
         MVC   PASSWD,ACTVADID      PERSON WHO CREATED REC (2 BYTE ID)          
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   DSRCRTR,PRSNLID      PERSON WHO CREATED REC (NAME)               
         OI    DSRCRTRH+6,X'80'     TRANSMIT                                    
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVC   SECRAGY,SVSECAGY     SECURITY AGENCY                             
         MVC   PASSWD,ACTVCHID      PERSON WHO CHANGED REC (2 BYTE ID)          
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   DSRCWHO,PRSNLID      PERSON WHO CHANGED REC (NAME)               
         OI    DSRCWHOH+6,X'80'     TRANSMIT                                    
*                                                                               
DRX      TM    DESTFLAG,BADROUTE   DID WE NOT HAVE A GOOD ROUTE?                
         BO    ERNODEST             - NOPE, WE DIDN'T, ERROR                    
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
         L     R3,AIO                                                           
         USING DESTNRCD,R3                                                      
         MVC   AMBYTE,DSRKAGMD     ISOLATE MEDIA CODE                           
         NI    AMBYTE,X'0F'                                                     
         LA    R5,MEDTAB           FIND MEDIA CODE USING MEDIA TABLE            
*                                                                               
DK10     CLC   AMBYTE,1(R5)                                                     
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   DSRMEDK,0(R5)                                                    
         OI    DSRMEDKH+6,X'80'                                                 
         MVI   DSRMEDKH+5,1        TRANSMIT MEDIA CODE TO SCREEN                
*                                                                               
***********************************************************************         
*                                                                               
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),DSRKSTA                                                
***      OI    WORK,X'80'          NEED THIS BIT ON FOR 8 BYTE STATION          
         GOTO1 MSUNPK,DMCB,WORK,WORK+7,DSRSTA                                   
         OI    DSRSTAH+6,X'80'     TRANSMIT STATION CODE TO SCREEN              
         CLI   DSRSTA+3,C' '       IS THE 4TH BYTE A SPACE?                     
         BNE   DK27                 - NO, DON'T WORRY ABOUT IT                  
         CLI   DSRMEDK,C'R'        IS THE MEDIA RADIO?                          
         BNE   DK27                 - ONLY REPLACE IF IT'S MEDIA RADIO          
         MVI   DSRSTA+3,C'-'        - YEAH, REPLACE IT WITH A DASH              
*                                                                               
DK27     LA    R4,DSRSTA+L'DSRSTA-1                                             
DK28     CLI   0(R4),X'40'                                                      
         BH    DK30                                                             
         BCT   R4,DK28                                                          
*                                                                               
DK30     LA    R5,DSRSTA                                                        
         SR    R4,R5                                                            
         AHI   R4,1                NEED TO ADD 1 TO CORRECT THE LENGTH          
         STC   R4,DSRSTAH+5                                                     
*                                                                               
***********************************************************************         
*                                                                               
         OC    DSRKCLT,DSRKCLT     IS THE CLIENT BLANK?                         
         BZ    DKX                  YEAH, LET'S GET OUTTA HERE                  
         MVI   DSRCLTH+5,2                                                      
         CLI   DSRKCLT,C'*'                                                     
         BE    *+12                                                             
         CLI   DSRKCLT,C'$'                                                     
         BNE   DK35                                                             
         MVC   DSRCLT(L'DSRKCLT),DSRKCLT                                        
         B     DK37                                                             
*                                                                               
*&&DO                                                                           
DK35     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(13),KEY                                                  
         XC    KEY,KEY                                                          
         USING CLTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVI   CKEYTYPE,CKEYTYPQ   RECORD TYPE X'00'                            
         MVC   CKEYAM,DSRKAGMD                                                  
         MVC   CKEYCLT,DSRKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY                                                   
         BE    DK36A                                                            
         GOTO1 CLUNPK,DMCB,DSRKCLT,DSRCLT                                       
         B     DK36B                                                            
*                                                                               
DK36A    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         GOTO1 CLUNPK,DMCB,(CPROF+6,DSRKCLT),DSRCLT                             
         DROP  R6                                                               
*                                                                               
DK36B    XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 READ                                                             
*&&                                                                             
DK35     GOTO1 CLUNPK,DMCB,DSRKCLT,DSRCLT                                       
         CLI   DSRCLT+2,X'40'      IS THE 3RD CHAR HIGHER THAN SPACE?           
         BNH   *+8                  NO, LENGTH IS STILL ONLY 2                  
         MVI   DSRCLTH+5,3                                                      
DK37     OI    DSRCLTH+6,X'80'     TRANSMIT CLIENT CODE TO SCREEN               
*                                                                               
***********************************************************************         
*                                                                               
DKX      B     VK                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
         MVI   NLISTS,14           MAXIMUM OF 14 RECORDS PER SCREEN             
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                 NAH                                         
*                                                                               
LR8      MVC   KEY(13),SVKEY       GETTING READY TO DO READ HIGH                
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(3),SVKEY        SAME RECORD TYPE/MEDIA?                      
         BNE   LRX                 NO MORE DESTINATION RECORDS TO LIST          
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         USING DESTNRCD,R4                                                      
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),DSRKSTA                                                
         OI    WORK,X'80'          NEED THIS BIT ON FOR 8 BYTE STATION          
         GOTO1 MSUNPK,DMCB,WORK,WORK+7,LSSTATN                                  
         CLI   DSRMEDK,C'R'        IF IT'S MEDIA RADIO                          
         BNE   LR35                                                             
         CLI   LSSTATN+3,C' '      AND THE 4TH BYTE IS SPACE                    
         BNE   LR35                                                             
         MVI   LSSTATN+3,C'-'      REPLACE IT WITH A DASH                       
*                                                                               
LR35     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(13),KEY                                                  
         XC    KEY,KEY             NEED TO READ CLIENT RECORD TO CHECK          
         USING CLTRECD,R5          ...FOR AAN BYTE                              
         LA    R5,KEY                                                           
         MVI   CKEYTYPE,CKEYTYPQ   RECORD TYPE X'00'                            
         MVC   CKEYAM,DSRKAGMD                                                  
         MVC   CKEYCLT,DSRKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY                                                   
         BE    LR40                                                             
         GOTO1 CLUNPK,DMCB,DSRKCLT,LSCLIENT                                     
         B     LR50                                                             
*                                                                               
LR40     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R5,AIO2                                                          
         GOTO1 CLUNPK,DMCB,(CPROF+6,DSRKCLT),LSCLIENT                           
         DROP  R5                                                               
         DROP  R4                                                               
*                                                                               
LR50     XC    KEY,KEY             KEY PREVIOUSLY HELD CLIENT REC KEY           
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DSRIDELQ     FAX ELEMENT (X'10')                          
         BAS   RE,GETEL                                                         
         USING DSRIDELD,R6                                                      
         MVC   LSFAXNUM,DSRIDFAX                                                
         MVC   LSATTENT,DSRIDATT                                                
         MVC   LSPHNNUM,DSRIDPHN                                                
***    WHY WAS THIS PIECE OF CODE HERE ANYWAY!??!   MHC  12/16/04               
***      CLC   LSCLIENT,=C'ZZZ'                                                 
***      BNE   *+6                                                              
***      DC    H'0'                                                             
         DROP  R6                                                               
***********************************************************************         
*                                                                               
         CLI   MODE,PRINTREP       ARE WE PRINTING REPORTS?                     
         BNE   LR70                 NAH                                         
         CLC   =C'NOW',CONWHEN     IS IT A NOW REPORT?                          
         BNE   LR68                 - NOPE                                      
*****  THIS CHECKS IF WE'RE OVER THE MAX NUM OF IO'S                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIO COUNT IS "SOFT"                        
         MH    R3,=H'9'                                                         
         D     R2,=F'10'                                                        
         CLM   R3,3,FATIOCNT       COMPARE IT TO CURRENT COUNT OF IO'S          
         BNH   ERRIO                - TOO MANY IOS, NEED STA FILTER             
         DROP  R1                                                               
*****                                MHC  04/20/04                              
LR68     BAS   RE,PR                                                            
         B     LRNEXT                                                           
*                                                                               
LR70     GOTO1 LISTMON                                                          
LRNEXT   B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       PRINT RECORD                                  *         
***********************************************************************         
PR       NTR1                                                                   
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         LA    R5,HDHOOK                                                        
         ST    R5,HEADHOOK                                                      
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PSTATN,LSSTATN                                                   
         MVC   PCLIENT,LSCLIENT                                                 
         MVC   PFAXNUM,LSFAXNUM                                                 
         MVC   PATTENT,LSATTENT                                                 
         MVC   PPHNNUM,LSPHNNUM                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     XIT                                                              
***********************************************************************         
*        REPCHK SUBROUTINE                                            *         
***********************************************************************         
REPCHK   DS    0H                                                               
         LR    R0,RE               SAVE OFF RE                                  
         MVC   DMCB+4(4),=X'D9000A1F'   LOAD DDDAREREPS IN R1                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
         L     R1,DMCB                                                          
***      LA    R1,REPIDS                                                        
         USING DRREPTBD,R1                                                      
*                                                                               
         OC    DSRROUT,=C'   '     PAD IT WITH SPACES                           
*                                                                               
REPCHK10 CLI   0(R1),X'FF'         ARE WE DONE YET?                             
         BE    REPCHKNO             - YEAH, WE ARE                              
         TM    DRREPFLG,X'02'      IS IT RADIO?                                 
         BZ    REPNXT               - NOPE, SKIP                                
         TM    DRREPFLG,X'40'      IS IT TESTING RED?                           
         BO    REPNXT               - YUP, SKIP                                 
         CLC   DSRROUT,DRREPCOD    DID WE FIND THE REP CODE?                    
         BE    RECPHKYS             - YUP, WE DONE                              
*                                                                               
REPNXT   LA    R1,DRREPLNQ(R1)     BUMP TO THE NEXT REP                         
         B     REPCHK10                                                         
*                                                                               
RECPHKYS SR    R1,R1                                                            
REPCHKNO LTR   R1,R1                                                            
*                                                                               
REPCHKX  BR    RE                                                               
***********************************************************************         
*        ZEROUP SUBROUTINE                                            *         
***********************************************************************         
ZEROUP   OI    DSRREPTH+1,X'0C'    ZERO INTENSITY                               
         OI    DSRREPUH+1,X'0C'                                                 
         OI    DSRREP1H+1,X'0C'                                                 
         OI    DSRREP2H+1,X'0C'                                                 
         OI    DSRREP3H+1,X'0C'                                                 
         OI    DSRREP4H+1,X'0C'                                                 
         OI    DSRREP5H+1,X'0C'                                                 
         OI    DSRREP6H+1,X'0C'                                                 
         OI    DSRREP7H+1,X'0C'                                                 
         OI    DSRREP8H+1,X'0C'                                                 
         OI    DSRREP9H+1,X'0C'                                                 
         OI    DSRREPAH+1,X'0C'                                                 
         OI    DSRREPBH+1,X'0C'                                                 
         OI    DSRREPCH+1,X'0C'                                                 
         OI    DSRREPDH+1,X'0C'                                                 
         OI    DSRREPEH+1,X'0C'                                                 
         OI    DSRREPFH+1,X'0C'                                                 
         OI    DSRREPGH+1,X'0C'                                                 
         OI    DSRREPHH+1,X'0C'                                                 
         OI    DSRREPIH+1,X'0C'                                                 
         OI    DSRREPJH+1,X'0C'                                                 
         OI    DSRREPKH+1,X'0C'                                                 
         OI    DSRREPLH+1,X'0C'                                                 
         OI    DSRREPMH+1,X'0C'                                                 
*                                                                               
         BR    RE                                                               
***********************************************************************         
*        UNZERO SUBROUTINE                                            *         
***********************************************************************         
UNZERO   NI    DSRREPTH+1,X'FF'-X'04'   CHANGE FIELD TO HIGH INTENSITY          
         NI    DSRREPUH+1,X'FF'-X'04'   TAKE OFF ZERO INTENSITY                 
         NI    DSRREP1H+1,X'FF'-X'0C'                                           
         NI    DSRREP2H+1,X'FF'-X'0C'                                           
         NI    DSRREP3H+1,X'FF'-X'0C'                                           
         NI    DSRREP4H+1,X'FF'-X'0C'                                           
         NI    DSRREP5H+1,X'FF'-X'0C'                                           
         NI    DSRREP6H+1,X'FF'-X'0C'                                           
         NI    DSRREP7H+1,X'FF'-X'0C'                                           
         NI    DSRREP8H+1,X'FF'-X'0C'                                           
         NI    DSRREP9H+1,X'FF'-X'0C'                                           
         NI    DSRREPAH+1,X'FF'-X'0C'                                           
         NI    DSRREPBH+1,X'FF'-X'0C'                                           
         NI    DSRREPCH+1,X'FF'-X'0C'                                           
         NI    DSRREPDH+1,X'FF'-X'0C'                                           
         NI    DSRREPEH+1,X'FF'-X'0C'                                           
         NI    DSRREPFH+1,X'FF'-X'0C'                                           
         NI    DSRREPGH+1,X'FF'-X'0C'                                           
         NI    DSRREPHH+1,X'FF'-X'0C'                                           
         NI    DSRREPIH+1,X'FF'-X'0C'                                           
         NI    DSRREPJH+1,X'FF'-X'0C'                                           
         NI    DSRREPKH+1,X'FF'-X'0C'                                           
         NI    DSRREPLH+1,X'FF'-X'0C'                                           
         NI    DSRREPMH+1,X'FF'-X'0C'                                           
*                                                                               
         BR    RE                                                               
***********************************************************************         
*        BLDREPS SUBROUTINE                                           *         
***********************************************************************         
BLDREPS  NTR1                                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1F'   LOAD DDDAREREPS IN R1                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB                                                          
         USING DRREPTBD,R1                                                      
*                                                                               
         LA    RE,DSRREP1H         LET'S START WITH THE FIRST ONE               
         LA    R2,DSRREPMH         THE LAST ONE                                 
BLDREP10 CR    RE,R2                                                            
         BH    BLDREPX             WE'RE DONE                                   
*                                                                               
         LR    RF,RE                                                            
         TM    1(RF),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    BLDREPNX                                                         
         XR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         SHI   RF,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RF),111           IS IT ONE OF THE REP FIELDS?                 
         BNE   BLDREPNX             - NOPE                                      
*                                                                               
BLDREP50 CLI   0(R1),X'FF'         ARE WE DONE?                                 
         BE    BLDREPX              - YUP WE ARE                                
         TM    DRREPFLG,X'02'      IS IT RADIO?                                 
         BZ    NEXTREP              - NOPE, SKIP                                
         TM    DRREPFLG,X'40'      IS IT TESTING REP?                           
         BO    NEXTREP              - YUP, SKIP                                 
         MVI   5(RE),15            FOUND ONE THAT WE NEED TO DISPLAY            
         MVC   8(10,RE),DRREPNAM   DISPLAY REP NAME                             
         MVC   20(3,RE),DRREPCOD   DISPLAY REP CODE                             
         LA    R1,DRREPLNQ(R1)     NEXT REP                                     
         B     BLDREPNX            NEXT FIELD                                   
*                                                                               
NEXTREP  LA    R1,DRREPLNQ(R1)     BUMP TO THE NEXT REP                         
         B     BLDREP50                                                         
*                                                                               
BLDREPNX XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     BLDREP10                                                         
*                                                                               
BLDREPX  J     XIT                                                              
***********************************************************************         
*        REPXMIT SUBROUTINE                                           *         
***********************************************************************         
REPXMIT  OI    DSRREPTH+6,X'80'                                                 
         OI    DSRREPUH+6,X'80'                                                 
         OI    DSRREP1H+6,X'80'                                                 
         OI    DSRREP2H+6,X'80'                                                 
         OI    DSRREP3H+6,X'80'                                                 
         OI    DSRREP4H+6,X'80'                                                 
         OI    DSRREP5H+6,X'80'                                                 
         OI    DSRREP6H+6,X'80'                                                 
         OI    DSRREP7H+6,X'80'                                                 
         OI    DSRREP8H+6,X'80'                                                 
         OI    DSRREP9H+6,X'80'                                                 
         OI    DSRREPAH+6,X'80'                                                 
         OI    DSRREPBH+6,X'80'                                                 
         OI    DSRREPCH+6,X'80'                                                 
         OI    DSRREPDH+6,X'80'                                                 
         OI    DSRREPEH+6,X'80'                                                 
         OI    DSRREPFH+6,X'80'                                                 
         OI    DSRREPGH+6,X'80'                                                 
         OI    DSRREPHH+6,X'80'                                                 
         OI    DSRREPIH+6,X'80'                                                 
         OI    DSRREPJH+6,X'80'                                                 
         OI    DSRREPKH+6,X'80'                                                 
         OI    DSRREPLH+6,X'80'                                                 
         OI    DSRREPMH+6,X'80'                                                 
*                                                                               
         BR    RE                                                               
***********************************************************************         
*        HEADSPECS                                                    *         
***********************************************************************         
HEDSPECS SSPEC H1,50,C'DESTINE RECORDS'                                         
         SSPEC H2,50,C'---------------'                                         
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,AGYADD                                                      
         SSPEC H1,105,PAGE                                                      
         SSPEC H2,105,REQUESTOR                                                 
         SSPEC H3,105,RUN                                                       
         SSPEC H4,1,C'   '                                                      
         SSPEC H4,2,C'MEDIA'                                                    
*                                                                               
         SSPEC H6,2,C'STATION'                                                  
         SSPEC H7,2,C'--------'                                                 
         SSPEC H6,12,C'CLT'                                                     
         SSPEC H7,12,C'---'                                                     
         SSPEC H6,17,C'FAX NUMBER'                                              
         SSPEC H7,17,C'----------------'                                        
         SSPEC H6,34,C'ATTENTION'                                               
         SSPEC H7,34,C'-------------------------'                               
         SSPEC H6,60,C'PHONE'                                                   
         SSPEC H7,60,C'----------------'                                        
*                                                                               
         DC    X'00'                                                            
***********************************************************************         
*        HEADHOOKS                                                              
***********************************************************************         
HDHOOK   NTR1                                                                   
         MVC   H4+11(L'QMED),QMED                                               
         MVC   H4+16(L'MEDNM),MEDNM                                             
*                                                                               
HDHKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRMED   MVC   ERRNUM,=AL2(BADMED)   INVALID MEDIA                              
         B     SPERREX                                                          
ERRSTATN MVC   ERRNUM,=AL2(BADSTATN)   INVALID STATION                          
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRFAXEL MVC   ERRNUM,=AL2(NOFAXEL)   REQUIRED FAX ELEMENT NOT FOUND            
         LA    R2,DSRFAXH                                                       
         B     SPERREX                                                          
ERRNOFAX MVC   ERRNUM,=AL2(NOFAXINP)   NO FAX INPUTTED (REQUIRED)               
         B     SPERREX                                                          
ERRFAXNO MVC   ERRNUM,=AL2(FAXNONUM)   FAX FIELD NOT 100% NUMERIC               
         B     SPERREX                                                          
ERRCLTOF MVC   ERRNUM,=AL2(CLTOFLST)   HAVE BOTH CLIENT AND OFF LIST            
         B     SPERREX                                                          
ERROFF1  MVC   ERRNUM,=AL2(OFFCHAR1)   1ST CHAR MUST BE $ OR *                  
         B     SPERREX                                                          
ERROFF2  MVC   ERRNUM,=AL2(OFFCHAR2)   2ND CHAR MUST BE ALPHA NUMERIC           
         B     SPERREX                                                          
ERRNOSTA MVC   ERRNUM,=AL2(NOSTATN)   MISSING STATION FOR NON LIST              
         B     SPERREX                                                          
ERRNOCAB MVC   ERRNUM,=AL2(USECABLE)   CABLE STATION INVALID                    
         B     SPERREX                                                          
ERNODASH MVC   ERRNUM,=AL2(NODASH)   NO DASH FOR 3 LETTER RADIO STATION         
         B     SPERREX                                                          
ERRSTA5  MVC   ERRNUM,=AL2(STAMORE5)   STATION HAD MORE THAN 5 CHARS            
         B     SPERREX                                                          
ERNODEST MVC   ERRNUM,=AL2(NODEST)   DESTINATION IS REQUIRED                    
         LA    R2,DSRDESTH                                                      
         B     SPERREX                                                          
ERBADDES MVC   ERRNUM,=AL2(BADDEST)   DESTINATION MUST BE REP OR STA            
         LA    R2,DSRDESTH                                                      
         B     SPERREX                                                          
ERBADROU MVC   ERRNUM,=AL2(BDROUTE)   THE ROUTE MUST BE 3 CHARS                 
         LA    R2,DSRROUTH                                                      
         B     SPERREX                                                          
ERBADREP MVC   ERRNUM,=AL2(29)     INVALID REP (NOT IN DDDARETAB)               
         LA    R2,DSRROUTH                                                      
         B     SPERREX                                                          
ERRRDREL MVC   ERRNUM,=AL2(NORDREL)   RADIO DES/ROU ELEM MISSING                
         LA    R2,DSRDESTH                                                      
         B     SPERREX                                                          
ERRIO    MVC   ERRNUM,=AL2(NOFILTER)  TOO MANY IO, NEED STA FILTER              
         LA    R2,DSRSTAH                                                       
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
*                                  SHORT DESP OF ERROR MSGS                     
BADMED   EQU   13                  INVALID MEDIA                                
BADSTATN EQU   18                  INVALID STATION                              
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
NOFAXEL  EQU   932                 REQUIRED FAX ELEMENT NOT FOUND               
NOFAXINP EQU   934                 REQUIRED FAX FIELD INPUT MISSING             
FAXNONUM EQU   935                 NON NUMERIC PHONE NUMBER                     
CLTOFLST EQU   945                 CLIENT AND OFFICE LIST CAN'T COEXIST         
OFFCHAR1 EQU   946                 1ST CHAR MUST BE $ OR *                      
OFFCHAR2 EQU   947                 2ND CHAR MUST BE ALPHA-NUMERIC               
NOSTATN  EQU   948                 MISSING STATION FIELD FOR NON LIST           
USECABLE EQU   959                 CABLE STATION CURRENTLY INVALID              
NODASH   EQU   993                 NO DASH FOR 3 LETTER RADIO STATION           
STAMORE5 EQU   996                 STATION HAS MORE THAN 5 CHARACTERS           
NODEST   EQU   1048                DESTINATION IS REQUIRED                      
BADDEST  EQU   1044                DESTINATION MUST BE REP OR STA               
BDROUTE  EQU   1049                THE ROUTE MUST CONTAIN 3 CHARS               
NORDREL  EQU   1141                REQUIRED RADIO DES/ROU ELEM NO FOUND         
NOFILTER EQU   1222                STA FILTER NEEDED FOR NOW REPORTS            
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         XC    SVMYPSWD,SVMYPSWD   AUTHORIZATION NUMBER                         
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SVSECAGY,FATAGYSC   SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVMYPSWD,FAPASSWD   SAVE AUTHORIZATION NUMBER                    
         DROP  R3                                                               
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   CANNOT DELETE FROM LIST                      
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM49D          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM4AD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDESTN        DESTINE RECORD DSECT                         
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD DSECT                          
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA          STATION RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE DDDARETABD                                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
SAVEKEY  DS    CL32                                                             
SAVEMENU DS    CL1                 SAVE THE EDAYMENU VALUE                      
AMBYTE   DS    CL1                 BYTE TO SAVE AGENCY/MEDIA                    
SVMYPSWD DS    XL2                 AUTHORIZATION NUMBER                         
SVSECAGY DS    XL2                 SECURITY AGENCY                              
DESTFLAG DS    CL1                 MISCELLANEOUS FLAGS FOR DESTINE RECS         
BADROUTE EQU   X'80'                - THE ROUTE IS NOT REP OR STA               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSSTATN  DS    CL8                                                              
         DS    CL2                                                              
LSCLIENT DS    CL3                                                              
         DS    CL2                                                              
LSFAXNUM DS    CL16                                                             
         DS    CL1                                                              
LSATTENT DS    CL25                                                             
         DS    CL1                                                              
LSPHNNUM DS    CL16                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PSTATN   DS    CL8                                                              
         DS    CL2                                                              
PCLIENT  DS    CL3                                                              
         DS    CL2                                                              
PFAXNUM  DS    CL16                                                             
         DS    CL1                                                              
PATTENT  DS    CL25                                                             
         DS    CL1                                                              
PPHNNUM  DS    CL16                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'217SPSFM93   02/27/06'                                      
         END                                                                    
