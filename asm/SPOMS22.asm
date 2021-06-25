*          DATA SET SPOMS22    AT LEVEL 069 AS OF 04/04/13                      
*PHASE T23422A                                                                  
T23422   TITLE 'SPOMS09 - DELETE DARE ORDERS'                                   
T23422   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23422*,R7,RR=R3,CLEAR=YES                                    
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
INIT10   L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,RECDEL         RECORD DELETE?                               
         BE    DELREC                                                           
         CLI   MODE,XRECDEL        RECORD DELETE?                               
         BE    XDELREC                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   MISCFLG1,0                                                       
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED    DS    0H                                                               
         LA    R2,ORSMEDH                                                       
         TM    4(R2),X'20'         IF THIS KEY FIELD CHANGE                     
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   THEN INDICATE IT                             
*                                                                               
         CLI   5(R2),0             NEED THE MEDIA                               
         BNE   VKMED05                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   8(R1),0                                                          
         BE    VKMED05                                                          
         J     NEEDFLDS                                                         
*                                                                               
VKMED05  GOTO1 VALIMED                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    4(R2),X'20'                                                      
VKMEDX   DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* VALIDATE THE ORDER NUMBER                                                     
***************                                                                 
VKORD    DS    0H                                                               
         LA    R2,ORSORDH                                                       
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BO    *+8                 NO                                           
         OI    MISCFLG1,MF1KYCHG   YES, THEN INDICATE IT                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    NEEDFLDS                                                         
         CLI   5(R2),8             ARE THERE 8 CHARACTERS?                      
         BNE   INVLFLD                                                          
*****    TM    4(R2),X'08'         HAS TO BE NUMERIC?                           
*****    BZ    INVLFLD                                                          
         LA    RE,8                THE ABOVE CODE IS COMMENTED BECAUSE          
         LA    RF,8(R2)              WHEN WE SELECT FROM THE LIST               
VKORD10  CLI   0(RF),C'0'            SCR, IT COPIES THE VALIDATED BITS.         
         BL    INVLFLD                                                          
         CLI   0(RF),C'9'          SINCE WE'RE USING 1 BYTE FOR TRADE           
         BH    INVLFLD               IN THE ORDER NUMBER ON THE LIST            
         LA    RF,1(RF)              SCREEN IT WON'T BE A NUMERIC FLD           
         BCT   RE,VKORD10                                                       
*                                                                               
         CLI   8+1(R2),C'3'        Is 2nd digit, higher than 3?                 
         BH    VKORD20                                                          
*                                                                               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,8(R2),FAKEFLD,8   SAVE AS IF ENTRY WAS HEX            
         MVC   PACKOF4B,FAKEFLD    CONVERT IT TO PACK                           
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),FAKEFLD   STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
         B     VKORDX                                                           
*                                                                               
VKORD20  PACK  DUB,8(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
*                                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
VKORDX   DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* BUILD THE KEY                                                                 
***************                                                                 
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING DOKEY,R4            OVERLAY KEY WITH OUR TEMPLATE                
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   RECNTFND                                                         
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE THE RECORD ON SCREEN                                                   
***********************************************************************         
DELREC   DS    0H                                                               
         LA    R2,ORSUPDTH                                                      
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    DELR10                                                           
         MVI   ORSUPDT,C'N'        MOVE IN N                                    
         OI    ORSUPDTH+6,X'80'    TRANSMIT THE FIELD                           
*                                                                               
DELR10   CLI   ORSUPDT,C'N'        DELETE?                                      
         BE    NODELETE            NO                                           
*                                                                               
         CLI   ORSUPDT,C'Y'        DELETE?                                      
         BNE   INVLFLD                                                          
         MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 GETREC                                                           
         CLI   8(R1),0                                                          
         BE    DELR20                                                           
         TM    8(R1),X'02'         WAS THE RECORD DELETED?                      
         BO    ALRDYDEL            RECORD ALREADY DELETED                       
*                                                                               
DELR20   CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    DELRECX             YES, ALLOW DELETE OF ANY ORDER               
         L     R6,AIO                                                           
         MVI   ELCODE,DOXMTELQ     CHECK FOR X'11' ELEMENT!!                    
         BAS   RE,GETEL                                                         
         BE    CANTDELE            CAN NOT DELETE IF PREVIOUSLY SENT!!          
*                                                                               
DELRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE ALL THE PASSIVE KEYS                                                   
***********************************************************************         
XDELREC  DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
***************                                                                 
* DELETE BUYER PASSIVE KEY                                                      
***************                                                                 
XDRBYR   LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   DBKTYPE,DBKTYPQ     X'0D'                                        
         MVI   DBKSUBTY,DBKSTYPQ   X'B4' - BUYER PASSIVE                        
         MVC   DBKAGMD,BAGYMD                                                   
         MVC   DBKBYR,DOIDBYR                                                   
         MVC   DBKORD,BINORDER                                                  
         MVC   DBKSTA,DOISTA                                                    
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   XDRBYRX                                                          
*                                                                               
         OI    KEY+13,X'80'        DELETE IT (BUYER PASSIVE)                    
         GOTO1 WRITE                                                            
XDRBYRX  DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* DELETE STATION PASSIVE KEY                                                    
***************                                                                 
XDRSTA   LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   DSKTYPE,DSKTYPQ     X'0D'                                        
         MVI   DSKSUBTY,DSKSTYPQ   X'B7 - STATION PASSIVE                       
         MVC   DSKAGMD,BAGYMD                                                   
         MVC   DSKBYR,DOIDBYR                                                   
         MVC   DSKSTA,DOISTA                                                    
         MVC   DSKORD,BINORDER                                                  
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   XDRSTAX                                                          
*                                                                               
         OI    KEY+13,X'80'        DELETE IT (STATION PASSIVE)                  
         GOTO1 WRITE                                                            
XDRSTAX  DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* DELETE CLIENT PASSIVE KEY                                                     
***************                                                                 
XDRCLT   LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   DCKTYPE,DCKTYPQ     X'0D'                                        
         MVI   DCKSUBTY,DCKSTYPQ   X'B5' - CLIENT PASSIVE                       
         MVC   DCKAGMD,BAGYMD      AGENCY/MEDIA                                 
         MVC   DCKCLT,DOIDCLT      CLIENT                                       
         MVC   DCKPRD,DOIDPRD      PRODUCT                                      
         MVC   DCKEST,DOIDEST      ESTIMATE                                     
         MVC   DCKSTA,DOISTA       STATION                                      
         MVC   DCKPRD2,DOIDPRD2    PRODUCT 2                                    
         MVC   DCKFLTNM,DOIDFLTN   FLIGHT NUMBER                                
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ      X'03' - SUPPLEMENTARY ID ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   XDRCLT10                                                         
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPTRDE                                                
         BZ    XDRCLT10                                                         
         OI    DCKFLAG,DCKFTRDE                                                 
*                                                                               
XDRCLT10 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   XDRCLTX                                                          
*                                                                               
         OI    KEY+13,X'80'        DELETE IT (CLIENT PASSIVE)                   
         GOTO1 WRITE                                                            
XDRCLTX  DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* DELETE POL ORDER PASSIVE KEY (IF ANY)                                         
* ONCE A POL ORDER IS CONFIRMED, ALL BRANDED ORDERS ARE SENT OUT                
* SO YOU CAN NEVER DELETE ANYWAY                                                
***************                                                                 
***************                                                                 
* DELETE COLOR ORDER PASSIVE KEY                                                
***************                                                                 
XDRCOL   L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   DSCKTYPE,DSCKTYPQ   X'0D'                                        
         MVI   DSCKSTYP,DSCKSTYQ   X'B8' - COLLOR PASSIVE                       
         MVC   DSCKAGMD,BAGYMD                                                  
         MVC   DSCKBYR,DOIDBYR                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COLELQ       X'13' - COLOR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   XDRCOLX                                                          
*                                                                               
         USING COLEL,R6                                                         
         MVC   DSCKSTAT,COLCOL     STATUS COLOR                                 
         MVC   DSCKDATE,COLDATE    CURRENT COLOR DATE (X'FF' COMP)              
         MVC   DSCKORDR,BINORDER                                                
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   XDRCOLX                                                          
*                                                                               
         OI    KEY+13,X'80'        DELETE IT (CLIENT PASSIVE)                   
         GOTO1 WRITE                                                            
*                                                                               
XDRCOLX  DS    0H                                                               
***************                                                                 
* DELETE ALL COMMENT RECORDS                                                    
***************                                                                 
XDRCOM   L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XC    KEY,KEY                                                          
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
         MVC   DOKSTA,DOISTA                                                    
         MVI   DOKCMT,X'01'                                                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
XDRCOM10 CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   XDRCOMX                                                          
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     XDRCOM10                                                         
*                                                                               
XDRCOMX  DS    0H                                                               
***************                                                                 
* UNDELETE THE BATCH RECORD, AND UPDATE THE ELEMENT TO BE NOT LINKED            
***************                                                                 
XDRBATCH L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING DBTKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   DBTKTYP,DBTKTYPQ    X'0D'                                        
         MVI   DBTKSTYP,DBTKSTYQ   X'35'                                        
         MVC   DBTKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   DBTKCLT,DOIDCLT     CLIENT                                       
         MVC   DBTKPRD,DOIDPRD     PRODUCT                                      
         MVC   DBTKEST,DOIDEST     ESTIMATE                                     
         MVC   DBTKSTA,DOISTA      BINARY STAION                                
         MVC   DBTKPRD2,DOIDPRD2   PRODUCT2                                     
         MVC   SVBFLT,DOIDFLTN     FLIGHT (MAY BE 0 FOR NONFLIGHT)              
*                                                                               
         MVI   ELCODE,DOSPELQ      GET THE SUPPLEMENTARY ID X'03'               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOSPELD,R6                                                       
         NI    MISCFLG1,X'FF'-MF1TRADE                                          
         TM    DOSPFLG1,DOSPTRDE   IS THIS A TRADE ORDER?                       
         BZ    *+8                                                              
         OI    MISCFLG1,MF1TRADE   SAVE THE FLAG FOR LATER                      
*                                                                               
         MVC   DBTKMKT,DOSPMKT     GET MARKET                                   
         OC    DOSPMKT,DOSPMKT     WAS MARKET IN THE OM RECORD?                 
         BNZ   XDRBAT05            YES, NO NEED TO GET IT                       
         BAS   RE,GETMKT                                                        
         MVC   DBTKMKT,BMKTSTA     SAVE THE MARKET IN THE KEY                   
         DROP  R6                                                               
*                                                                               
XDRBAT05 OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'       TURN OFF                                     
         CLC   KEY(L'DBTKEY),KEYSAVE  DID WE FIND OUR KEY?                      
         BNE   XDRBATX             NO, JUST EXIT                                
         TM    KEY+13,X'80'        WAS THE KEY DELETED                          
         BZ    XDRBAT10                                                         
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   RDUPDATE,C'Y'       TO AVOID DEADLY EMBRACES                     
         GOTO1 HIGH                                                             
         NI    DBTKSTAT,X'FF'-X'80' UNDELETE THE KEY                            
         GOTO1 WRITE                                                            
         NI    DMINBTS,X'F7'       TURN OFF                                     
         DROP  R4                                                               
*                                                                               
XDRBAT10 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING DARBTCHD,R6                                                      
         NI    DBTRSTAT,X'FF'-X'80' UNDELETE JUST IN CASE                       
         MVI   ELCODE,DBFLTELQ     X'20'                                        
         BAS   RE,GETEL                                                         
XDRBAT15 BNE   XDRBATX                                                          
         USING DBFLTELD,R6                                                      
         CLC   DBFLTFLT,SVBFLT     MATCH ON FLIGHT?                             
         BNE   XDRBAT17            NO                                           
         MVC   BYTE,DBFLTFL1                                                    
         XC    BYTE,MISCFLG1                                                    
         TM    BYTE,MF1TRADE       DID WE MATCH ON CASH/TRADE?                  
         BZ    XDRBAT20            YES                                          
XDRBAT17 BAS   RE,NEXTEL           FIND NEXT FLIGHT ELEM                        
         B     XDRBAT15                                                         
*                                  MARK THE RECORD UNSENT AND UNLINKED          
XDRBAT20 NI    DBFLTFL1,X'FF'-DBFLTSNT-DBFLTDAR                                 
         GOTO1 PUTREC                                                           
*                                                                               
XDRBATX  DS    0H                                                               
*                                                                               
XDELRECX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE BINARY MARKET                                                         
***********************************************************************         
GETMKT   NTR1                                                                   
         XC    BMKT,BMKT                  CLEAR MARKET                          
         XC    FAKEFLDH,FAKEFLDH          CLEAR FIELD HEADER                    
         MVC   FAKEFLD,SPACES             AND FIELD                             
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,FAKEFLD                                 
         LA    R2,FAKEFLDH                                                      
         MVI   FAKEFLDH+5,4               SET FIELD LENGTH                      
         CLI   FAKEFLD+4,C' '             TEST BAND                             
         BNH   *+8                                                              
         MVI   FAKEFLDH+5,5               FIX LENGTH FOR BAND                   
         GOTO1 VALISTA                                                          
         B     XIT                                                              
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
*                                                                               
         LA    R2,SPFTABLE         USE PREDEFINED PFKEYS                        
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
***************                                                                 
* SETUP THE PFKEY LINE                                                          
***************                                                                 
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   ORSPFLN(11),=CL11'PF12=Return'                                   
*                                                                               
         OI    ORSPFLNH+6,X'80'                                                 
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                 *         
* ON ENTRY:    PARAM 1             A(EBCDIC ORDER NUMBER)             *         
* ON EXIT:     BINORDER            BINARY ORDER NUMBER                *         
***********************************************************************         
BINORDR  NTR1                                                                   
         L     R2,0(R1)                                                         
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(R2),DUB,8   SAVE AS IF ENTRY WAS HEX                 
         MVC   PACKOF4B,DUB            CONVERT IT TO PACK                       
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OTHER MESSAGES                                                                
***********************************************************************         
ALRDYDEL MVI   GERROR1,RCALRDEL    RECORD ALREADY DELETED                       
         MVI   GETMSYS,16                                                       
         B     ERREXIT                                                          
RCALRDEL EQU   149                                                              
*                                                                               
CANTDELE MVI   GERROR1,CANTDEL     CAN'T DELETE RECORD                          
         MVI   GETMSYS,255                                                      
         B     ERREXIT                                                          
CANTDEL  EQU   216                                                              
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
NODELETE MVI   GERROR1,YESCNFM     CONFIRM DELETION                             
         B     ERREXIT                                                          
YESCNFM  EQU   214                                                              
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
RECCHNGD MVI   GERROR1,RCWASCHA    RECORD WAS CHANGED - ENTER NEXT RE..         
         B     INFEXIT                                                          
*                                                                               
RECADDED MVI   GERROR1,NEWRECRD    NEW RECORD HAS BEEN ADDED TO THE F..         
         B     INFEXIT                                                          
*                                                                               
MAKESELS MVI   GERROR1,LSTDISPL    LIST DISPLAYED - SELECT OR HIT ENT..         
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
***********************************************************************         
* INFO MESSAGES (SYSTEM 23)                                                     
***********************************************************************         
INFRTEXT LA    R1,MYINFXIT                                                      
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
NOTHING  DC    H'0'                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,55,RUN                                                        
         DC    X'00'                                                            
*                                                                               
ZEROS    DC    12C'0'                                                           
RELO     DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
SPFTABLE  DS    0C                                                              
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD2D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRBTC                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE SPOMSERROR                                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
BITFLAG  DS    XL1                 VARIOUS FLAGS                                
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS SET 1                    
MF1KYCHG EQU   X'80'               -  A KEY FIELD HAS BEEN CHANGED              
MF1TRADE EQU   X'04'               -  TRADE FLAG, DON'T CHANGE OR BAD           
*                                     THINGS WILL HAPPEN                        
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS SET 2                    
*                                                                               
FLTORD   DS    CL8                                                              
BINORDER DS    0XL4                BINARY ORDER NUMBERRDER NUMBER               
BINORDDT DS    XL2                  DATE PORTION                                
BINORDSQ DS    XL2                  SEQUENCE PORTION                            
*                                                                               
SVBFLT   DS    X                   SAVE FLIGHT                                  
                                                                                
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
SAVEKEY  DS    XL(L'DOKEY)                                                      
DISKADDR DS    XL4                 DISK ADDRESS                                 
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
*                                                                               
VGLOBBER DS    A                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069SPOMS22   04/04/13'                                      
         END                                                                    
