*          DATA SET SPEZF23    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T23023A                                                                  
         TITLE 'T23023 - EASI BILLING RECORD'                                   
***********************************************************************         
*                                                                     *         
*  TITLE: T23023 - EASI BILLING RECORDS                               *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR BILL RECS           *         
*            WHICH ARE STORED ON GENDIR/GENFIL, AND ONLY MAINTAINED   *         
*            BY DDS.                                                  *         
*  OUTPUTS: UPDATED BILL RECORDS                                      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO PROGRAM/UNIT TABLES             *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  LEV 08    NOV15/95 ADDED NC - NORTH CAROLINA & UT - UTAH           *         
*  LEV 09    NOV16/95 ADD TYP FILTER AND PRINT ENTIRE ADDR ON REPORT  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23023   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3023**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE                                                                  
VKEY     EQU   *                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY DDS TERMINALS                           
         BNE   INVALER                                                          
         SPACE                                                                  
         LA    R2,LINTYPEH         TYPE                                         
         MVI   SVTYP,0                                                          
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         CLI   ACTNUM,ACTLIST      IGNORE IF LIST                               
         BNE   MISSTYP                                                          
         B     VK200                                                            
         SPACE                                                                  
VK100    CLI   8(R2),C'A'          AGENCY                                       
         BE    VK140                                                            
         CLI   8(R2),C'R'          SOURCE                                       
         BE    VK140                                                            
         CLI   8(R2),C'S'          STATION                                      
         BNE   BADTYP                                                           
VK140    MVC   SVTYP,8(R2)                                                      
         SPACE                                                                  
VK200    LA    R2,LINUIDH          USER ID                                      
         XC    QSTA,QSTA                                                        
         XC    QNET,QNET                                                        
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   VK220                                                            
         CLI   ACTNUM,ACTLIST      IGNORE IF LIST                               
         BNE   MISSID                                                           
         B     VK300                                                            
         SPACE                                                                  
VK220    CLI   SVTYP,C'A'          THIS AGENCY                                  
         BNE   VK250                                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         MVC   SVUID,8(R2)                                                      
         OC    SVUID,SPACES                                                     
         MVC   KEY+15(8),SVUID                                                  
         SPACE                                                                  
         L     R6,AIO1                                                          
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
         SPACE                                                                  
         CLI   8(R1),0                                                          
         BNE   BADUID                                                           
         SPACE                                                                  
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        MVC   ???????,2(R6)       BINARY USER ID (ORIGIN) IF NEEDED            
         B     VK300                                                            
         SPACE                                                                  
VK250    CLI   SVTYP,C'S'          THIS STATION                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* DOESN'T READ STATION FROM STATION FILE, JUST CHECKS REASONABLE *              
         SPACE                                                                  
         ZIC   R0,SPOTCAN                                                       
         MVI   SPOTCAN,C'C'        ALLOW ALL MEDIA                              
         SPACE                                                                  
         GOTO1 VALISTA             VALID STATION RETURNED IN WORK               
         SPACE                                                                  
         STC   R0,SPOTCAN                                                       
         SPACE                                                                  
VK300    LA    R2,LINOPTH                                                       
         SPACE                                                                  
         XC    CUUID,CUUID         USER ID FILTER                               
         XC    CUSTA,CUSTA         STATION FILTER                               
         SPACE                                                                  
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK800                                                            
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         LA    R4,WORK                                                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    OPTERR                                                           
*                                                                               
VK500    CLI   0(R4),0                                                          
         BE    VK800                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
*                                                                               
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK600                                                            
         CLI   1(R4),8                                                          
         BH    UIDLENER                                                         
         MVC   CUUID,22(R4)                                                     
         B     VK700                                                            
*                                                                               
*                                  STATION NO REAL VALIDATION                   
VK600    CLI   12(R4),C'S'                                                      
         BNE   VKERR                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID STATION *'                      
         CLI   1(R4),7                                                          
         BH    VKERR                                                            
         MVC   CUSTA(4),22(R4)                                                  
         MVI   CUSTA+4,C' '                                                     
         LA    RE,25(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   VK620                                                            
         LA    RE,1(RE)                                                         
         MVI   CUSTA+3,C' '                                                     
         B     *+8                                                              
VK620    LA    RE,2(RE)                                                         
         CLI   0(RE),C'T'                                                       
         BE    VK700                                                            
         MVC   CUSTA+4(1),0(RE)                                                 
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK700    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK500                                                         
         MVC   CONHEAD,SPACES                                                   
         SPACE                                                                  
VK800    XC    KEY,KEY             SET UP KEY/SVKEY                             
         LA    R4,KEY                                                           
         USING EZBILLD,R4                                                       
         MVC   EZBKID,=C'ZB'                                                    
         MVC   EZBKTYP,SVTYP                                                    
         MVC   EZBKIDN,SVUID                                                    
         OC    QSTA,QSTA                                                        
         BZ    VK840                                                            
         MVC   EZBKIDN(5),QSTA                                                  
         MVC   EZBKIDN+5(3),QNET                                                
         SPACE                                                                  
VK840    MVC   SVKEY,KEY                                                        
         MVC   DATADISP,=H'42'                                                  
*                                                                               
VKXIT    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              VALIDATE RECORD ROUTINE                                          
         SPACE                                                                  
VREC     EQU   *                                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         LA    R6,ELEM                                                          
         USING EZBDTAEL,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   EZBDTAEL,X'10'                                                   
         MVI   EZBDTALN,EZBDTAX-EZBDTAEL                                        
         SPACE                                                                  
         LA    R2,LINBILLH         BILL Y OR N                                  
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'Y'                                                       
         BE    VR100                                                            
         CLI   8(R2),C'N'                                                       
         BNE   BADBILL                                                          
VR100    MVC   EZBBILL,8(R2)                                                    
         SPACE                                                                  
         LA    R2,LINFRMTH         BILL FORMAT                                  
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BE    VR170                                                            
         SPACE                                                                  
         LA    R0,FMTABLEN                                                      
         LA    R1,FMTABLE                                                       
         CLI   5(R2),2                                                          
         BNE   BADFRMT                                                          
         SPACE                                                                  
VR160    CLC   0(2,R1),8(R2)                                                    
         BE    VR164                                                            
         LA    R1,2(,R1)                                                        
         BCT   R0,VR160                                                         
         B     BADFRMT                                                          
VR164    MVC   EZBFRMT,8(R2)                                                    
         SPACE                                                                  
VR170    LA    R2,LINLINKH         LINKS IDS TOGETHER FOR BILLING               
         CLI   5(R2),0                                                          
         BE    VR190                                                            
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   EZBLINK,WORK                                                     
         MVC   SVKEY,KEY                                                        
         LA    R4,KEY                                                           
         USING EZBILLD,R4                                                       
         MVC   EZBKIDN,EZBLINK                                                  
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADLINK                                                          
         MVC   KEY,SVKEY                                                        
         SPACE                                                                  
VR190    LA    R2,LINARCDH         SOL A/R SYSTEM CODE                          
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   5(R2),7                                                          
         BE    BADARCD                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   EZBARCD,WORK                                                     
         SPACE                                                                  
         LA    R2,LINCSTCH         SOL A/R SYSTEM CUST CODE                     
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   5(R2),7                                                          
         BE    BADCSTC                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   EZBCSTC,WORK                                                     
         SPACE                                                                  
         LA    R2,LININACH                                                      
         CLI   5(R2),0                                                          
         BE    VR191                                                            
         GOTO1 ANY                                                              
         MVC   EZBINAC,WORK                                                     
         SPACE                                                                  
VR191    LA    R2,LINBNAMH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   EZBNAME,8(R2)                                                    
         OC    EZBNAME,SPACES                                                   
         SPACE                                                                  
         LA    R2,LINBATTH                                                      
         CLI   5(R2),0                                                          
         BE    VR194                                                            
         MVC   EZBATTN,8(R2)                                                    
         OC    EZBATTN,SPACES                                                   
         SPACE                                                                  
VR194    LA    R2,LINBAD1H                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   EZBADR1,8(R2)                                                    
         OC    EZBADR1,SPACES                                                   
         SPACE                                                                  
         LA    R2,LINBAD2H                                                      
         CLI   5(R2),0                                                          
         BE    VR196                                                            
         MVC   EZBADR2,8(R2)                                                    
         OC    EZBADR2,SPACES                                                   
         SPACE                                                                  
VR196    LA    R2,LINBCTYH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   EZBCITY,8(R2)                                                    
         OC    EZBCITY,SPACES                                                   
         SPACE                                                                  
         LA    R2,LINBSTH                                                       
         CLI   5(R2),2                                                          
         BNE   BADST                                                            
         LA    R0,STABLEN                                                       
         LA    R1,STABLE                                                        
VR200    CLC   0(2,R1),8(R2)                                                    
         BE    VR210                                                            
         LA    R1,2(,R1)                                                        
         BCT   R0,VR200                                                         
         B     BADST                                                            
         SPACE                                                                  
VR210    MVC   EZBST,8(R2)                                                      
         SPACE                                                                  
         LA    R2,LINBZIPH                                                      
         LA    R0,EZBZIP                                                        
         LA    R1,5                                                             
         LA    R3,BADZIP                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
         LA    R2,LINTELAH                                                      
         CLI   5(R2),0                                                          
         BE    VR300                                                            
         LA    R0,EZBTELA                                                       
         LA    R1,3                                                             
         LA    R3,BADTEL                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR300    LA    R2,LINTELEH                                                      
         CLI   5(R2),0                                                          
         BNE   VR320                                                            
         OC    EZBTELA,EZBTELA     WAS AREA CODE ENTERED                        
         BZ    VR340                                                            
VR320    LA    R0,EZBTELE                                                       
         LA    R1,3                                                             
         LA    R3,BADTEL                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR340    LA    R2,LINTELNH                                                      
         CLI   5(R2),0                                                          
         BNE   VR360                                                            
         OC    EZBTELE,EZBTELE     WAS EXCHANGE ENTERED                         
         BZ    VR380                                                            
VR360    LA    R0,EZBTELN                                                       
         LA    R1,4                                                             
         LA    R3,BADTEL                                                        
         SR    RF,RF                                                            
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR380    LA    R2,LINTELXH                                                      
         CLI   5(R2),0                                                          
         BE    VR400                                                            
         LA    R0,EZBTELX                                                       
         ZIC   R1,5(R2)                                                         
         LA    R3,BADTEL                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR400    LA    R2,LINFAXAH                                                      
         CLI   5(R2),0                                                          
         BE    VR410                                                            
         LA    R0,EZBFAXA                                                       
         LA    R1,3                                                             
         LA    R3,BADTEL                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR410    LA    R2,LINFAXEH                                                      
         CLI   5(R2),0                                                          
         BNE   VR420                                                            
         OC    EZBFAXA,EZBFAXA     WAS AREA CODE ENTERED                        
         BZ    VR440                                                            
VR420    LA    R0,EZBFAXE                                                       
         LA    R1,3                                                             
         LA    R3,BADFAX                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR440    LA    R2,LINFAXNH                                                      
         CLI   5(R2),0                                                          
         BNE   VR460                                                            
         OC    EZBFAXE,EZBFAXE     WAS EXCHANGE ENTERED                         
         BZ    VR480                                                            
VR460    LA    R0,EZBFAXN                                                       
         LA    R1,4                                                             
         LA    R3,BADFAX                                                        
         SR    RF,RF                                                            
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR480    LA    R2,LINFAXXH                                                      
         CLI   5(R2),0                                                          
         BE    VR490                                                            
         LA    R0,EZBFAXX                                                       
         ZIC   R1,5(R2)                                                         
         LA    R3,BADFAX                                                        
         BAS   RE,CKNUM                                                         
         SPACE                                                                  
VR490    MVC   DATADISP,=H'42'                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         SPACE                                                                  
         LA    R2,LINMED1H         MEDIA                                        
         LA    R3,4                                                             
         SPACE                                                                  
VR500    CLI   5(R2),0                                                          
         BNE   VR504                                                            
         SPACE                                                                  
         LR    RF,R2                                                            
         ZIC   R0,0(R2)            MOVE TO RATE FIELD                           
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR540                                                            
         LR    R2,RF                                                            
         B     MISMEDER                                                         
         SPACE                                                                  
VR504    CLI   5(R2),1                                                          
         BH    BADMED                                                           
         SPACE                                                                  
         LA    R0,MEDTABLN                                                      
         LA    R1,MEDTABLE                                                      
VR510    CLC   8(1,R2),0(R1)                                                    
         BE    VR520                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VR510                                                         
         B     BADMED                                                           
         SPACE                                                                  
VR520    MVC   BYTE,8(R2)                                                       
         ZIC   R0,0(R2)            MOVE TO RATE FIELD                           
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            MOVE TO RATE FIELD                           
         AR    R2,R0                                                            
         SPACE                                                                  
         CLI   5(R2),0             WAS MEDIA, MUST BE RATE                      
         BE    MISSERR                                                          
         SPACE                                                                  
         CLI   5(R2),1                                                          
         BNE   BADRATE                                                          
         SPACE                                                                  
         LA    R0,RATABLEN                                                      
         LA    R1,RATABLE                                                       
VR524    CLC   0(1,R1),8(R2)                                                    
         BE    VR526                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VR524                                                         
         B     BADRATE                                                          
         SPACE                                                                  
VR526    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EZBMEDEL,R6                                                      
         MVI   EZBMEDEL,X'20'                                                   
         MVI   EZBMEDLN,EZBMEDX-EZBMEDEL                                        
         MVC   EZBMED,BYTE                                                      
         MVC   EZBMEDRT,8(R2)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         SPACE                                                                  
VR540    ZIC   R0,0(R2)            MOVE TO NEXT MEDIA FIELD                     
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R3,VR500                                                         
         SPACE                                                                  
VR900    CLI   ACTNUM,ACTADD                                                    
         BE    VRXIT                                                            
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
VRXIT    B     DREC                                                             
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     EQU   *                                                                
         LA    R4,KEY                                                           
         USING EZBILLD,R4                                                       
         XC    LINTYPE,LINTYPE                                                  
         MVC   LINTYPE(L'EZBKTYP),EZBKTYP                                       
         OI    LINTYPEH+6,X'80'                                                 
         SPACE                                                                  
         XC    LINUID,LINUID       USER ID                                      
         MVC   LINUID(L'EZBKIDN),EZBKIDN                                        
         OI    LINUIDH+6,X'80'                                                  
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
DKXIT    B     XIT                                                              
         SPACE 2                                                                
* DISPLAY RECORD ROUTINE *                                                      
         SPACE                                                                  
DREC     EQU   *                                                                
         LA    R2,LINBILLH                                                      
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZBDTAEL,R6                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBBILL),EZBBILL                                          
         CLC   LINBILL,WORK                                                     
         BE    *+14                                                             
         MVC   LINBILL,WORK                                                     
         OI    LINBILLH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBFRMT),EZBFRMT                                          
         CLC   LINFRMT,WORK                                                     
         BE    *+14                                                             
         MVC   LINFRMT,WORK                                                     
         OI    LINFRMTH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBLINK),EZBLINK                                          
         CLC   LINLINK,WORK                                                     
         BE    *+14                                                             
         MVC   LINLINK,WORK                                                     
         OI    LINLINKH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBCSTC),EZBCSTC                                          
         CLC   LINCSTC,WORK                                                     
         BE    *+14                                                             
         MVC   LINCSTC,WORK                                                     
         OI    LINCSTCH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBARCD),EZBARCD                                          
         CLC   LINARCD,WORK                                                     
         BE    *+14                                                             
         MVC   LINARCD,WORK                                                     
         OI    LINARCDH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBNAME),EZBNAME                                          
         CLC   LINBNAM,WORK                                                     
         BE    *+14                                                             
         MVC   LINBNAM,WORK                                                     
         OI    LINBNAMH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBATTN),EZBATTN                                          
         CLC   LINBATT,WORK                                                     
         BE    *+14                                                             
         MVC   LINBATT,WORK                                                     
         OI    LINBATTH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBADR1),EZBADR1                                          
         CLC   LINBAD1,WORK                                                     
         BE    *+14                                                             
         MVC   LINBAD1,WORK                                                     
         OI    LINBAD1H+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBADR2),EZBADR2                                          
         CLC   LINBAD2,WORK                                                     
         BE    *+14                                                             
         MVC   LINBAD2,WORK                                                     
         OI    LINBAD2H+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBCITY),EZBCITY                                          
         CLC   LINBCTY,WORK                                                     
         BE    *+14                                                             
         MVC   LINBCTY,WORK                                                     
         OI    LINBCTYH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBST),EZBST                                              
         CLC   LINBST,WORK                                                      
         BE    *+14                                                             
         MVC   LINBST,WORK                                                      
         OI    LINBSTH+6,X'80'                                                  
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBZIP),EZBZIP                                            
         CLC   LINBZIP,WORK                                                     
         BE    *+14                                                             
         MVC   LINBZIP,WORK                                                     
         OI    LINBZIPH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBTELA),EZBTELA                                          
         CLC   LINTELA,WORK                                                     
         BE    *+14                                                             
         MVC   LINTELA,WORK                                                     
         OI    LINTELAH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBTELE),EZBTELE                                          
         CLC   LINTELE,WORK                                                     
         BE    *+14                                                             
         MVC   LINTELE,WORK                                                     
         OI    LINTELEH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBTELN),EZBTELN                                          
         CLC   LINTELN,WORK                                                     
         BE    *+14                                                             
         MVC   LINTELN,WORK                                                     
         OI    LINTELNH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBTELX),EZBTELX                                          
         CLC   LINTELX,WORK                                                     
         BE    *+14                                                             
         MVC   LINTELX,WORK                                                     
         OI    LINTELXH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBFAXA),EZBFAXA                                          
         CLC   LINFAXA,WORK                                                     
         BE    *+14                                                             
         MVC   LINFAXA,WORK                                                     
         OI    LINFAXAH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBFAXE),EZBFAXE                                          
         CLC   LINFAXE,WORK                                                     
         BE    *+14                                                             
         MVC   LINFAXE,WORK                                                     
         OI    LINFAXEH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBFAXN),EZBFAXN                                          
         CLC   LINFAXN,WORK                                                     
         BE    *+14                                                             
         MVC   LINFAXN,WORK                                                     
         OI    LINFAXNH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBFAXX),EZBFAXX                                          
         CLC   LINFAXX,WORK                                                     
         BE    *+14                                                             
         MVC   LINFAXX,WORK                                                     
         OI    LINFAXXH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZBINAC),EZBINAC                                          
         CLC   LININAC,WORK                                                     
         BE    *+14                                                             
         MVC   LININAC,WORK                                                     
         OI    LININACH+6,X'80'                                                 
         SPACE                                                                  
         MVI   ELCODE,X'20'                                                     
         LA    R2,LINMED1H                                                      
         LA    R3,4                                                             
         XC    WORK,WORK                                                        
         XC    LINMED1,LINMED1                                                  
         OI    LINMED1H+6,X'80'                                                 
         XC    LINMED2,LINMED1                                                  
         OI    LINMED2H+6,X'80'                                                 
         XC    LINMED3,LINMED1                                                  
         OI    LINMED3H+6,X'80'                                                 
         XC    LINMED4,LINMED1                                                  
         OI    LINMED4H+6,X'80'                                                 
         XC    LINRTE1,LINRTE1                                                  
         OI    LINRTE1H+6,X'80'                                                 
         XC    LINRTE2,LINRTE1                                                  
         OI    LINRTE2H+6,X'80'                                                 
         XC    LINRTE3,LINRTE1                                                  
         OI    LINRTE3H+6,X'80'                                                 
         XC    LINRTE4,LINRTE1                                                  
         OI    LINRTE4H+6,X'80'                                                 
DR200    BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         USING EZBMEDEL,R6                                                      
         SPACE                                                                  
         MVC   WORK(L'EZBMED),EZBMED                                            
         CLC   8(L'LINMED1,R2),WORK                                             
         BE    *+14                                                             
         MVC   8(L'LINMED1,R2),WORK                                             
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         ZIC   R0,0(R2)            MOVE TO RATE FIELD                           
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE                                                                  
         MVC   WORK(L'EZBMEDRT),EZBMEDRT                                        
         CLC   8(L'LINRTE1,R2),WORK                                             
         BE    *+14                                                             
         MVC   8(L'LINRTE1,R2),WORK                                             
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         ZIC   R0,0(R2)            MOVE TO RATE FIELD                           
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE                                                                  
         BCT   R3,DR200                                                         
         SPACE                                                                  
DRX      B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
LIST     EQU   *                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         MVI   CURSYST,C'C'        CONTROL                                      
         GOTO1 VALIFAS             SWITCH                                       
         SPACE                                                                  
LS100    OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS120                                                            
         MVC   KEY(2),=C'ZB'                                                    
*                                                                               
LS120    GOTO1 HIGH                                                             
         CLC   KEY(2),=C'ZB'        KEY ID                                      
         BNE   XIT                                                              
         B     LS220                                                            
         SPACE                                                                  
LS200    GOTO1 SEQ                                                              
         SPACE                                                                  
LS220    CLC   KEY(2),KEYSAVE       KEY ID                                      
         BNE   XIT                                                              
         SPACE                                                                  
* FILTERS HERE                                                                  
         SPACE                                                                  
         LA    R4,KEY                                                           
         CLI   SVTYP,0             FILTERING ON TYPE                            
         BE    LS240                NO                                          
         CLC   SVTYP,EZBKTYP                                                    
         BNE   LS200                                                            
         SPACE                                                                  
LS240    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LR    R4,R6                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   LISTAR,SPACES                                                    
         USING EZBKEY,R4                                                        
         USING EZBDTAEL,R6                                                      
         MVC   LTYP,EZBKTYP                                                     
         MVC   LID,EZBKIDN                                                      
         MVC   LBIL,EZBBILL                                                     
         MVC   LCSTC,EZBCSTC                                                    
         MVC   LLINK,EZBLINK                                                    
         MVC   LNAME,EZBNAME                                                    
LS550    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    MVC   P+2(80),LISTAR                                                   
         MVI   ALLOWLIN,6                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         OC    EZBADR1,EZBADR1                                                  
         BZ    LS610                                                            
         CLC   EZBADR1,SPACES                                                   
         BE    LS610                                                            
         MVC   P+2+LNAME-LISTAR(L'EZBADR1),EZBADR1                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
LS610    OC    EZBADR2,EZBADR2                                                  
         BZ    LS620                                                            
         CLC   EZBADR2,SPACES                                                   
         BE    LS620                                                            
         MVC   P+2+LNAME-LISTAR(L'EZBADR2),EZBADR2                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
LS620    MVC   P+2+LNAME-LISTAR(L'EZBCITY),EZBCITY                              
         LA    R1,P+2+LNAME-LISTAR+L'EZBCITY                                    
LS630    CLI   0(R1),C' '                                                       
         BH    LS634                                                            
         BCT   R1,LS630                                                         
         SPACE                                                                  
LS634    MVC   2(2,R1),EZBST                                                    
         MVC   5(10,R1),EZBZIP                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         OC    EZBATTN,EZBATTN                                                  
         BZ    LS640                                                            
         CLC   EZBATTN,SPACES                                                   
         BE    LS640                                                            
         MVC   P+2+LNAME-LISTAR-8(8),=C'ATTN TO:'                               
         MVC   P+2+LNAME-LISTAR(L'EZBATTN),EZBATTN                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
LS640    MVC   LISTAR,SPACES                                                    
         B     LS200                                                            
         DROP  R6                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         B     XIT                                                              
         SPACE                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         PROTECTED FIELD                              
         BO    CS020                                                            
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         SPACE                                                                  
CKNUM    CLM   R1,1,5(R2)          CHECK LENGTH                                 
         BNER  R3                                                               
         MVC   DUB(5),=C'00000'                                                 
         MVN   DUB(5),8(R2)                                                     
         BCTR  R1,0                                                             
         EX    R1,CKNUMC                                                        
         BNER  R3                                                               
         LR    R3,R0                                                            
         EX    R1,CKNUMM                                                        
         BR    RE                                                               
CKNUMC   CLC   DUB(0),8(R2)                                                     
CKNUMM   MVC   0(0,R3),8(R2)                                                    
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
MISSID   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'* ERROR * ID REQUIRED *'                          
         B     MYERR2                                                           
MISSMED  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * MEDIA REQUIRED *'                       
         B     MYERR2                                                           
MISSTYP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'* ERROR * TYPE REQUIRED *'                        
         B     MYERR2                                                           
BADRATE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDRATEMS),BDRATEMS                                     
         B     MYERR2                                                           
BADFRMT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDFRMTMS),BDFRMTMS                                     
         B     MYERR2                                                           
BADST    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDSTMS),BDSTMS                                         
         B     MYERR2                                                           
BADZIP   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDZIPMS),BDZIPMS                                       
         B     MYERR2                                                           
BADTEL   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDTELMS),BDTELMS                                       
         B     MYERR2                                                           
BADFAX   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDFAXMS),BDFAXMS                                       
         B     MYERR2                                                           
BADBILL  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * BILLABLE Y OR N *'                      
         B     MYERR2                                                           
BADUID   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID USER ID *'                      
         B     MYERR2                                                           
BADMED   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'* ERROR * INVALID MEDIA *'                        
         B     MYERR2                                                           
BADTYP   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADTYPMS),BADTYPMS                                     
         B     MYERR2                                                           
BADLINK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADLNKMS),BADLNKMS                                     
         B     MYERR2                                                           
BADARCD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADARCMS),BADARCMS                                     
         B     MYERR2                                                           
BADCSTC  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADCSTMS),BADCSTMS                                     
         B     MYERR2                                                           
BADINCO  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADINCMS),BADINCMS                                     
         B     MYERR2                                                           
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERR    LA    R2,LINOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
BDMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMEDMS),BDMEDMS                                       
         GOTO1 ERREX2                                                           
MISMEDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MISMEDMS),MISMEDMS                                     
         GOTO1 ERREX2                                                           
         SPACE                                                                  
OPTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * INVALID OPTION *'                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
UIDLENER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID USER ID *'                      
         GOTO1 ERREX2                                                           
         SPACE                                                                  
INVALER  MVI   ERROR,INVALID                                                    
         LA    R2,CONRECH                                                       
         B     TRAPERR                                                          
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
MEDTABLE DC    C'TRNXCA'                                                        
MEDTABLN EQU   *-MEDTABLE                                                       
         SPACE                                                                  
MEDTBL   DC    CL3' A '                                                         
         DC    CL3'TT '                                                         
         DC    CL3'RR '                                                         
         DC    CL3'NN '                                                         
         DC    CL3'XX '                                                         
MEDTBLEN EQU   (*-MEDTBL)/3                                                     
MEDTBENT EQU   3                                                                
         DC    CL3'???'                                                         
RATABLE  DC    C'1234'                                                          
RATABLEN EQU   *-RATABLE                                                        
FMTABLE  DC    C'IS'                                                            
         DC    C'SS'                                                            
FMTABLEN EQU   (*-FMTABLE)/2                                                    
STABLE   DC    C'AL'                                                            
         DC    C'AK',C'AZ',C'AR',C'CA',C'CO',C'CT',C'DC',C'DE',C'FL'            
         DC    C'GA',C'HI',C'IA',C'ID',C'IL',C'IN',C'KS',C'KY',C'LA'            
         DC    C'MA',C'MD',C'ME',C'MI',C'MN',C'MS',C'MO',C'MT',C'NC'            
         DC    C'NE',C'NV',C'NH',C'NJ',C'NY',C'NM',C'OH',C'OK',C'OR'            
         DC    C'PA',C'PR',C'RI',C'SC',C'SD',C'TN',C'TX',C'UT',C'VA'            
         DC    C'VI',C'VT',C'WA',C'WV',C'WI',C'WY'                              
STABLEN  EQU   (*-STABLE)/2                                                     
BADTYPMS DC    C'* ERROR * VALID TYPES ARE A=AGY, S=STA, R=SOURCE *'            
BADLNKMS DC    C'* ERROR * NO LINK ID FOUND *'                                  
BADARCMS DC    C'* ERROR * A/R CODE MUST BE 1-6 CHAR *'                         
BADCSTMS DC    C'* ERROR * CUST CODE MUST BE 1-6 CHAR *'                        
BADINCMS DC    C'* ERROR * INCOME CODE MUST BE 1-6 CHAR *'                      
BDMEDMS  DC    C'* ERROR * MEDIA MUST BE T, R, N, X, OR A *'                    
MISMEDMS DC    C'* ERROR * MEDIA MUST ENTERED IF RATE ENTERED *'                
BDRATEMS DC    C'* ERROR * ENTER 1 CHARACTER RATE CODE *'                       
BDFRMTMS DC    C'* ERROR * ENTER 2 CHARACTER FORMAT CODE *'                     
BDSTMS   DC    C'* ERROR * ENTER 2 CHARACTER STATE CODE *'                      
BDZIPMS  DC    C'* ERROR * ZIP CODE MUST BE 5 DIGITS *'                         
BDTELMS  DC    C'* ERROR * TELEPHONE NUMBER BAD *'                              
BDFAXMS  DC    C'* ERROR * FAX NUMBER BAD *'                                    
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,35,C'EIX BILLING RECORDS'                                     
         SSPEC H2,35,C'-------------------'                                     
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,80,AGYADD                                                     
         SSPEC H3,80,REPORT                                                     
         SSPEC H4,80,RUN                                                        
         SSPEC H6,80,PAGE                                                       
         SSPEC H8,3,C'TYP ID...... BILL  CUST #  LINK     NAME'                 
         SSPEC H9,3,C'--- -------- ----  ------  ----     ----'                 
         DC    X'00'                                                            
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFB3D                                                                      
       ++INCLUDE SPEZFB3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
* SPEZBILL                                                                      
       ++INCLUDE SPEZBILL                                                       
         EJECT                                                                  
*SPEZFWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
ERRFLD   DS    CL1                 ERROR FIELD                                  
CUUID    DS    CL8                 USER ID FILTER                               
CUSTA    DS    CL5                 STATION FILTER                               
OPTNAM   DS    CL8                 START NAME                                   
SVUID    DS    CL8                 USER ID FROM CONTROL SYSTEM                  
SVTYP    DS    CL1                 TYPE                                         
         SPACE 3                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LTYP     DS    CL1                                                              
         DS    CL2                                                              
LID      DS    CL8                                                              
         DS    CL2                                                              
LBIL     DS    CL1                                                              
         DS    CL4                                                              
LCSTC    DS    CL6                                                              
         DS    CL2                                                              
LLINK    DS    CL8                                                              
         DS    CL1                                                              
LNAME    DS    CL30                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPEZF23   05/01/02'                                      
         END                                                                    
