*          DATA SET DMERASE    AT LEVEL 007 AS OF 08/07/19                      
*PHASE DMERASEA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMDDNAME                                                               
*INCLUDE DMSECHK                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'DMERASE - CREATE AND/OR ERASE A DIRECT ACCESS FILE'             
                                                                                
***********************************************************************         
* UPSI   1....... CARD INPUT                                          *         
* UPSI   .1...... DO NOT ASK FOR VERIFICATION                         *         
* UPSI   ..1..... OVERRIDE CPU/DATASPACE VERIFICATION                 *         
* UPSI   ...1.... WRITE=NO (DO NOT ERASE)                             *         
* UPSI   .......1 SYSPRINT OUTPUT WANTED                              *         
*                                                                     *         
* THIS VERSION USES A 22-BIT FILE WITH 48 EXTENTS AS THE STANDARD DTF *         
* R3 = DTF                                                                      
***********************************************************************         
         PRINT NOGEN                                                            
DMERASE  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,DMERASE,WORK=A(DMERWORK),RA,R9                                 
         LA    R3,FILE                                                          
         USING DTFPHD,R3           R3=A(FILE DTF)                               
*                                                                               
         ST    R1,ACMRG            SAVE MVS PARAM ADDRESS                       
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    DMER018                                                          
*                                                                               
         MVI   UPSI,0                                                           
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
DMER015  CLI   0(R1),C'0'                                                       
         BE    DMER016                                                          
         CLI   0(R1),C'1'                                                       
         BNE   DMER018                                                          
         OC    UPSI,0(RF)                                                       
DMER016  LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,DMER015                                                       
*                                                                               
DMER018  L     R1,X'10'(,R0)       GET CPU ID                                   
         L     R1,X'C4'(,R1)       R1=A(SMCA) FROM CVT                          
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
*                                                                               
         L     RC,=V(CPRINT)       RC=PRINTER CONTROL REGISTER                  
         USING DPRINT,RC                                                        
         MVC   TITLE(17),=C'XXX - ERASE FILES'                                  
         MVC   TITLE(3),CPUID                                                   
         TM    UPSI,QPRINT                                                      
         BZ    DMER030                                                          
         MVC   P(15),=C'PARAMETER CARDS'                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=C'---------------'                                        
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS FROM SYSIN OR CONSOLE                          *         
* CHECK FOR SPECIAL CONTROL PARAMETER CARDS                           *         
***********************************************************************         
DMER030  TM    UPSI,QCARDIN        TEST CARD OR CONSOLE INPUT                   
         BZ    DMER034                                                          
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         B     DMER040                                                          
*                                                                               
DMER034  LA    R0,GETFILEL         OUTPUT "ENTER FILE NAME"                     
         GOTO1 =V(LOGIO),DMCB,1,((R0),GETFILE)                                  
         TM    UPSI,QPRINT                                                      
         BZ    DMER036                                                          
         MVC   P(GETFILEL),GETFILE                                              
         GOTO1 =V(PRINTER)         PRINT CONSOLE OUTPUT                         
*                                                                               
DMER036  MVC   CARD,SPACES                                                      
         GOTO1 =V(LOGIO),DMCB,0,(16,CARD)                                       
*                                                                               
DMER040  CLC   CARD(2),=C'/*'      TEST END OF INPUT                            
         BE    DMEOJ                                                            
         CLC   CARD(4),=C'EOJ '                                                 
         BE    DMEOJ                                                            
         CLC   CARD(4),=C'END '                                                 
         BE    DMEOJ                                                            
*                                                                               
DMER041  CLC   CARD(6),=C'DDSIO='  TEST IF SPECIAL DDSIO                        
         BNE   DMER042                                                          
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     DMER060                                                          
*                                                                               
DMER042  CLC   CARD(7),=C'DSPACE=' SET DSPACE IN SSB                            
         BNE   DMER044                                                          
         MVC   DSPACE,CARD+7                                                    
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSBOFFD(1,RF),DSPACE                                    
         B     DMER060                                                          
*                                                                               
DMER044  CLC   CARD(8),=C'OVERRIDE'                                             
         BE    DMER045                                                          
         CLC   CARD(6),=C'FORCE'                                                
         BNE   DMER046                                                          
         CLI   CARD+6,C'Y'                                                      
         BNE   DMER060                                                          
DMER045  OI    UPSI,QFORCE         SET TO OVERRIDE DSPACE INFO                  
         B     DMER060                                                          
*                                                                               
DMER046  CLC   CARD(7),=C'GLOBAL=' GLOBAL=N/Y                                   
         BNE   DMER050                                                          
         MVC   GLOBAL,CARD+7                                                    
         CLI   GLOBAL,YES                                                       
         BNE   DMER048                                                          
         CLI   DSPACE,C' '         IF YES THEN MUST HAVE DSPACE=                
         BNH   DMEERR5             WARNING MESSAGE                              
         B     DMER060                                                          
*                                                                               
DMER048  CLI   GLOBAL,NO           SET OVERRIDE IF GLOBAL=N                     
         BNE   DMER060                                                          
         OI    UPSI,QFORCE         SET TO OVERRIDE DSPACE INFO                  
         B     DMER060                                                          
*                                                                               
DMER050  CLC   CARD(7),=C'CTLOCK=' LOCK=YES/NO                                  
         BNE   DMER052                                                          
         CLI   CARD+7,C'Y'                                                      
         BNE   DMER060                                                          
         CLI   DSPACE,C' '         IF YES THEN MUST HAVE DSPACE=                
         BNH   DMEERR6                                                          
         MVC   MINORNQ(1),DSPACE   SET DSPACE VALUE IN ENQ                      
         MVC   MINORNQ+2(6),=C'CTRCVR'                                          
         MVI   MINORNQL,8                                                       
         MVI   GLOBAL,C'C'         GLOBAL = CT LOCK                             
         B     DMER060                                                          
*                                                                               
DMER052  DS    0H                                                               
*&&UK                                                                           
         CLC   CARD(7),=C'MZLOCK=' MZLOCK=YES/NO                                
         BNE   DMER053                                                          
         CLI   CARD+7,C'Y'                                                      
         BNE   DMER060                                                          
         CLI   DSPACE,C' '         IF YES THEN MUST HAVE DSPACE=                
         BNH   DMEERR6                                                          
         MVC   MINORNQ(1),DSPACE   SET DSPACE VALUE IN ENQ                      
         MVC   MINORNQ+2(7),=C'MEDRCVZ'                                         
         MVI   MINORNQL,9                                                       
         MVI   GLOBAL,C'Z'         GLOBAL = MZ LOCK                             
         B     DMER060                                                          
DMER053  EQU   *                                                                
*&&                                                                             
         CLC   CARD(6),=C'WRITE='  WRITE=NO/YES                                 
         BE    *+14                                                             
         CLC   CARD(6),=C'ERASE='  ERASE=NO/YES                                 
         BNE   DMER056                                                          
         CLI   CARD+6,C'N'                                                      
         BNE   DMER060             YES IS DEFAULT.SO DO NOTHING                 
*                                                                               
DMER054  OI    UPSI,QNOWRITE       SET WRITE=NO                                 
         CLC   CARD+9(2),=C'00'                                                 
         BL    DMER060                                                          
         CLC   CARD+9(2),=C'99'                                                 
         BH    DMER060                                                          
         PACK  DUB,CARD+9(2)       WRITE=NO NN WHERE NN IS NUM OF SECS          
         CVB   R0,DUB                                                           
         STH   R0,WRNOWAIT         SET NUM OF SECS TO SIMULATE ERASE            
         B     DMER060                                                          
*                                                                               
DMER056  CLC   CARD(5),=C'BIG=YES' BIG=YES NO LONGER REQUIRED                   
         BE    DMER060             DO NOTHING                                   
*                                                                               
         CLC   =C'ABEND=',CARD                                                  
         BNE   DMER058                                                          
         CLI   CARD+6,YES                                                       
         BNE   DMER060                                                          
         MVI   ABENDFLG,YES                                                     
         B     DMER060                                                          
*                                                                               
DMER058  CLC   CARD(4),=C'UPSI'                                                 
         BNE   DMER090                                                          
         MVI   UPSI,0                                                           
         CLI   CARD+5,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'80'                                                       
         CLI   CARD+6,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'40'                                                       
         CLI   CARD+7,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'20'                                                       
         CLI   CARD+8,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'10'                                                       
         CLI   CARD+09,C'1'                                                     
         BNE   *+8                                                              
         OI    UPSI,X'08'                                                       
         CLI   CARD+10,C'1'                                                     
         BNE   *+8                                                              
         OI    UPSI,X'04'                                                       
         CLI   CARD+11,C'1'                                                     
         BNE   *+8                                                              
         OI    UPSI,X'02'                                                       
         CLI   CARD+12,C'1'                                                     
         BNE   *+8                                                              
         OI    UPSI,X'01'                                                       
*                                                                               
DMER060  TM    UPSI,QPRINT         TEST IF WANT SYSPRINT OUTPUT                 
         BZ    DMER030                                                          
         MVC   P(16),CARD          MOVE INPUT TO PRINT LINE                     
         GOTO1 =V(PRINTER)         PRINT CARD/CONSOLE INPUT                     
         B     DMER030             BACK FOR NEXT INPUT                          
         EJECT                                                                  
***********************************************************************         
* INPUT IS A FILE NAME WITH OPTIONAL =ACTION                          *         
***********************************************************************         
DMER090  TM    UPSI,QPRINT         TEST IF WANT SYSPRINT OUTPUT                 
         BZ    DMER096                                                          
         MVI   P,C' '              SET START OF NEW FILE                        
         MVC   P+1(7),P                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),CARD          MOVE INPUT TO PRINT LINE                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DMER096  CLC   CARD(4),=C'SYS='    ONLY APPLIES TO RECOVERY FOR NOW             
         BNE   DMER100                                                          
         LA    RF,12                                                            
         LA    RE,CARD+4           LOOK FOR RCV OR REQ                          
DMER097  CLI   0(RE),C','                                                       
         BE    DMER098                                                          
         CLI   0(RE),C'='                                                       
         BE    DMER098                                                          
         AHI   RE,1                                                             
         BCT   RF,DMER097                                                       
         B     DMEERR2                                                          
*                                                                               
DMER098  LA    RF,X'24'           LOCATE RECOVERY FILE                          
         CLC   =C'RCV',1(RE)                                                    
         BE    DMER099                                                          
         LA    RF,X'22'           LOCATE REQUEST FILE                           
         CLC   =C'REQ',1(RE)                                                    
         BNE   DMEERR2                                                          
*                                                                               
DMER099  MVC   0(5,RE),SPACES                                                   
         GOTOR =V(DMDDNAME),DMCB,((RF),DDNAME),CARD,0                           
         TM    8(R1),X'10'         WAS IT FOUND?                                
         JO    *+2                                                              
         XR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         MVC   FILEDATA,0(RE)                                                   
*                                                                               
         MVC   FILEDD,DDNADDN      SET DDNAME INTO FILEDD                       
         MVI   TYPE,QOLD           FILE=OLD (DEFAULT)                           
         MVI   DISPTYPE,C' '                                                    
         B     DMER110                                                          
*                                                                               
DMER100  LA    RE,CARD             VALIDATE FILENAME AND ACTION                 
         LA    R0,9                                                             
         MVC   FILEDD,SPACES                                                    
         LA    RF,FILEDD                                                        
DMER105  CLI   0(RE),C' '          SCAN TO END OF FILE NAME                     
         BE    DMER106                                                          
         CLI   0(RE),C'='                                                       
         BE    DMER106                                                          
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DMER105                                                       
         B     DMEERR1                                                          
*                                                                               
DMER106  LA    RF,9                FILE NAME 3 THRU 8 CHRS                      
         SR    RF,R0                                                            
         CHI   RF,3                                                             
         BL    DMEERR1                                                          
         ST    RE,SVRE             SAVE LOCATION POINT TO IN CARD               
         BRAS  RE,GETDSN                                                        
         L     RE,SVRE             RESTORE LOACAITON IN CARD                    
*                                                                               
         MVI   TYPE,QOLD           FILE=OLD (DEFAULT)                           
         MVI   DISPTYPE,C' '                                                    
         CLC   0(8,RE),SPACES                                                   
         BE    DMER108                                                          
         CLC   0(8,RE),=CL8'=OLD'                                               
         BE    DMER108                                                          
         MVI   TYPE,QVTOC          FILE=VTOC (NEW FILE FOR VTOC ONLY)           
         MVI   DISPTYPE,C'V'                                                    
         CLC   0(8,RE),=CL8'=VTOC'                                              
         BE    DMER108                                                          
         MVI   TYPE,QNEW           FILE=NEW (NEW FILE TO BE ERASED)             
         MVI   DISPTYPE,C'N'                                                    
         CLC   0(8,RE),=CL8'=NEW'                                               
         BNE   DMEERR1                                                          
DMER108  MVC   0(8,RE),SPACES      CLEAR =ACTION VALUE ON INPUT                 
*                                                                               
DMER110  MVI   DTFSNUM,0           SET SENUM UNKNOWN                            
         MVI   DTFXNUM,0           SET EXTERNAL FILE NUMBER UNKNOWN             
         MVC   DTFDD,FILEDD        SET DDNAME                                   
*                                                                               
         XC    FILEDATA,FILEDATA   GET FILE DATA FOR EXISTING FILE              
         CLI   TYPE,QVTOC                                                       
         BE    DMER120                                                          
         CLI   TYPE,QNEW                                                        
         BE    DMER120                                                          
         GOTO1 =V(DMDDNAME),PLIST,(X'00',DDNAME),FILEDD,0                       
         TM    8(R1),X'10'                                                      
         BO    DMER120             FILE DDNAME NOT FOUND                        
         XR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         MVC   FILEDATA,0(RE)                                                   
*                                                                               
         MVC   DYNDSN,SPACES                                                    
         MVC   DYNDSN(L'DDNADSN),DDNADSN                                        
         MVC   DTFSNUM,DDNASENO    SET SENUM                                    
         MVC   DTFXNUM,DDNAFINO    SET EXTERNAL FILE NUMBER                     
*                                                                               
DMER120  TM    UPSI,QNOVERI        ASK FOR ERASE VERIFICATION                   
         BO    DMER130                                                          
         MVC   CHKFILE,SPACES                                                   
         MVC   CHKFILE(L'DYNDSN),DYNDSN  FILE IN FILE NAME IN MESSAGE           
         LA    RE,CHKFILE+L'DYNDSN                                              
         LA    RF,L'DYNDSN                                                      
DMER123  CLI   0(RE),C' '                                                       
         BH    DMER124                                                          
         SHI   RE,1                                                             
         BCT   RF,DMER123                                                       
         DC    H'00'                                                            
*                                                                               
DMER124  AHI   RE,1                                                             
         MVC   0(L'ABOUTTO,RE),ABOUTTO                                          
         LA    R0,CHKFILEL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),CHKFILE)                                  
         TM    UPSI,QPRINT                                                      
         BZ    DMER125                                                          
         MVC   P(CHKFILEL),CHKFILE                                              
         GOTO1 =V(PRINTER)         PRINT CONSOLE OUTPUT                         
*                                                                               
DMER125  MVC   ANSWER,SPACES                                                    
         GOTO1 =V(LOGIO),DMCB,0,(8,ANSWER)                                      
         TM    UPSI,QPRINT                                                      
         BZ    DMER128                                                          
         MVC   P(8),ANSWER         MOVE REPLY TO PRINT LINE                     
         GOTO1 =V(PRINTER)         PRINT CONSOLE OUTPUT                         
*                                                                               
DMER128  CLC   ANSWER(1),=C'ERASE'                                              
         BE    DMER130                                                          
         CLC   ANSWER(1),=C'IGNORE'                                             
         BE    DMER030                                                          
         CLC   ANSWER(1),=C'CANCEL'                                             
         BE    DMEOJ                                                            
         CLC   ANSWER(1),=C'EOJ '                                               
         BE    DMEOJ                                                            
         CLC   ANSWER(1),=C'END '                                               
         BE    DMEOJ                                                            
         LA    R0,ERRANSWL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),ERRANSW)                                  
         TM    UPSI,QPRINT                                                      
         BZ    DMER125                                                          
         MVC   P(ERRANSWL),ERRANSW                                              
         GOTO1 =V(PRINTER)         PRINT CONSOLE OUTPUT                         
         B     DMER125                                                          
         EJECT                                                                  
***********************************************************************         
* OPEN FILE AND APPLY MAINTENANCE LOCK IF OK TO ERASE GLOBAL FILE     *         
***********************************************************************         
DMER130  NI    DTFFLAG,255-DTFGLOB SET LOCAL FILE                               
         CLI   GLOBAL,C'N'                                                      
         BE    *+8                                                              
         OI    DTFFLAG,DTFGLOB     SET GLOBAL FILE IF GLOBAL=Y INPUT            
*                                                                               
         NI    DTFOPEN,255-DTF_RO                                               
         TM    UPSI,QNOWRITE       READ-ONLY                                    
         BZ    DMER140                                                          
         OI    DTFOPEN,DTF_RO      SET READ-ONLY FLAG ON                        
*                                                                               
DMER140  DS    0H                  OPEN FILE                                    
         GOTO1 =V(DADDS),P1,A(DAOPEN),,,DTF                                     
         XC    TRACKS,TRACKS                                                    
         CLI   TYPE,QVTOC          TEST IF ONLY CREATING VTOC ENTRY             
         BE    DMER180                                                          
         GOTO1 =V(DADDS),P1,A(DACPUID),DISKREC,0,DTF,DISKADR                    
         XC    P3(4),P3                                                         
         SR    R0,R0               COUNT NUMBER OF TRACKS IN FILE               
         SR    R1,R1                                                            
*                                                                               
         LA    RF,DTFXTMAX                                                      
         LA    RE,DMTX                                                          
         USING EXTENTD,RE                                                       
         TM    DIND,DINDXAM        TEST HIGH CORE EXTENT                        
         BZ    DMER144                                                          
         ICM   RE,15,DMTX                                                       
         SAM31                                                                  
*                                                                               
DMER144  CLI   0(RE),X'FF'         TEST LAST EXTENT ENTRY                       
         BE    DMER148                                                          
         ICM   R0,3,EXT#TRKS                                                    
         AR    R1,R0                                                            
         LA    RE,EXTLNQ(RE)                                                    
         BCT   RF,DMER144                                                       
         DROP  RE                                                               
*                                                                               
DMER148  CLI   0(RE),X'FF'         TEST VALID EXTENT MATRIX                     
         SAM24                                                                  
         BNE   DMEERR4                                                          
         ST    R1,TRACKS                                                        
         CLI   TYPE,QNEW           TEST IF CREATING/ERASING A NEW FILE          
         BE    DMER170                                                          
*                                                                               
DMER150  CLI   GLOBAL,C'N'         CALL V(DMSECHK) TO CHECK OK TO ERASE         
         BE    DMER170                                                          
         CLI   GLOBAL,C'C'         CTLOCK=YES                                   
         BE    DMER156                                                          
         CLI   GLOBAL,C'Z'         MZLOCK=YES                                   
         BNE   DMER160                                                          
*                                                                               
DMER156  ISGENQ REQUEST=OBTAIN,QNAME=MAJORNQ,RNAME=MINORNQ,            X        
               RNAMELEN=MINORNQL,SCOPE=SYSTEMS,CONTROL=EXCLUSIVE,      X        
               ENQTOKEN=TOKNTOKN,RESLIST=NO,RNL=NO,COND=YES,           X        
               CONTENTIONACT=WAIT,WAITTYPE=SUSPEND,RETCODE=15,RSNCODE=0         
         LTR   RF,RF                                                            
         BZ    DMER158                                                          
         CIJE  RF,4,DMER158        ISGENQRC_WARN                                
         DC    H'0'                                                             
*                                                                               
DMER158  NILH  GR0,X'0000'                                                      
         CHI   R0,ISGENQRSN_UNPROTECTEDQNAME                                    
         JE    DMER170             WE GOT IT, SAFE TO ERASE NOW                 
         DC    H'0'                                                             
*                                                                               
MAJORNQ  DC    CL8'DDSENQ'         MAJOR NAME                                   
MINORNQ  DC    C'X.XXXXXXX'        LOCK ID FOR CTRCVR ERASE                     
MINORNQL DC    AL1(8)                                                           
TOKNTOKN DC    CL32' '                                                          
                                                                                
**********************************************************************          
* USE DMSECHK TO LOCK FILE                                                      
**********************************************************************          
DMER160  LA    R0,0                                                             
         TM    UPSI,QFORCE                                                      
         BZ    *+8                                                              
         LA    R0,X'20'                                                         
         GOTO1 =V(DMSECHK),DMCB,((R0),=C'DMSLCK'),FILEDD,(R3),(1,0)             
         L     RE,12(R1)                                                        
         MVC   MSG,0(RE)                                                        
         MVC   RESULT,12(R1)                                                    
         MVC   RESERR,16(R1)                                                    
         CLI   RESULT,QIGNORE      0,1,2 OK TO LOAD IS FILE                     
         BL    DMER170                                                          
         CLI   RESULT,QCANCEL                                                   
         BE    DMEERR3             OPERATOR CANCEL                              
         MVI   CCODE,4                                                          
         TM    UPSI,QPRINT         TEST IF WANT SYSPRINT OUTPUT                 
         BZ    DMER170                                                          
         MVC   P(L'MSG),MSG        OPERATOR/AUTO IGNORE                         
         GOTO1 =V(PRINTER)                                                      
                                                                                
***********************************************************************         
* ERASE FILE, RELEASE MAINTENANCE LOCK, AND CLOSE FILE                *         
***********************************************************************         
DMER170  XC    P1(24),P1           ERASE FILE                                   
         L     RE,=A(DMERBUFF)                                                  
         ST    RE,P2                                                            
         MVI   P2,X'FF'                                                         
         ST    R3,P4                                                            
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         TM    UPSI,QNOWRITE       TEST FOR WRITE=NO                            
         BZ    DMER172                                                          
         MVI   RESULT,QNOTDONE                                                  
         SR    R1,R1                                                            
         ICM   R1,3,WRNOWAIT       TEST IF WAIT FOR NN SECS ENTERED             
         BZ    DMER174                                                          
         MHI   R1,100                                                           
         ST    R1,DUB              WAIT TO SIMULATE DOING AN ERASE              
         STIMER WAIT,BINTVL=DUB                                                 
         B     DMER174                                                          
*                                                                               
DMER172  GOTO1 =V(DADDS),P1,A(WTERASE)                                          
         NI    P3+1,X'FB'                                                       
         OC    P3(2),P3                                                         
         BZ    *+8                                                              
         MVI   RESULT,QDSKERR                                                   
*                                                                               
DMER174  CLI   GLOBAL,C'N'         CALL V(DMSECHK) TO RELEASE LOCK              
         BE    DMER180                                                          
         CLI   GLOBAL,C'C'         IF CTLOCK RELEASE ENQ ON X.CTRCVR            
         BE    DMER176                                                          
         CLI   GLOBAL,C'Z'         IF MZLOCK RELEASE ENQ ON X.MEDRCVZ           
         BE    DMER176                                                          
         GOTO1 =V(DMSECHK),DMCB,(X'00',=C'DMSUNL')                              
         B     DMER180                                                          
*                                                                               
DMER176  ISGENQ REQUEST=RELEASE,ENQTOKEN=TOKNTOKN,                     X        
               COND=YES,RETCODE=15,RSNCODE=0                                    
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         CLI   GLOBAL,C'C'         IF CTLOCK RELEASE ENQ ON X.CTRCVR            
         BNE   *+8                                                              
         BRAS  RE,CTRESET          RESET CONTROL DSPACE                         
*&&UK                                                                           
         CLI   GLOBAL,C'Z'                                                      
         BNE   *+8                                                              
         BRAS  RE,MZRESET          RESET MEDZ DSPACE                            
*&&                                                                             
DMER180  EQU   *                   CLOSE FILE                                   
         CLI   RESULT,QIGNORE                                                   
         BNH   DMER190                                                          
         CLI   RESULT,QNOTDONE                                                  
         BE    DMER190                                                          
         GOTO1 =V(DADDS),P1,A(DACLOSE)                                          
         CLI   RESULT,QDSKERR                                                   
         BE    DMEERR4                                                          
                                                                                
***********************************************************************         
* OUTPUT FILE ERASED OK MESSAGE                                       *         
***********************************************************************         
DMER190  MVC   MSG,SPACES          SET UP ERASED OK MESSAGE                     
         MVC   MSG1,SPACES                                                      
         MVC   MSG(L'ERSFILE),ERSFILE                                           
         MVC   MSG(8),FILEDD                                                    
         LA    RE,MSG+L'ERSFILE                                                 
         CLI   RESULT,QNOTDONE     TEST FOR WRITE=NO                            
         BNE   DMER200                                                          
         MVC   MSG,SPACES          SET UP NOT ERASED (WRITE=NO) MSG             
         MVC   MSG(L'NERFILE),NERFILE                                           
         MVC   MSG(8),FILEDD                                                    
         LA    RE,MSG+L'NERFILE                                                 
*                                                                               
DMER200  L     R0,TRACKS           SET UP NUMBER OF TRACKS                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(18,RE),=C' - TRACKS=NNNNNNNN'                                  
         UNPK  10(8,RE),DUB+4(4)                                                
         LA    RE,18(RE)                                                        
         MVC   0(7,RE),=C' - DSN='                                              
         MVC   7(26,RE),DYNDSN     OUTPUT DSN                                   
*                                                                               
         GOTO1 =V(DADDS),P1,A(DACLOSE)                                          
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(80,MSG)                              
         TM    UPSI,QPRINT                                                      
         BZ    DMER030                                                          
         MVC   P(L'MSG+L'MSG1),MSG                                              
         GOTO1 =V(PRINTER)                                                      
         B     DMER030                                                          
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
DMEERR1  MVC   MSG,SPACES          INVALID FILE NAME                            
         MVC   MSG(ERRFILL),ERRFIL                                              
         MVC   MSG(16),CARD                                                     
         MVI   CCODE,8                                                          
         B     DMEERRX                                                          
*                                                                               
DMEERR2  MVC   MSG,SPACES          INVALID OR MISSING PARM ON SYS=              
         MVC   MSG(ERRSYSPL),ERRSYSP                                            
         MVC   MSG(16),CARD                                                     
         MVI   CCODE,8                                                          
         B     DMEERRX                                                          
*                                                                               
DMEERR3  MVC   ABEND,=AL2(660)     OPERATOR ENTERED CANCEL OR EOJ               
         B     DMEERRX                                                          
*                                                                               
DMEERR4  MVC   MSG,SPACES          FILE ERASE FAILURE                           
         MVC   MSG(ERRWRTL),ERRWRT                                              
         MVC   MSG(8),FILEDD                                                    
         MVC   ABEND,=AL2(661)                                                  
         B     DMEERRX                                                          
*                                                                               
DMEERR5  TM    UPSI,QPRINT         TEST IF WANT SYSPRINT OUTPUT                 
         BZ    DMEERR5A                                                         
         MVC   P(16),CARD          MOVE INPUT TO PRINT LINE                     
         GOTO1 =V(PRINTER)                                                      
DMEERR5A MVC   MSG,SPACES          MISSING DSPACE= CARD                         
         MVC   MSG(WRNDSPCL),WRNDSPC                                            
         MVI   CCODE,8                                                          
         B     DMEERRX                                                          
*                                                                               
DMEERR6  TM    UPSI,QPRINT         TEST IF WANT SYSPRINT OUTPUT                 
         BZ    DMEERR6A                                                         
         MVC   P(16),CARD          MOVE INPUT TO PRINT LINE                     
         GOTO1 =V(PRINTER)                                                      
DMEERR6A MVC   MSG,SPACES          MISSING DSPACE= CARD                         
         MVC   MSG(ABNDSPCL),ABNDSPC                                            
         MVC   ABEND,=AL2(676)                                                  
         B     DMEERRX                                                          
*                                                                               
DMEERRX  GOTO1 =V(LOGIO),DMCB,X'FF000001',(60,MSG)                              
         TM    UPSI,QPRINT                                                      
         BZ    DMEERRX1                                                         
         MVC   P(L'MSG),MSG                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DMEERRX1 CLI   ABENDFLG,YES        ABEND=Y SET                                  
         BNE   DMEERRX2                                                         
         CLI   CCODE,4             ABEND IF CCODE > 4                           
         BNH   DMEERRX2                                                         
         MVC   ABEND,=AL2(699)                                                  
*                                                                               
DMEERRX2 OC    ABEND,ABEND         ABEND IF FATAL ERROR                         
         BZ    DMER030                                                          
         LH    R1,ABEND                                                         
         ABEND (1)                                                              
*                                                                               
DMEOJ    XBASE RC=CCODE,RL=1       EXIT WITH CONDITION CODE                     
         EJECT                                                                  
***********************************************************************         
* GET DSN                                                             *         
***********************************************************************         
GETDSN   NTR1                                                                   
         MVC   DYNDD,FILEDD        GET DDNAME FROM DTF                          
         MVC   DYNDSN,SPACES                                                    
         LA    R1,DYNBLK2                                                       
         ST    R1,DYNBLK1                                                       
         OI    DYNBLK1,X'80'                                                    
         LA    R1,DYNBLK4                                                       
         ST    R1,DYNBLK3                                                       
         LA    R1,DYNBLK5                                                       
         ST    R1,DYNBLK3+4                                                     
         OI    DYNBLK3+4,X'80'                                                  
         MVC   DYNBLK2,=X'1407000000000000000000000000000018000000'             
         LA    R1,DYNBLK3                                                       
         ST    R1,DYNBLK2+8                                                     
         MVC   DYNBLK4(6),=X'000100010008'                                      
         MVC   DYNBLK4+6(8),DYNDD                                               
         MVC   DYNBLK5,SPACES                                                   
         MVC   DYNBLK5(6),=X'000500010020'                                      
         LA    R1,DYNBLK1                                                       
         DYNALLOC                                                               
         MVC   DYNDSN(26),DYNBLK5+6                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RESET CONTROL DSPACE                                                *         
***********************************************************************         
CTRESET  NTR1                                                                   
         XC    DMCB(6*4),DMCB      ENQUIRE ON SE                                
         MVI   DMCB+3,X'0A'                                                     
         OI    DMCB,X'20'                                                       
         GOTO1 =V(LOCKSPC),DMCB                                                 
         L     R2,4(,R1)                                                        
         USING DMSPACED,R2                                                      
         ICM   R2,15,DSPECB                                                     
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SAC   512                                                              
         USING DMSYSHDR,R2                                                      
         MVC   FULL,DSYABUFF                                                    
         LA    R2,DSYDATA                                                       
         LA    R0,16                                                            
CTRESET1 CLI   0(R2),X'A4'         CTRCVR                                       
         BE    CTRESET2                                                         
         LA    R2,32(R2)                                                        
         BCT   R0,CTRESET1                                                      
         DC    H'0'                                                             
*                                                                               
         USING DSFILHDR,R2                                                      
CTRESET2 XC    DSEOF1,DSEOF1       CLEAR EOFS                                   
         XC    DSEOF2,DSEOF2                                                    
*                                                                               
         L     R2,FULL                                                          
         MVC   HALF,4(R2)          SAVE BUFF LEN                                
         XC    0(64,R2),0(R2)      CLEAR HEADER                                 
         LH    R3,HALF                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR WHOLE BUFFER                           
*                                                                               
         SAC   0                                                                
         XIT1                                                                   
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* RESET MEDZ DSPACE                                                   *         
***********************************************************************         
MZRESET  NTR1                                                                   
         XC    DMCB(6*4),DMCB      ENQUIRE ON SE                                
         MVI   DMCB+3,X'14'                                                     
         OI    DMCB,X'20'                                                       
         GOTO1 =V(LOCKSPC),DMCB                                                 
         L     R2,4(,R1)                                                        
         USING DMSPACED,R2                                                      
         ICM   R2,15,DSPECB                                                     
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SAC   512                                                              
         USING DMSYSHDR,R2                                                      
         MVC   FULL,DSYABUFF                                                    
         LA    R2,DSYDATA                                                       
         LA    R0,16                                                            
MZRESET1 CLI   0(R2),X'44'         MEDRCVZ                                      
         BE    MZRESET2                                                         
         LA    R2,32(R2)                                                        
         BCT   R0,MZRESET1                                                      
         DC    H'0'                                                             
*                                                                               
         USING DSFILHDR,R2                                                      
MZRESET2 XC    DSEOF1,DSEOF1       CLEAR EOFS                                   
         XC    DSEOF2,DSEOF2                                                    
*                                                                               
         L     R2,FULL                                                          
         MVC   HALF,4(R2)          SAVE BUFF LEN                                
         XC    0(64,R2),0(R2)      CLEAR HEADER                                 
         LH    R3,HALF                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR WHOLE BUFFER                           
*                                                                               
         SAC   0                                                                
         XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* FILE DTF, CONSTANTS AND LITERALS                                    *         
***********************************************************************         
         DS    0D                                                               
         DC    C'**FILE**'                                                      
FILE     DMDA  DSKXTNT=0,BIG=22BIT                                              
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
ACMRG    DS    A                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
*                                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
*                                                                               
UPSITAB  DC    X'8040201008040201'                                              
CPUID    DC    CL4' '              CPU ID FROM MVS - FORMAT C'SYX '             
WRNOWAIT DC    H'0'                SECS TO WAIT IF WRITE=NO                     
ABEND    DC    H'0'                                                             
ABENDFLG DC    AL1(NO)                                                          
CCODE    DC    X'00'                                                            
         DC    XL3'00'                                                          
UPSI     DC    X'00'               NO DEFAULTS                                  
TYPE     DC    X'00'                                                            
GLOBAL   DS    0C                                                               
*&&UK*&& DC    C'Y'                UK DEFAULTS TO GLOBAL=Y                      
*&&US*&& DC    C'N'                US DEFAULTS TO GLOBAL=N                      
DISPTYPE DC    C' '                                                             
RESULT   DC    X'00'                                                            
RESERR   DC    X'00'                                                            
ACTION   DC    X'00'                                                            
DSPACE   DC    C' '                                                             
DISKADR  DC    X'00000400'         22-BIT TRACK 1/BLOCK 0/RECORD 0              
DDNAME   DC    CL8'DDNAME'                                                      
FILEDD   DC    CL8' '                                                           
TRACKS   DC    F'0'                                                             
*                                                                               
ANSWER   DC    CL8' '                                                           
CARD     DC    CL80' '                                                          
MSG      DC    CL60' '                                                          
MSG1     DC    CL20' '                                                          
*                                                                               
         DS    0F                                                               
FILEDATA DS    0XL(DDNADATL)       RETURNED FILE INFO FROM V(DMDDNAME)          
       ++INCLUDE DMDDNAMED                                                      
*                                                                               
         DS    0F                                                               
SYSINFO  DS    XL64                RETURNED SYSTEM INFO FROM V(LOCKSPC)         
*                                                                               
DISKREC  DC    100X'00'                                                         
SAVEP    DC    CL132' '                                                         
SVRE     DS    A                                                                
*                                                                               
DYNBLK1  DS    F                   DYNALLOC BLOCK TO GET DSN                    
DYNBLK2  DS    XL20                                                             
DYNBLK3  DS    XL8                                                              
DYNBLK4  DS    XL14                                                             
DYNBLK5  DS    XL38                                                             
*                                                                               
DYNDD    DS    CL8                 DDNAME FROM DTF+22(8)                        
DYNDSN   DS    CL32                DSN                                          
*                                                                               
*                                                                               
GETFILE  DC    C'ENTER FILE NAME'                                               
GETFILEL EQU   *-GETFILE                                                        
ABOUTTO  DC    C' IS ABOUT TO BE ERASED - ERASE,IGNORE,CANCEL'                  
CHKFILE  DC    CL70' '                                                          
CHKFILEL EQU   *-CHKFILE                                                        
ERRANSW  DC    C'INVALID REPLY - REINPUT REPLY'                                 
ERRANSWL EQU   *-ERRANSW                                                        
*                                                                               
ERSFILE  DC    C'XXXXXXXX ERASED'                                               
ERSFILEL EQU   *-ERSFILE                                                        
NERFILE  DC    C'XXXXXXXX NOT ERASED (WRITE=NO)'                                
NERFILEL EQU   *-NERFILE                                                        
ERRSYSP  DC    C'XXXXXXXXXXXXXXXX INVALID OR MISSING PARM ON SYS='              
ERRSYSPL EQU   *-ERRSYSP                                                        
ERRFIL   DC    C'XXXXXXXXXXXXXXXX INVALID FILE NAME'                            
ERRFILL  EQU   *-ERRFIL                                                         
ERRWRT   DC    C'XXXXXXXX DISK ERROR ENCOUNTERED DURING ERASE'                  
ERRWRTL  EQU   *-ERRWRT                                                         
WRNDSPC  DC    C'WARNING  MISSING DSPACE= CARD'                                 
WRNDSPCL EQU   *-WRNDSPC                                                        
ABNDSPC  DC    C'*ABEND*  MISSING DSPACE= CARD'                                 
ABNDSPCL EQU   *-ABNDSPC                                                        
         EJECT                                                                  
QOLD     EQU   0                   FILE ALREADY EXISTS                          
QVTOC    EQU   1                   CREATE NEW FILE AND EXIT                     
QNEW     EQU   2                   CREATE NEW FILE AND ERASE IT                 
*                                                                               
QCARDIN  EQU   X'80'               CARD INPUT                                   
QNOVERI  EQU   X'40'               NO VERIFY VIA CONSOLE REQUIRED               
QFORCE   EQU   X'20'               FORCE ERASE DO NOT OUTPUT ERRORS             
QNOWRITE EQU   X'10'               WRITE=NO                                     
QPRINT   EQU   X'01'               SYSPRINT OUTPUT WANTED                       
*                                                                               
QOK      EQU   0                   FILE ERASED                                  
QOKGLOB  EQU   1                   FILE ERASED GLOBAL                           
QIGNORE  EQU   2                   OPERATOR IGNORED ERROR                       
QCANCEL  EQU   3                   OPERATOR CANCELLED                           
QDSKERR  EQU   4                   DISK ERROR DURING ERASE                      
QNOTDONE EQU   5                   FILE NOT ERASED (WRITE=NO)                   
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
DMERBUFF DS    200D                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**WORK**'                                                      
DMERWORK DS    2000D                                                            
*                                                                               
DMERLAST DS    D                                                                
*                                                                               
         DS    0D                                                               
         DC    C'**SSBE**'                                                      
SSB      DC    XL2'00',X'FF',XL5'00'                                            
         DC    XL248'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    C'**UTLE**'                                                      
UTL      DC    256X'00'                                                         
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
* DMXTNTD                                                                       
       ++INCLUDE DMXTNTD                                                        
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
* DMDSYSHDR                                                                     
       ++INCLUDE DMDSYSHDR                                                      
* FASSBOFF                                                                      
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
*                                                                               
         ISGYENQ                                                                
         ISGYCON                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DMERASE   08/07/19'                                      
         END                                                                    
