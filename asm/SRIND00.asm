*          DATA SET SRIND00    AT LEVEL 094 AS OF 05/01/02                      
*PHASE T16500A                                                                  
*&&      SET   SF=Y,TT=N                                                        
         TITLE '$INDFILE - 3270 STRUCTURED FIELD TRANSMISSIONS'                 
***********************************************************************         
***********************************************************************         
** NOTE:                                                                        
**  SECURITY IN THIS PROGRAM IS DERRIVED FROM =NWK                              
**  IF THE USER IS NOT ON A DDS TERMINAL THEN THEY MUST HAVE                    
**  COME THROUGH =NWK TO USE THIS PROGRAM                                       
**                                                                              
**  TIOB IS ASSUMED TO BE USED EXCLUSIVELY BY THIS TERMINAL WITH NO             
**  CHANCE OF SOMELESE'S =NWK SESSION BEING PRESENT.                            
**                                                                              
***********************************************************************         
***********************************************************************         
         PRINT NOGEN                                                            
INDFILE  CSECT                                                                  
         NMOD1 WORKX-WORKD,**INDF**,CLEAR=YES,RR=R2                             
         USING WORKD,RC                                                         
         USING SRPARMD,R1                                                       
         MVC   SRPARS(SRPARX-SRPARS),SRPARM1  SAVE CALLING PARAMS               
*                                                                               
         ST    R2,RELO                                                          
         L     RF,=A(VALOCHRS)                                                  
         AR    RF,R2                                                            
         ST    RF,AVALCHRS                                                      
         L     RF,=A(ASCIICHR)                                                  
         AR    RF,R2                                                            
         ST    RF,AASCII                                                        
         L     RF,=A(PQCHARS)                                                   
         AR    RF,R2                                                            
         ST    RF,APQCHARS                                                      
*                                                                               
         L     RA,SRQATWA                                                       
         USING T165FFD,RA                                                       
*                                                                               
         L     R9,SRQATIA          USED TO READ TWA SAVE STORAGE                
         USING SRSD,R9                                                          
*                                                                               
         L     R3,SRQAUTL                                                       
         USING UTLD,R3                                                          
         NI    TTYPE2,X'FF'-TTYPE2CH     RESET CHAINED WRITE FLAG               
*                                                                               
         MVC   ASYSFAC,SRQASYSF                                                 
         MVC   VCOMFACS,SRQACOMF                                                
         DROP  R1                                                               
*                                                                               
         L     RE,ASYSFAC                                                       
         L     RE,VSSB-SYSFACD(RE) GET SSB ADDRESS                              
         USING SSBD,RE                                                          
         MVC   RECLEN,SSBTWAL                                                   
         DROP  RE                                                               
* ALWAYS, ALWAYS READ TWA 11 BEFORE WRITING IT !                                
         BAS   RE,RDTWA                                                         
*&&SF                                                                           
         TM    TTYPE2,TTYPE2SF     TEST TERMINAL IN SF MODE                     
         BZ    IND10                                                            
         CLC   SV$INDF,=C'$INDF'   TEST $INDF ID                                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MAIN                                                             
*&&                                                                             
*&&TT                                                                           
         CLC   =C'RESET',INDFNM                                                 
         BNE   IND02                                                            
         MVI   SVACTN,0                                                         
         OI    SVFLAGS,SVFDONE                                                  
         MVC   INDOUT(30),=CL30'RESET COMPLETE'                                 
         BAS   RE,WRTWA                                                         
         B     EXIT                                                             
*                                                                               
IND02    CLI   SVACTN,0                                                         
         BE    IND10                                                            
         CLC   SV$INDF,=C'$INDF'   TEST BEEN HERE BEFORE                        
         BE    MAIN                                                             
         B     IND10                                                            
*&&                                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
* START A NEW STRUCTURED FIELD TRANSFER                                         
*----------------------------------------------------------------*              
         SPACE 1                                                                
IND10    MVC   INDOUT(20),=CL20'COMMENCING TRANSFER'                            
         NI    SVFLAGS,X'FF'-SVFOPEN                                            
*                                                                               
         OC    TUSER,TUSER                                                      
         BNZ   *+14                                                             
         MVC   INDOUT(20),=CL20'NOT CONNECTED'                                  
         B     EXIT                                                             
         MVC   DATALEN,TUSER                                                    
*                                                                               
         TM    SVFLAGS,SVFNWKOK    SEQUENTIAL REQUEST?                          
         BNZ   IND12               YES                                          
*                                                                               
         CLC   SR$NWK,=C'T135 '    CHECK =NWK                                   
         BE    IND12               YES WE'RE OK                                 
*                                                                               
         TM    TSTAT1,X'60'        DDS TERMINAL?                                
         BNZ   IND19               YES NO SECURITY CHECK REQ'D                  
*                                                                               
         MVC   INDOUT(20),=CL20'SECURITY LOCKOUT'                               
         B     EXIT                                                             
*                                                                               
IND12    DS    0H                                                               
         LA    RE,SR$NWK                                                        
         AH    RE,=Y(NWKMODE-SR$NWK)                                            
         CLI   0(RE),NWKMDEMO      STRATA?                                      
         BNE   IND13                                                            
*                                                                               
         MVC   DATALEN,=H'6183'     SET TO STRATA ID                            
         B     IND20                                                            
*                                                                               
IND13    DS    0H                                                               
         TM    TSTAT1,X'60'        DDS TERMINAL?                                
         BNZ   IND19               YES NO SECURITY CHECK REQ'D                  
*                                                                               
         MVC   INDOUT(20),=CL20'INVALID NWK OPTION'                             
         B     EXIT                                                             
*                                                                               
IND19    DS    0H                                                               
         LA    RE,SR$NWK                                                        
         AH    RE,=Y(NWKMODE-SR$NWK)                                            
         MVI   0(RE),0                                                          
*                                                                               
IND20    DS    0H                                                               
         LA    RE,SV$INDF          CLEAR SAVE AREA                              
         LA    RF,SVINDLQ                                                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   SV$INDF,=C'$INDF'     SET $INDF ID                               
         MVI   SVACTN,ACTOPEN        SET CMD = READ PARTITION 1 QUERY           
         MVC   SVUSER,DATALEN                                                   
*                                                                               
         CLC   SVUSER,=H'6183'     =NWK OK?                                     
         BNE   *+8                 NO                                           
         OI    SVFLAGS,SVFNWKOK                                                 
*                                                                               
         L     R8,TBUFF                                                         
         SH    R8,=H'8'            POINT TO MESSAGE HEADER                      
         LH    R0,6(R8)            GET MESSAGE LENGTH                           
         LA    R8,8+4(R8)          POINT TO MESSAGE DATA                        
         SPACE 1                                                                
*-------------------------------------------------------------------*           
* EXPECTED MESSAGE SYNTAX IS                                        *           
* SBA XX YY INDFILE GET SBA XX YY XSEQNUM.GARBAGE SBA ...           *          
*-------------------------------------------------------------------*           
         SPACE 1                                                                
         SR    R1,R1                                                            
IND28    CLI   0(R8),SBA           FIND SECOND SBA                              
         BE    IND30                                                            
         LA    R8,1(R8)                                                         
         B     IND28                                                            
*                                                                               
IND30    LTR   R1,R1              IS THIS THE FIRST SBA                         
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         BZ    IND28               NO - FIND ANOTHER                            
*                                                                               
         LA    R2,2(R8)            R2-> FIRST FILENAME BYTE                     
         LA    R8,1(R2)                                                         
         L     R1,TBUFF                                                         
         LR    R4,R8                                                            
         SR    R4,R1               DATA LENGTH SO FAR                           
         LA    R1,1                CHARACTER COUNT                              
*                                                                               
IND40    CR    R4,R0               END OF DATA??                                
         BH    IND50               YES                                          
         CLI   0(R8),SBA           NEXT FIELD?                                  
         BE    IND50               YES                                          
         CLI   0(R8),ETX           END OF MESSAGE?                              
         BE    IND50               YES                                          
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         LA    R8,1(R8)                                                         
         B     IND40                                                            
*                                                                               
IND50    DS    0H                                                               
         MVC   SVARGS,SPACES                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVARGS(0),0(R2)                                                  
*&&TT                                                                           
         MVC   INDOUT+86(30),=CL30'SURVIVED INVALID PARSING'                    
*&&                                                                             
         L     R8,TBUFF                                                         
         MVC   0(WSFLN,R8),WSF         SET WRITE STRUCTURED FIELD               
         LA    R8,WSFLN(R8)                                                     
         MVC   0(RDPARTLN,R8),RDPART   SET READ PARTITION 1 QUERY               
         LA    R8,RDPARTLN(R8)                                                  
         MVC   SVRECNUM,=F'1'          RESET RECORD NUMBER                      
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
*&&SF*&& OI    TTYPE2,TTYPE2SF       SET STRUCTURED FIELD XMT                   
         BAS   RE,WRTWA                                                         
*&&TT                                                                           
         LA    R1,INDOUT                                                        
         LA    R1,86(R1)                                                        
         MVC   0(26,R1),=C'I MAKE THE NAME OUT TO BE'                           
         MVC   27(10,R1),SVARGS                                                 
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
ACTOPEN  EQU   10                                                               
ACTPARSE EQU   15                                                               
ACTINS   EQU   20                                                               
ACTCLOSE EQU   30                                                               
ACTXCOM  EQU   40                                                               
ACTXOK   EQU   ACTXCOM+ACTOK                                                    
ACTXERR  EQU   ACTXCOM+ACTERROR                                                 
ACTROPEN EQU   50                                                               
ACTMSG   EQU   60                                                               
ACTOK    EQU   0                   ACTION COMPLETE SUCCESSFULLY                 
ACTERROR EQU   1                   ACTION COMLETED UNSUCCESSFULLY               
MSGREADY EQU   ACTMSG+ACTOK                                                     
MSGERROR EQU   ACTMSG+ACTERROR                                                  
*                                                                               
MAXSEND  EQU   1900                MAX BYTES TO SEND TO PC                      
         EJECT                                                                  
*----------------------------------------------------------------*              
* MAIN                                                           *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
MAIN     DS    0H                                                               
         L     R8,TBUFF                                                         
*                                                                               
         CLI   SVACTN,ACTOPEN      ACTION OPEN?                                 
         BE    INDOPEN                                                          
         CLI   SVACTN,ACTPARSE     ACTION PARSE?                                
         BE    INDPARSE                                                         
         CLI   SVACTN,ACTINS       ACTION INSERT & INSERTDATA?                  
         BE    INDINS                                                           
         CLI   SVACTN,ACTCLOSE     ACTION CLOSE?                                
         BE    INDCLOSE                                                         
         CLI   SVACTN,ACTROPEN     ACTION OPEN FOR RESET?                       
         BE    INDROPEN                                                         
         CLI   SVACTN,ACTXOK       ACTION TRANSFER COMPLETE?                    
         BE    INDXOK                                                           
         CLI   SVACTN,ACTXERR      ACTION TRANSFER COMPLETE W/ERROR?            
         BE    INDXERR                                                          
         CLI   SVACTN,MSGREADY     ACTION READY MESSAGE?                        
         BE    INDMOK                                                           
         CLI   SVACTN,MSGERROR     ACTION ERROR MESSAGE?                        
         BE    INDMERR                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* HOST OPEN REQUEST                                              *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDOPEN  DS    0H                                                               
         MVI   SVACTN,ACTPARSE     SET ACTION CODE                              
         L     R8,TBUFF                                                         
*                                                                               
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(HOSTOPLN,R8),HOSTOP                                            
         LA    R8,HOSTOPLN(R8)                                                  
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
         BAS   RE,WRTWA                                                         
*&&TT                                                                           
         MVC   INDOUT(20),=CL20'HOST OPEN IN BUFFER'                            
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* PARSE THE PRINTQ FILENAME                                      *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDPARSE DS    0H                                                               
         MVI   SVACTN,ACTINS                                                    
         XC    SVFILNUM,SVFILNUM                                                
         MVI   SVERRCD,INVFILQ                                                  
*                                                                               
         LA    RF,SVARGS+1         SKIP THE FIRST CHAR                          
         LA    R0,SVARGS+5+1       MAX NUMBER LENGTH                            
*                                                                               
PARSE10  DS    0H                                                               
         CLI   0(RF),C'.'          END OF NUMBER?                               
         BE    PARSE12             YES                                          
*                                                                               
         CLI   0(RF),C'0'          NO - MUST BE NUMERIC                         
         BNL   *+8                                                              
         BAS   RE,INDERR                                                        
         CLI   0(RF),C'9'                                                       
         BNH   *+8                                                              
         BAS   RE,INDERR                                                        
*                                                                               
         LA    RF,1(RF)            NEXT CHARACTER                               
         CR    RF,R0               TOO FAR?                                     
         BNH   PARSE10             NO                                           
         BAS   RE,INDERR                                                        
*                                                                               
PARSE12  DS    0H                                                               
         XC    DUB,DUB                                                          
         LA    RE,SVARGS+1                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SVARGS+1(0)                                                  
         CVB   R0,DUB                                                           
         STH   R0,SVFILNUM         SAVE HALFWORD SEQNUM                         
*                                                                               
         LA    RE,SR$NWK                                                        
         AH    RE,=Y(NWKMODE-SR$NWK)                                            
         CLI   0(RE),NWKMDEMO                                                   
         BNE   PARSE14                                                          
*                                                                               
         LR    RF,R0                                                            
         SR    RE,RE                                                            
         D     RE,=F'8'                                                         
         LA    RF,SR$NWK(RF)                                                    
         AH    RF,=Y(NWKBITTB-SR$NWK)                                           
         LA    R1,X'80'                                                         
         SRL   R1,0(RE)                                                         
         STC   R1,BYTE                                                          
         NC    BYTE,0(RF)                                                       
         BNZ   *+12                                                             
         MVI   SVERRCD,INVDEMQ                                                  
         BAS   RE,INDERR                                                        
*                                                                               
PARSE14  DS    0H                                                               
         MVI   SVERRCD,INVFILQ                                                  
         B     INDINS                                                           
         EJECT                                                                  
*----------------------------------------------------------------*              
* INSERT & INSERT DATA                                           *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDINS   DS    0H                                                               
*&&SF                                                                           
         L     R8,TBUFF                                                         
         CLC   =X'880005D00009',4(R8)      OK OPEN REQUEST?                     
         BE    INS00                                                            
         CLC   =X'88000BD047056306',4(R8)  OK INS & INS DATA                    
         BE    INS00                       SOMETHING WENT WRONG                 
         MVI   SVERRCD,OPNERRQ                                                  
         BAS   RE,INDERR                                                        
*&&                                                                             
INS00    DS    0H                  FIRST TIME                                   
         TM    SVFLAGS,SVFOPEN                                                  
         BNZ   INS04                                                            
*                                                                               
         OI    SVFLAGS,SVFOPEN                                                  
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'BUFFER'),=C'WRKFI',KEY,WRKREC,      X        
               CIREC,0                                                          
         MVC   SAVEBDSP,CIREC+8    SAVE DISP TO START OF BUFFER SAVE            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY              GET FILENUM FOR THIS USERID                  
         USING UKRECD,R5                                                        
*                                                                               
         MVC   UKUSRID,SVUSER                                                   
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',GFILE),WRKFIL,KEY,WRKREC,CIREC                  
         MVC   SAVEWKID,UKUSRINF     SAVE WRKF ID FOR USERID                    
*                                                                               
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,SVUSER                                                   
*                                                                               
         OC    SVFILNUM,SVFILNUM   FILE NUMBER KNOWN?                           
         BZ    INS02               NO                                           
*                                                                               
         MVI   UKFLAG,X'80'                                                     
         MVC   UKFILENO,SVFILNUM                                                
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'08',INDEX),SAVEWKID,KEY,WRKREC,CIREC,0              
         CLI   8(R1),0                                                          
         BE    INS100                                                           
         MVI   SVERRCD,FILNTFQ                                                  
         BAS   RE,INDERR                                                        
*                                                                               
INS02    DS    0H                                                               
         MVC   UKSYSPRG,SVSUBID                                                 
         MVC   UKSUBPRG,SVTYPE                                                  
         MVC   UKDAY,SVDAY                                                      
         MVC   UKCLASS,SVCLASS                                                  
*                                                                               
         MVI   SVERRCD,NOINDEX     WE ARE DOOMED                                
         BAS   RE,INDERR                                                        
*                                                                               
*                                                                               
*** NWK CODE INDEX READING CODE HERE IF REQUIRED                                
*                                                                               
*                                                                               
INS04    DS    0H                  NOT THE FIRST TIME                           
         MVC   WRKREC(30),TWASAVA                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'BURSTR'),=C'WRKFI',KEY,             X        
               WRKREC,CIREC,0                                                   
         LA    RE,CIREC                                                         
         A     RE,SAVEBDSP                                                      
         MVC   8(92,RE),TWASAVB+8  RESTORE SAVE AREA LESS FIRST 8 BYTES         
         MVC   FULL,SKADDR-SKBUFFD(RE)                                          
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMREAD'),SAVEWKID,FULL,CIREC                 
*                                                                               
INS100   MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(INSERTLN,R8),INSERT                                            
         LA    R2,10(R8)           LENGTH BYTES                                 
         LA    R8,INSERTLN(R8)                                                  
*                                                                               
         BAS   RE,INDGET                                                        
*                                                                               
         LH    R1,DATALEN          R1 -> LENGTH OF DATALINE                     
         LA    R0,10(R1)           LENGTH OF DATA FOR INSERT                    
         STH   R0,0(R2)                                                         
         LA    R0,5(R1)            LENGTH FOR INSERT DATA                       
         STH   R0,0(R8)                                                         
         LA    R8,2(R1,R8)                                                      
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'BUSAVE'),=C'WRKFI',KEY,             X        
               WRKREC,CIREC,0                                                   
         MVC   TWASAVA,WRKREC      SAVE INDEX DATA                              
         LA    RE,CIREC                                                         
         A     RE,SAVEBDSP                                                      
         MVC   TWASAVB(100),0(RE)  SAVE BUFFER DATA                             
*                                                                               
         BAS   RE,WRTWA                                                         
*&&TT                                                                           
         LA    R0,5                OUTPUT FIRST 5 DATA LINES                    
         LA    R2,INDOUT                                                        
         L     R4,TBUFF                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
*                                                                               
NEXTOUT  DS    0H                                                               
         GOTO1 (RF),DMCB,(R4),(R2),39,=C'TOG'                                   
         LA    R2,86(R2)                                                        
         LA    R4,39(R4)                                                        
         BCT   R0,NEXTOUT                                                       
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* HOST CLOSE REQUEST                                             *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDCLOSE DS    0H                                                               
         MVI   SVACTN,ACTROPEN     SET ACTION CODE                              
         L     R8,TBUFF                                                         
*                                                                               
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(CLOSELN,R8),CLOSE                                              
         LA    R8,CLOSELN(R8)                                                   
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
         BAS   RE,WRTWA                                                         
*&&TT                                                                           
         MVC   INDOUT(20),=CL20'HOST CLOSE REQUEST'                             
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* HOST OPEN FOR RESET REQUEST                                    *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDROPEN DS    0H                                                               
         ZIC   R1,SVMSG                                                         
         LA    R1,ACTXCOM(R1)                                                   
         STC   R1,SVACTN           SET ACTION CODE                              
         L     R8,TBUFF                                                         
*                                                                               
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(HOSTOP2L,R8),HOSTOP2                                           
         LA    R8,HOSTOP2L(R8)                                                  
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
         BAS   RE,WRTWA                                                         
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* HOST TRANSFER COMPLETE MESSAGE                                 *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDXOK   DS    0H                                                               
         MVI   SVACTN,MSGREADY     SET ACTION CODE                              
         L     R8,TBUFF                                                         
*                                                                               
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(XFRCOMPL,R8),XFRCOMP                                           
         LA    R8,XFRCOMPL(R8)                                                  
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
         BAS   RE,WRTWA                                                         
*&&TT                                                                           
         MVC   INDOUT(20),=C'HOST TRANSFER COMPLETE'                            
*&&                                                                             
         B     EXIT                                                             
*----------------------------------------------------------------*              
* HOST NOP 'READY' MESSAGE                                       *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
*                                                                               
INDMOK   DS    0H                                                               
         OI    SVFLAGS,SVFDONE         SET TRANSFER COMPLETE                    
         NI    TTYPE2,X'FF'-TTYPE2SF   UNSET STRUCTURED FIELD                   
         XC    INDHEAD,INDHEAD                                                  
         MVC   INDHEAD(9),=C'WE DID IT'                                         
         OI    INDHEADH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* SETUP TO SEND ERROR CONDITION                                  *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDERR   DS    0H                                                               
         MVI   SVMSG,ACTERROR                                                   
*                                                                               
         SR    RE,RB               OFFSET TO INSTRUCTION                        
         ST    RE,SVDIE                                                         
*&&TT                                                                           
         MVC   INDOUT(20),=CL20'TOO BAD. WE DIED AT'                            
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,SVDIE,INDOUT+22,4,=C'TOG'                              
         B     EXIT                                                             
*&&                                                                             
         B     INDCLOSE            CLOSE THE TRANSFER                           
         EJECT                                                                  
*----------------------------------------------------------------*              
* INSERT & INSERT DATA ON ERROR                                  *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
INDXERR  DS    0H                                                               
         MVI   SVACTN,MSGERROR     SET ACTION CODE                              
         L     R8,TBUFF                                                         
*                                                                               
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(TRANELN,R8),TRANERR                                            
         LA    R8,TRANELN(R8)                                                   
*                                                                               
         L     RE,TBUFF                                                         
         SR    R8,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R8,6(RE)              SET MESSAGE LENGTH                         
         BAS   RE,WRTWA                                                         
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* ERROR HANDLING DONE                                                           
*************************************************************                   
         SPACE 1                                                                
INDMERR  DS    0H                                                               
         NI    TTYPE2,X'FF'-TTYPE2SF   UNSET STRUCTURED FIELD                   
         XC    INDHEAD,INDHEAD                                                  
         OI    INDHEADH+6,X'80'                                                 
         ZIC   RE,SVERRCD                                                       
         MH    RE,=Y(L'ERRTABLE)                                                
         LA    RE,ERRTABLE(RE)                                                  
         MVC   INDHEAD,0(RE)                                                    
         OI    SVFLAGS,SVFDONE         CLEAR SAVE AREA                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* INDGET   - GET A RECORD FROM THE WORKER FILE              *                   
*                                                                               
*  R8 -> BEGINGING OF DATA AREA FOR INSERT DATA             *                   
*************************************************************                   
         SPACE                                                                  
INDGET   NTR1                                                                   
         LA    R5,MAXSEND          MAX BYTES TO SEND                            
         SR    R2,R2               TOTAL BYTES SO FAR                           
         LA    R8,2(R8)            SKIP LENGTH BYTES                            
*                                                                               
         XC    WRKREC(256),WRKREC                                               
         MVC   WRKREC+0(4),SVRECNUM                                             
         MVC   WRKREC+4(4),=C'RECD'                                             
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'01',RANDOM),SAVEWKID,KEY,WRKREC,CIREC,0             
         CLI   8(R1),0                                                          
         BE    INDGB10                                                          
*                                                                               
         TM    8(R1),X'80'         TEST EOF                                     
         BO    INDGBEOF                                                         
         MVI   SVERRCD,DSKERRQ                                                  
         BAS   RE,INDERR           DIE ON DISK ERROR                            
*                                                                               
INDGB10  DS    0H                                                               
*                                                                               
         LH    RF,WRKREC           FIRST 4 BYTES ARE LENGTH                     
         SH    RF,=H'4'            SET 'FROM' LENGTH                            
*                                                                               
         LA    R0,0(RF,R2)         R2 HAS BYTES SO FAR                          
         CR    R0,R5               TEST TO MAX LENGTH                           
         BH    INDGBX              WON'T FIT - DO IT NEXT TIME                  
*                                                                               
         LA    RE,WRKREC+4         SET 'FROM' ADDR                              
         LR    R0,R8               SET 'TO' ADDR                                
         LR    R1,RF               SET 'TO' LEN                                 
         MVCL  R0,RE                                                            
*                                                                               
         LH    R0,WRKREC           RESTORE LENGTH                               
         SH    R0,=H'4'                                                         
         AR    R8,R0               NEXT OUTPUT ADDRESS                          
         AR    R2,R0               LENGTH SO FAR                                
*                                                                               
         L     RF,SVRECNUM         BUMP RECORD NUMBER                           
         AH    RF,=H'1'                                                         
         ST    RF,SVRECNUM                                                      
*                                                                               
         L     RF,VCOMFACS         NEXT LINE                                    
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'01',READ),SAVEWKID,KEY,WRKREC,CIREC,0               
         CLI   8(R1),0                                                          
         BE    INDGB10             PROCESS NEXT RECORD                          
*                                                                               
         TM    8(R1),X'80'         TEST EOF                                     
         BO    INDGBEOF                                                         
         MVI   SVERRCD,DSKERRQ                                                  
         BAS   RE,INDERR           DIE ON DISK ERROR                            
*                                                                               
INDGBEOF MVI   SVACTN,ACTCLOSE                                                  
*                                                                               
INDGBX   DS    0H                                                               
         STH   R2,DATALEN          STORE THE LENGTH                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ TWAB                                                           *         
***********************************************************************         
         SPACE 1                                                                
RDTWA    NTR1                                                                   
         LA    R2,SRPAGENO         READ IN S/R SAVE TWA                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),(R9)               
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE TWAB                                                          *         
***********************************************************************         
         SPACE 1                                                                
WRTWA    NTR1                                                                   
         LA    R2,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R9)                
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC   H'0'                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
WRKFIL   DC    CL8'WRKFIL'                                                      
*                                                                               
SPACES   DC    CL256' '                                                         
         EJECT                                                                  
***********************************************************************         
VALOCHRS DC    XL16'404E40404E40404E40404E40404E4040'  00-0F **TEMP**           
         DC    XL16'4E40404E40404E40404E40406040407A'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040404E'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404E4E404040'  A0-AF                    
         DC    XL16'40404040404040404040404E4E404060'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404E4E404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404E4E404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F97A4040404040'  F0-FF                    
***********************************************************************         
ASCIICHR DC    XL16'20202020202020202020202020202020'  00-0F **TEMP**           
         DC    XL16'20202020202020202020202020202020'  10-1F **TEMP**           
         DC    XL16'20202020202020202020202020202020'  20-2F                    
         DC    XL16'20202020202020202020202020202020'  30-3F                    
         DC    XL16'20202020202020202020202020202020'  40-4F                    
         DC    XL16'20202020202020202020202020202020'  50-5F                    
         DC    XL16'20202020202020202020202020202020'  60-6F                    
         DC    XL16'20202020202020202020202020202020'  70-7F                    
         DC    XL16'20616263646566676869202020202020'  80-8F                    
         DC    XL16'206A6B6C6D6E6F707172202020202020'  90-9F                    
         DC    XL16'2020737475767778797A202020202020'  A0-AF                    
         DC    XL16'20202020202020202020202020202020'  B0-BF                    
         DC    XL16'20414243444546474849202020202020'  C0-CF                    
         DC    XL16'204A4B4C4D4E4F505152202020202020'  D0-D1                    
         DC    XL16'20205354555657585960202020202020'  E0-EF                    
         DC    XL16'30313233343536373839202020202020'  F0-FF                    
***********************************************************************         
PQCHARS  DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F **TEMP**           
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F **TEMP**           
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  40-4F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  50-5F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  70-7F                    
         DC    XL16'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  0-8F                     
         DC    XL16'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  0-9F                     
         DC    XL16'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  0-AF                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-D1                    
         DC    XL16'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* IND$FILE MESSAGES                                                   *         
***********************************************************************         
         SPACE 1                                                                
WSF      DC    X'F3'               WSF                                          
         DC    X'0006'             SF LEN                                       
         DC    X'40'               OUTBOUND 3270 DS ID CODE                     
         DC    X'00'               PID                                          
         DC    X'F1C2'             WRITE                                        
WSFLN    EQU   *-WSF                                                            
*                                                                               
RDPART   DC    X'0005'                                                          
         DC    X'01FF02'           READ PARTITION 1 QUERY                       
RDPARTLN EQU   *-RDPART                                                         
*                                                                               
HOSTOP   DC    AL2(HOSTOPLN)                                                    
         DC    X'D00012010601010403'   BYTES 2-10                               
         DC    X'0A0A00000000110101'   BYTES 11-19                              
         DC    X'0050055203F003'       BYTES 20-26                              
         DC    X'0946543A44415441'                                              
HOSTOPLN EQU   *-HOSTOP                                                         
*                                                                               
HOSTOP2  DC    AL2(HOSTOP2L)                                                    
         DC    X'D00012010601010403'   BYTES 2-10                               
         DC    X'0A0A00000000110101'   BYTES 11-19                              
         DC    X'0050055203F003'       BYTES 20-26                              
         DC    X'0946543A4D534720'                                              
HOSTOP2L EQU   *-HOSTOP2                                                        
*                                                                               
XFRCOMP  DS    0H                                                               
         DC    X'000AD047110105008000005FD04704C080'                            
         DC    X'61005A5452414E53303320202046696C65207472'                      
         DC    X'616E7366657220636F6D706C65746524'                              
         DC    52X'20'                                                          
XFRCOMPL EQU   *-XFRCOMP                                                        
*                                                                               
INSERT   DC    AL2(10)                                                          
         DC    X'D047110105008000'   BYTES 2-9                                  
*                                                                               
         DC    AL2(0)                BYTES 10-11                                
         DC    X'D04704C08061'       BYTES 12-17                                
INSERTLN EQU   *-INSERT                                                         
*                                                                               
CLOSE    DC    AL2(CLOSELN)                                                     
         DC    X'D04112'                                                        
CLOSELN  EQU   *-CLOSE                                                          
         SPACE 1                                                                
TRANERR  DS    0C                                                               
         DC    X'00',X'0AD04711',X'01050080',X'00005FD0',X'4704C080'            
         DC    X'61005A54'                                                      
         DC    X'52414E53',X'31372020',X'204D6973',X'73696E67'                  
         DC    X'206F7220',X'696E636F',X'72726563'                              
         DC    X'74205453',X'4F206461',X'74612073',X'6574206E'                  
         DC    X'616D653A',X'2066696C',X'65207472'                              
         DC    X'616E7366',X'65722063',X'616E6365',X'6C656424'                  
         DC    X'20202020',X'20202020',X'20202020'                              
TRANELN  EQU   *-TRANERR                                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ERROR MESSAGES & EQUATES                                                      
**********************************************************************          
ERRTABLE DS    0CL60                                                            
NOTCONQ  EQU   0                                                                
         DC    CL60'USER NOT CONNECTED'                                         
INVFILQ  EQU   1                                                                
         DC    CL60'INVALID FILE NAME'                                          
DSKERRQ  EQU   2                                                                
         DC    CL60'DISK ERROR'                                                 
OPNERRQ  EQU   3                                                                
         DC    CL60'HOST OPEN ERROR'                                            
FILNTFQ  EQU   4                                                                
         DC    CL60'FILE NOT FOUND'                                             
NOINDEX  EQU   5                                                                
         DC    CL60'CANNOT SEARCH BY NAME'                                      
INVDEMQ  EQU   6                                                                
         DC    CL60'INVALID DEMO FILE'                                          
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                               
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    F                                                                
FULL     DS    F                                                                
AVALCHRS DS    A                                                                
AASCII   DS    A                                                                
APQCHARS DS    A                                                                
ASYSFAC  DS    A                                                                
VCOMFACS DS    A                                                                
SRPARS   DS    8A                                                               
SRPARX   EQU   *                                                                
DMCB     DS    6F                                                               
RECLEN   DS    H                                                                
DATALEN  DS    H                   LENGTH OF DATA LINE                          
BYTE     DS    X                                                                
*                                                                               
         DS    0H                                                               
WORK     DS    CL64                                                             
*                                                                               
KEY      DS    XL40                INDEX ENTRY                                  
WRKREC   DS    XL1024              REPORT RECORD                                
CIREC    DS    CL14336             CONTROL INTERVAL RECORD                      
WORKX    EQU   *                                                                
         EJECT                                                                  
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
T165FFD  DSECT                                                                  
         DS    CL64                                                             
* SRINDFFD                                                                      
       ++INCLUDE SRINDFFD                                                       
       EJECT                                                                    
* DDCOMFACS, FADSECTS, DMWRKFK, DMWRKFS                                         
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMWRKFS                                                        
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                                 
***********************************************************************         
         SPACE 1                                                                
SRSD     DSECT                                                                  
         ORG   SR$INDF             DEFINE SAVE AREA FOR IND$FILE                
SV$INDF  DS    CL5                                                              
SVACTN   DS    X                                                                
SVMSG    DS    X                   SAVE READY MESSAGE #                         
SVERRCD  DS    X                   SAVE ERROR MESSAGE NUMBER                    
SVRECNUM DS    F                   NEXT RECORD NUMBER TO BE PROCESSED           
*                                                                               
SVUKEY   DS    0CL10               WRKF FILE KEY                                
SVUSER   DS    XL2                 USER ID                                      
SVSUBID  DS    CL3                                                              
SVTYPE   DS    CL1                                                              
SVDAY    DS    XL1                                                              
SVCLASS  DS    CL1                                                              
SVFILNUM DS    XL2                 SEQUENCE #                                   
*                                                                               
SVFLAGS  DS    X                                                                
SVFNWKOK EQU   X'80'                - NWK SECURITY CLEARED                      
SVFDONE  EQU   X'40'                - TRANSFER COMLPLETE                        
SVFOPEN  EQU   X'20'                - FILE OPEN                                 
*                                                                               
SVDIE    DS    F                                                                
SVARGS   DS    CL80                SAVE CARD FOR FILENAME                       
SAVEBDSP DS    F                   DISPLACEMENT TO BUFFER                       
SAVEWKID DS    CL8                 WORKER FILE NAME                             
TWASAVA  DS    XL30                                                             
TWASAVB  DS    XL100                                                            
SVINDLQ  EQU   *-SV$INDF                                                        
******************************************************************              
       ++INCLUDE SRNWKTWAB                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094SRIND00   05/01/02'                                      
         END                                                                    
