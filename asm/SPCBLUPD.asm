*          DATA SET SPCBLUPD   AT LEVEL 003 AS OF 06/18/15                      
*PHASE SPCBUPDA,*                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
SPCBLUPD TITLE 'SPCBLUPD - SYSTEMS+NETWORKS+COSUPD -> CABLFILE'                 
SPCBLUPD CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SPCBLUPD,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(SPCBLUPD,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         OPEN  (SYSTFILE,INPUT)    OPEN SYSTEM FILE FOR INPUT                   
         OPEN  (NETWFILE,INPUT)    OPEN NETWORK FILE FOR INPUT                  
         OPEN  (UPDTFILE,INPUT)    OPEN NETWORK FILE FOR INPUT                  
         OPEN  (CABLFILE,OUTPUT)   OPEN OUR BIG CABLE FILE                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD     INITIALIZE SORTER           
*                                                                               
SYSRDLIN GET   SYSTFILE,IPUTLINE   READ A LINE FROM THE SYSTEM FILE             
         GOTO1 CHNGLINE,DMCB,(C'S',0)      CHANGE THE SYSTEM LINE               
         B     SYSRDLIN            READ THE NEXT LINE                           
*                                                                               
NETRDLIN GET   NETWFILE,IPUTLINE   READ A LINE FROM THE NETWORK FILE            
         GOTO1 CHNGLINE,DMCB,(C'N',0)      CHANGE THE NETWORK LINE              
         B     NETRDLIN            READ THE NEXT LINE                           
*                                                                               
UPDRDLIN GET   UPDTFILE,IPUTLINE   READ A LINE FROM THE NETWORK FILE            
         GOTO1 CHNGLINE,DMCB,(C'U',0)      CHANGE THE UPDATE LINE               
         B     UPDRDLIN            READ THE NEXT LINE                           
*                                                                               
***********************************************************************         
* NOMORE                                                                        
***************                                                                 
*     WHAT HAPPENS WHEN THERE IS NO MORE LINES LEFT IN THE FILE                 
***********************************************************************         
*                                                                               
NOMORE   DS    0H                                                               
         CLOSE SYSTFILE            CLOSE THE CABLE INPUT FILES                  
         CLOSE NETWFILE                                                         
         CLOSE UPDTFILE                                                         
*                                                                               
         BAS   RE,WRTBFILE         WRITE THE SORTER RECS TO OUR FILE            
         CLOSE CABLFILE            CLOSE OUR BIG CABLE FILE                     
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'   TERMINATE SORTER                       
*                                                                               
NOMOREX  XBASE                                                                  
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHNGLINE                                                                      
***************                                                                 
*     THIS ROUTINE CHANGES THE LINE FROM THE INPUT FILE BY CONVERTING           
* THE SYSTEM NUMBER INTO A 4 BYTE SYSTEM NUMBER ZERO FILLED AND ALSO            
* BY ADDING A BYTE -- '1' FOR SYSTEM LINES OR '2' FOR NETWORK LINES.            
* THE ADDITIONAL BYTE IS FOR SORTING PURPOSES.                                  
***********************************************************************         
*                                                                               
CHNGLINE NTR1                                                                   
         MVC   LINETYPE,DMCB                                                    
         XCEFL OPUTLINE,4008                                                    
*                                                                               
         LA    R2,IPUTDATA                                                      
         GOTO1 NEXTFLD,DMCB,(0,(R2)),(L'SYSTNUM,SYSTNUM)                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZICM  R2,DMCB+1,3         A(SYSTEM CODE FIELD)                         
*                                                                               
         EDIT  (B2,SYSTNUM),(4,OPUTDATA),FILL=0                                 
*                                                                               
         CLI   LINETYPE,C'U'                                                    
         BNE   *+14                                                             
         MVC   OPUTDATA+4(3),=C',0,'    FOR UPDATE LINES                        
         B     CSLN10                                                           
         CLI   LINETYPE,C'S'                                                    
         BNE   *+14                                                             
         MVC   OPUTDATA+4(3),=C',1,'    FOR SYSTEM LINES                        
         B     CSLN10                                                           
         MVC   OPUTDATA+4(3),=C',2,'    FOR NETWORK LINES                       
*                                                                               
CSLN10   GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'WORK,WORK)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ZICM  R2,DMCB+1,3         A(FIELD AFTER SYSTEM CODE)                   
*                                                                               
         LR    R0,R2                                                            
         LA    R1,IPUTDATA                                                      
         SR    R0,R1                                                            
         ZICM  R1,IPUTLGTH,2                                                    
         SH    R1,=H'4'            QSAM OVERHEAD                                
         SR    R1,R0               WHAT'S LEFT                                  
         ST    R1,FULL                                                          
         LR    RF,R1                                                            
         LA    R0,OPUTDATA+7                                                    
         LR    RE,R2                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,FULL                                                          
         AH    R1,=H'11'           THE 7 FOR  9999,1,  AND 4 FOR QSAM           
         STCM  R1,3,OPUTLGTH                                                    
*                                                                               
         LA    R8,OPUTLINE                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R8)                                     
*                                                                               
CSLNX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* NEXTFLD                                                                       
***************                                                                 
*     THIS ROUTINE POINTS TO THE NEXT FIELD OF THE INPUT LINE.                  
*                                                                               
* ON ENTRY:    PARAM1  BYTE  0     NUMBER OF FIELDS TO SKIP                     
*                      BYTES 1-3   ADDRESS OF CURRENT FIELD                     
*                                                                               
*              PARAM2  BYTE  0     MAX LENGTH OF STORAGE AREA                   
*                      BYTES 1-3   ADDRESS OF WHERE TO STORE FIELD              
*                                                                               
* ON EXIT:     PARAM1  BYTE  0     FIRST BYTE OF NEXT FIELD                     
*                      BYTES 1-3   ADDRESS OF NEXT FIELD                        
***********************************************************************         
*                                                                               
NEXTFLD  NTR1                                                                   
         L     R2,DMCB             R2=A(CURRENT FIELD IN INPTLINE)              
         L     R4,DMCB+4           R4=A(STORAGE AREA)                           
*                                                                               
         ZIC   R1,DMCB+4           CLEAR THE STORAGE AREA                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
*                                                                               
         CLI   DMCB,0              SKIP ANY FIELDS?                             
         BNE   *+12                                                             
         LA    R3,1                NO                                           
         B     NFLD25                                                           
*                                                                               
         ZIC   R3,DMCB             R3=NUMBER OF FIELDS TO SKIP                  
NFLDRNTR NI    BITFLAG1,X'FF'-B1DBLQTE-B1COMMA                                  
*                                                                               
NFLDLOOP CLI   0(R2),C'"'          DO WE HAVE A DOUBLE QUOTE?                   
         BNE   NFLD10                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD25              YES, NEXT FIELD                              
*                                                                               
         TM    BITFLAG1,B1DBLQTE   USING A DOUBLE QUOTE ALREADY?                
         BZ    *+12                                                             
         NI    BITFLAG1,X'FF'-B1DBLQTE                                          
         B     NFLDNXTC                                                         
         OI    BITFLAG1,B1DBLQTE                                                
         B     NFLDNXTC                                                         
*                                                                               
NFLD10   CLI   0(R2),C','          DO WE HAVE A COMMA?                          
         BNE   NFLD20                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD15                                                           
         TM    BITFLAG1,B1DBLQTE   NO, COMMA IN BETWEEN QUOTES?                 
         BNZ   NFLDNXTC                YES                                      
         OI    BITFLAG1,B1COMMA        NO, WE HAVE A COMMA NOW                  
         B     NFLDNXTC            NEXT CHAR SHOULD BE START OF NXT FLD         
*                                                                               
NFLD15   ST    R2,DMCB             RETURN WITH A COMMA IN DMCB SO WE            
         MVI   DMCB,C','               KNOW WE HAVE 2 COMMAS IN A ROW           
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDYES             NO DATA TO STORE                             
*                                                                               
NFLD20   TM    BITFLAG1,B1COMMA                                                 
         BZ    NFLDNXTC                                                         
*                                                                               
NFLD25   ST    R2,DMCB                                                          
         MVC   DMCB(1),0(R2)                                                    
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDSTOR                                                         
*                                                                               
NFLDNXTC LA    R2,1(R2)            R2 = A(NEXT CHARACTER)                       
         LA    R0,IPUTLINE                                                      
         LR    R1,R2                                                            
         SR    R1,R0                                                            
         CLM   R1,3,IPUTLGTH       DID WE GO BEYOND THE DATA?                   
         BL    NFLDLOOP            NO                                           
         B     NFLDNO                                                           
*                                                                               
NFLDSTOR CLI   0(R2),C'"'          DEALING WITH A STRING?                       
         BNE   NFLD40                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         ZIC   R1,DMCB+4           COPY STRING WITHOUT FIRST QUOTE              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
*                                                                               
         L     R1,DMCB+4                                                        
         LA    R1,0(R1)            CLEAR HOB                                    
         LR    R0,R1                                                            
NFLD30   CLI   0(R4),C'"'          FOUND SECOND QUOTE?                          
         BE    NFLD35                                                           
         LA    R4,1(R4)            NO, CHECK NEXT CHARACTER                     
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         CLM   R1,1,DMCB+4                                                      
         BNL   NFLDYES                                                          
         B     NFLD30                                                           
*                                                                               
NFLD35   L     R0,DMCB+4           CLEAR FROM SECOND QUOTE ON                   
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         ZIC   R0,DMCB+4                                                        
         SR    R0,R1                                                            
         BCTR  R0,0                                                             
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     NFLDYES                                                          
         XC    0(0,R4),0(R4)                                                    
*                                                                               
NFLD40   LA    R1,1                AT LEAST ONE DIGIT IN NUMBER                 
         LR    R3,R2                                                            
*                                                                               
         LA    R3,1(R3)                                                         
         CLI   0(R3),C','                                                       
         BE    *+12                                                             
         LA    R1,1(R1)            INCREMENT DIGIT COUNTER                      
         B     *-16                                                             
*                                                                               
         BCTR  R1,0                CONVERT NUMBER TO BINARY                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         CLI   DMCB+4,1            ONE BYTE                                     
         BNE   *+12                                                             
         STC   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,2            HALF WORD                                    
         BNE   *+12                                                             
         STH   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,3            3 BYTES                                      
         BNE   *+12                                                             
         STCM  R1,7,0(R4)                                                       
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,4            FULL WORD                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
NFLDNO   B     NO                  NO MORE DATA                                 
*                                                                               
NFLDYES  B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* WRTBFILE                                                                      
***************                                                                 
*     THIS ROUTINE WRITE THE CHANGED LINE TO OUR BIG CABLE FILE                 
***********************************************************************         
*                                                                               
WRTBFILE NTR1                                                                   
GETMORE  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)                                                      
         BZ    WRTFX                                                            
         PUT   CABLFILE,(R3)                                                    
         B     GETMORE                                                          
*                                                                               
WRTFX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* SORTER CARDS                                                                  
***********************************************************************         
*                                                                               
* SORT STARTING @ COL 5, FOR A LENGTH OF 6, IN ASCENDING BINARY ORDER           
SORTCARD DC    CL80'SORT FIELDS=(5,6,A),FORMAT=BI,WORK=1'                       
* RECORDS ARE VARIABLE LENGTH, MAXIMUM LENGTH OF 4008                           
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4008'                                  
***********************************************************************         
* DATA SETS                                                                     
***********************************************************************         
*                                                                               
SYSTFILE DCB   DDNAME=SYSTFIL,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=6233, +        
               LRECL=3000,EODAD=NETRDLIN                                        
*                                                                               
NETWFILE DCB   DDNAME=NETWFIL,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=6233, +        
               LRECL=255,EODAD=UPDRDLIN                                         
*                                                                               
UPDTFILE DCB   DDNAME=UPDTFIL,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=6233, +        
               LRECL=255,EODAD=NOMORE                                           
*                                                                               
CABLFILE DCB   DDNAME=CABLFIL,MACRF=PM,DSORG=PS,RECFM=VB,BLKSIZE=4096, +        
               LRECL=4004,BUFNO=2                                               
*                                                                               
DMCB     DS    6F                                                               
FULL     DS    F                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
*                                                                               
SYSTNUM  DS    XL2                 SYSTEM NUMBER                                
LINETYPE DS    CL1                 LINE TYPE - (S)YSTEM OR (N)ETWORK            
BITFLAG1 DS    XL1                                                              
B1COMMA  EQU   X'80'                                                            
B1DBLQTE EQU   X'40'                                                            
*                                                                               
IPUTLINE DS    0CL4008             INPUT LINE FROM THE FILE                     
IPUTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
IPUTDATA DS    CL4004                  DATA FOR THIS LINE                       
*                                                                               
OPUTLINE DS    0CL4008             OUTPUT LINE FOR THE FILE                     
OPUTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
OPUTDATA DS    CL4004                  DATA FOR THIS LINE                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPCBLUPD  06/18/15'                                      
         END                                                                    
